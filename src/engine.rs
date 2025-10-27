//! The main code for the engine. This responds to messages from the gui (parsed in the main
//! function), handles parameters and calculation threads and whatnot.

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use std::{cmp, f32};

use ahash::AHashMap;
use ordered_float::OrderedFloat;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use smallvec::{SmallVec, smallvec};

use crate::model::{Move, Player, Position};
use crate::usi::{EngineMessage, GuiMessage, IdParam};

fn iddfs(
    stop: &AtomicBool,
    level: u32,
    mut alpha: f32,
    beta: f32,
    position: &mut Position,
    cache: &RwLock<AHashMap<Position, (SmallVec<[Move; 8]>, f32)>>,
    old_cache: &AHashMap<Position, (SmallVec<[Move; 8]>, f32)>,
    cache_hits: &AtomicU64,
    nodes: &AtomicU64,
) -> Option<(SmallVec<[Move; 8]>, f32)> {
    if stop.load(Ordering::Relaxed) {
        return None;
    }

    nodes.fetch_add(1, Ordering::Relaxed);

    if level == 0 {
        return Some((smallvec![], position.eval_relative()));
    }

    if let Some(res) = cache.read().unwrap().get(position) {
        cache_hits.fetch_add(1, Ordering::Relaxed);
        return Some(res.clone());
    }

    let mut moves = position.possible_moves();
    moves.sort_by_cached_key(|m| {
        position.make_move_unchecked(*m);
        let key = old_cache
            .get(&position)
            .map(|(_, eval)| -*eval)
            .unwrap_or(f32::NEG_INFINITY);
        position.unmake_move_unchecked(*m);

        // We want to sort best to worst, so highest to lowest (hence the cmp Reverse)
        cmp::Reverse(OrderedFloat(key))
    });

    let mut best: Option<(SmallVec<[Move; 8]>, f32)> = None;

    for m in moves {
        position.make_move_unchecked(m);

        let (mut line, eval) = iddfs(
            stop,
            level - 1,
            -beta,
            -alpha,
            position,
            cache,
            old_cache,
            cache_hits,
            nodes,
        )?;

        if best
            .as_ref()
            .is_none_or(|(_, current_best)| -eval > *current_best)
        {
            line.push(m);
            best = Some((line, -eval));

            if -eval > alpha {
                alpha = -eval;
            }
        }

        position.unmake_move_unchecked(m);

        if -eval >= beta {
            break;
        }
    }

    let res = best.unwrap_or((smallvec![], f32::NEG_INFINITY));
    cache.write().unwrap().insert(position.clone(), res.clone());
    Some(res)
}

fn ponder(stop: Arc<AtomicBool>, mut position: Position, depth: Option<u32>) {
    let now = std::time::Instant::now();
    let mut level = 1;
    let mut old_cache = AHashMap::<Position, (SmallVec<[Move; 8]>, f32)>::new();
    let cache = RwLock::new(AHashMap::new());

    while !stop.load(Ordering::Relaxed) && depth.is_none_or(|d| level < d) {
        let mut moves = position.possible_moves();
        let mut cache_handle = cache.write().unwrap();

        std::mem::swap(&mut old_cache, &mut cache_handle);
        cache_handle.clear();
        drop(cache_handle);

        moves.sort_by_cached_key(|m| {
            position.make_move_unchecked(*m);
            let key = old_cache
                .get(&position)
                .map(|(_, eval)| -*eval)
                .unwrap_or(f32::NEG_INFINITY);
            position.unmake_move_unchecked(*m);

            // We want to sort best to worst, so highest to lowest (hence the cmp Reverse)
            cmp::Reverse(OrderedFloat(key))
        });

        let cache_hits = AtomicU64::new(0);
        let nodes = AtomicU64::new(0);

        let Some(res) = moves
            .into_par_iter()
            .map(|m| {
                let mut position = position.clone();
                position.make_move_unchecked(m);
                let (mut line, eval) = iddfs(
                    &stop,
                    level,
                    f32::NEG_INFINITY,
                    f32::INFINITY,
                    &mut position,
                    &cache,
                    &old_cache,
                    &cache_hits,
                    &nodes,
                )?;
                line.push(m);
                Some((line, -eval))
            })
            .collect::<Option<Vec<(SmallVec<[Move; 8]>, f32)>>>()
        else {
            eprintln!("stopped before reaching max depth");
            return;
        };

        let (line, eval) = res
            .into_iter()
            .max_by(|x, y| x.1.partial_cmp(&y.1).unwrap())
            .unwrap();

        println!(
            "best move: {}, evaluation: {}",
            line.last()
                .map(|m| format!("{m}"))
                .unwrap_or("No moves".into()),
            eval * if position.player_to_move() == Player::Black {
                1.0
            } else {
                -1.0
            }
        );

        print!("line: ");
        for m in line.iter().rev() {
            print!("{m} ");
        }
        println!(
            "\nnodes explored: {}, cache hits: {}",
            nodes.load(Ordering::SeqCst),
            cache_hits.load(Ordering::SeqCst)
        );

        level += 2;
    }

    println!("Ponder took {} seconds", now.elapsed().as_secs_f64());
}

// Prints an engine message to stdout
fn print_msg(msg: &EngineMessage) {
    let serialised = msg.to_string();
    println!("{serialised}");
}

pub struct Engine {
    position: Position,
    stop_thread: Option<Arc<AtomicBool>>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            position: Position::startpos(),
            stop_thread: None,
        }
    }
    pub fn handle_message(&mut self, msg: GuiMessage) -> bool {
        match msg {
            GuiMessage::Usi => self.handle_usi(),
            GuiMessage::Position(position, moves) => {
                self.position = position;

                for mve in moves {
                    // TODO probably check these moves
                    self.position.make_move_unchecked(mve);
                }

                println!("{}", self.position);
            }
            GuiMessage::Quit => return true,
            GuiMessage::Go { depth } => {
                if let Some(stop) = self.stop_thread.take() {
                    stop.store(true, Ordering::Relaxed);
                }

                let stop_thread = Arc::new(AtomicBool::new(false));
                self.stop_thread = Some(Arc::clone(&stop_thread));
                let position = self.position.clone();
                std::thread::spawn(move || ponder(stop_thread, position, depth));
            }
            GuiMessage::Stop => {
                if let Some(stop) = self.stop_thread.take() {
                    stop.store(true, Ordering::Relaxed);
                }
            }
        }

        false
    }

    fn handle_usi(&self) {
        // Identify the engine
        print_msg(&EngineMessage::Id(IdParam::Name("nanoshogi".into())));
        print_msg(&EngineMessage::Id(IdParam::Author("villuna".into())));

        // Print possible options
        // if i get any

        // Print ok
        print_msg(&EngineMessage::UsiOk);
    }
}
