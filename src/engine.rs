//! The main code for the engine. This responds to messages from the gui (parsed in the main
//! function), handles parameters and calculation threads and whatnot.

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use std::time::Instant;
use std::{cmp, f32, thread};

use ahash::AHashMap;
use ordered_float::OrderedFloat;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use smallvec::{SmallVec, smallvec};

use crate::model::{Move, Position};
use crate::usi::{GoParams, GuiMessage};

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

fn ponder(params: GoParams, stop: Arc<AtomicBool>, mut position: Position) {
    // If the move has a time limit spawn a thread that automatically stops this thread when
    // time is up.
    if let Some(time) = params.move_time {
        let stop = Arc::clone(&stop);
        thread::spawn(move || {
            thread::sleep(time);
            stop.store(true, Ordering::Relaxed);
        });
    }

    let start_time = Instant::now();
    let mut level = 1;
    let mut old_cache = AHashMap::<Position, (SmallVec<[Move; 8]>, f32)>::new();
    let cache = RwLock::new(AHashMap::new());

    let mut alpha = f32::NEG_INFINITY;
    let mut beta = f32::INFINITY;
    let mut last_eval = None;
    let mut best_line = None;

    while !stop.load(Ordering::Relaxed) && params.depth.is_none_or(|d| level < d) {
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

        if let Some(eval) = last_eval {
            // Update aspiration windows based on what we would expect the eval to be
            // TODO widen these windows and research if the real eval is outside this window
            alpha = eval - 0.25;
            beta = eval + 0.25;
        }

        let Some(res) = moves
            .into_par_iter()
            .map(|m| {
                let mut position = position.clone();
                position.make_move_unchecked(m);
                let (mut line, eval) = iddfs(
                    &stop,
                    level,
                    alpha,
                    beta,
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
            break;
        };

        let (line, eval) = res
            .into_iter()
            .max_by(|x, y| x.1.partial_cmp(&y.1).unwrap())
            .unwrap();

        best_line = Some(line);
        last_eval = Some(eval);

        // Print information about the current state of the search
        print!(
            "info depth {} time {} score cp {} nodes {} pv",
            level + 1,
            start_time.elapsed().as_millis(),
            (eval * 100.0).round() as u32,
            nodes.load(Ordering::Relaxed),
        );

        for m in best_line.as_ref().unwrap().iter().rev() {
            print!(" {m}");
        }
        println!("");

        level += 2;
    }

    // Print the best move after finishing search
    if let Some(line) = best_line {
        print!("bestmove {}", line.last().unwrap());
        if line.len() > 1 {
            print!(" ponder {}", line[line.len() - 2]);
        }
        println!("");
    }
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
            }
            GuiMessage::Quit => return true,
            GuiMessage::Go(params) => {
                if let Some(stop) = self.stop_thread.take() {
                    stop.store(true, Ordering::Relaxed);
                }

                let stop_thread = Arc::new(AtomicBool::new(false));
                self.stop_thread = Some(Arc::clone(&stop_thread));
                let position = self.position.clone();
                thread::spawn(move || ponder(params, stop_thread, position));
            }
            GuiMessage::Stop => {
                if let Some(stop) = self.stop_thread.take() {
                    stop.store(true, Ordering::Relaxed);
                }
            }
            GuiMessage::PrintPosition => {
                println!("{}", self.position);
            }
        }

        false
    }

    fn handle_usi(&self) {
        // Identify the engine
        println!("id name nanoshogi");
        println!("id author villuna");

        // Print possible options
        // if i get any

        // Print ok
        println!("usiok");
    }
}
