//! The main code for the engine. This responds to messages from the gui (parsed in the main
//! function), handles parameters and calculation threads and whatnot.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use smallvec::{SmallVec, smallvec};

use crate::model::{Move, Position};
use crate::usi::{EngineMessage, GuiMessage, IdParam};

fn iddfs(
    stop: &AtomicBool,
    level: u32,
    position: &mut Position,
) -> Option<(SmallVec<[Move; 8]>, f32)> {
    if stop.load(Ordering::Relaxed) {
        return None;
    }
    if level == 0 {
        return Some((smallvec![], position.eval_relative()));
    }

    let moves = position.possible_moves();
    let mut best: Option<(SmallVec<[Move; 8]>, f32)> = None;

    for m in moves {
        position.make_move_unchecked(m);

        let (mut line, eval) = iddfs(stop, level - 1, position)?;

        if best
            .as_ref()
            .is_none_or(|(_, current_best)| -eval > *current_best)
        {
            line.push(m);
            best = Some((line, -eval));
        }

        position.unmake_move_unchecked(m);
    }

    Some(best.unwrap_or((smallvec![], f32::NEG_INFINITY)))
}

fn ponder(stop: Arc<AtomicBool>, mut position: Position, depth: Option<u32>) {
    let now = std::time::Instant::now();
    let mut level = 1;
    while !stop.load(Ordering::Relaxed) && depth.is_none_or(|d| level < d) {
        let moves = position.possible_moves();

        let Some(res) = moves
            .into_par_iter()
            .map(|m| {
                let mut position = position.clone();
                position.make_move_unchecked(m);
                let (mut line, eval) = iddfs(&stop, level, &mut position)?;
                line.push(m);
                Some((line, -eval))
            })
            .collect::<Option<Vec<(SmallVec<[Move; 8]>, f32)>>>()
        else {
            eprintln!("Stopping");
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
            eval
        );

        print!("line: ");
        for m in line.iter().rev() {
            print!("{m} ");
        }
        println!("");

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
