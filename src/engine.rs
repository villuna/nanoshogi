//! The main code for the engine. This responds to messages from the gui (parsed in the main
//! function), handles parameters and calculation threads and whatnot.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use smallvec::{SmallVec, smallvec};

use crate::model::{Move, Position};
use crate::usi::{EngineMessage, GuiMessage, IdParam};

fn iddfs(level: u32, position: &mut Position) -> (SmallVec<[Move; 8]>, f32) {
    if level == 0 {
        return (smallvec![], position.eval_relative());
    }

    let moves = position.possible_moves();
    let mut best: Option<(SmallVec<[Move; 8]>, f32)> = None;

    for m in moves {
        position.make_move_unchecked(m);

        let (mut line, eval) = iddfs(level - 1, position);

        if best
            .as_ref()
            .is_none_or(|(_, current_best)| -eval > *current_best)
        {
            line.push(m);
            best = Some((line, -eval));
        }

        position.unmake_move_unchecked(m);
    }

    best.unwrap_or((smallvec![], f32::NEG_INFINITY))
}

fn ponder(stop: Arc<AtomicBool>, mut position: Position) {
    let mut level = 1;
    while !stop.load(Ordering::Relaxed) {
        let (line, eval) = iddfs(level, &mut position);
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

        level += 1;
    }
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
            GuiMessage::Go => {
                if self.stop_thread.is_none() {
                    let stop_thread = Arc::new(AtomicBool::new(false));
                    self.stop_thread = Some(Arc::clone(&stop_thread));
                    let position = self.position.clone();
                    let _thread = std::thread::spawn(|| ponder(stop_thread, position));
                }
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
