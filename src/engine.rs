use std::sync::mpsc::{self, TryRecvError};
use std::thread::{self, JoinHandle};

use haitaka_usi::{EngineMessage, EngineParams, GuiMessage, IdParams};

use crate::types::Position;

enum ThreadMessage {
    Go(EngineParams, Position),
    Pause,
    Stop,
}

// Prints an engine message to stdout
fn print_msg(msg: &EngineMessage) {
    let serialised = msg.to_string();
    println!("{serialised}");
}

fn evaluate(position: &Position) -> f32 {
    todo!();
}

fn iddfs(depth: u32, params: &EngineParams, position: &mut Position) -> f32 {
    todo!();
}

fn think(rx: &mpsc::Receiver<ThreadMessage>, params: EngineParams, mut position: Position) -> bool {
    let mut depth = 1;

    loop {
        // Check if engine has been stopped
        match rx.try_recv() {
            Ok(msg) => match msg {
                // We are already running? I think stockfish handles this so we might have to
                // as well.
                ThreadMessage::Go(..) => {}
                ThreadMessage::Pause => {
                    return false;
                }
                ThreadMessage::Stop => {
                    return true;
                }
            },
            // If the reciever broke we should probably shut down the thread
            Err(TryRecvError::Disconnected) => return true,
            // If nothing has been sent yet just continue
            Err(TryRecvError::Empty) => {}
        };
        // Do IDDFS
        iddfs(depth, &params, &mut position);
    }
}

fn calculation_thread(rx: mpsc::Receiver<ThreadMessage>) {
    while let Ok(msg) = rx.recv() {
        match msg {
            ThreadMessage::Go(engine_params, position) => {
                let should_stop = think(&rx, engine_params, position);
                if should_stop {
                    return;
                }
            }
            ThreadMessage::Pause => {}
            ThreadMessage::Stop => return,
        }
    }
}

pub struct Engine {
    position: Position,
    thread: JoinHandle<()>,
    tx: mpsc::Sender<ThreadMessage>,
}

impl Engine {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        let thread = thread::spawn(|| calculation_thread(rx));
        let mut position = Position::startpos();

        Self {
            position,
            thread,
            tx,
        }
    }
    pub fn handle_message(&mut self, msg: GuiMessage) -> bool {
        match msg {
            GuiMessage::Usi => self.handle_usi(),
            GuiMessage::Unknown(command) => eprintln!("Error: unknown command \"{command}\""),
            GuiMessage::Position { sfen, moves } => {
                self.position = sfen
                    .map(|sfen| Position::from_sfen(&sfen))
                    .unwrap_or_else(|| Position::startpos());
            }
            GuiMessage::Go(params) => {
                todo!();
            }
            GuiMessage::Quit => return true,
            _ => eprintln!("Error: unsupported command"),
        }

        false
    }

    fn handle_usi(&self) {
        // Identify the engine
        print_msg(&EngineMessage::Id(IdParams::Name("nanoshogi".into())));
        print_msg(&EngineMessage::Id(IdParams::Author("villuna".into())));

        // Print possible options
        // if i get any

        // Print ok
        print_msg(&EngineMessage::UsiOk);
    }
}
