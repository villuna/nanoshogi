use haitaka_types;
use std::sync::mpsc;
use std::thread::{self, JoinHandle};

use haitaka_usi::{EngineMessage, EngineParams, GuiMessage, IdParams};
use shogi::Position;

const STARTPOS: &str = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";

// Maybe I will write my own parser
fn convert_move(m: haitaka_types::Move) -> shogi::Move {
    match m {
        haitaka_types::Move::Drop { piece, to } => shogi::Move::Drop {
            piece_type: match piece {
                haitaka_types::Piece::Pawn => shogi::PieceType::Pawn,
                haitaka_types::Piece::Lance => shogi::PieceType::Lance,
                haitaka_types::Piece::Knight => shogi::PieceType::Knight,
                haitaka_types::Piece::Silver => shogi::PieceType::Silver,
                haitaka_types::Piece::Bishop => shogi::PieceType::Bishop,
                haitaka_types::Piece::Rook => shogi::PieceType::Rook,
                haitaka_types::Piece::Gold => shogi::PieceType::Gold,
                haitaka_types::Piece::King => shogi::PieceType::King,
                haitaka_types::Piece::Tokin => shogi::PieceType::ProPawn,
                haitaka_types::Piece::PLance => shogi::PieceType::ProLance,
                haitaka_types::Piece::PKnight => shogi::PieceType::ProKnight,
                haitaka_types::Piece::PSilver => shogi::PieceType::ProSilver,
                haitaka_types::Piece::PBishop => shogi::PieceType::ProBishop,
                haitaka_types::Piece::PRook => shogi::PieceType::ProRook,
            },
            to: shogi::Square::new(to.rank().to_index() as u8, to.file().to_index() as u8).unwrap(),
        },
        haitaka_types::Move::BoardMove {
            from,
            to,
            promotion,
        } => {
            let from =
                shogi::Square::new(from.rank().to_index() as u8, from.file().to_index() as u8)
                    .unwrap();
            let to =
                shogi::Square::new(to.rank().to_index() as u8, to.file().to_index() as u8).unwrap();

            shogi::Move::Normal {
                from,
                to,
                promote: promotion,
            }
        }
    }
}

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

fn calculation_thread(rx: mpsc::Receiver<ThreadMessage>) {}

pub struct Engine {
    position: Position,
    thread: JoinHandle<()>,
    tx: mpsc::Sender<ThreadMessage>,
}

impl Engine {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        let thread = thread::spawn(|| calculation_thread(rx));
        let mut position = Position::new();
        position.set_sfen(STARTPOS).unwrap();

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
                let sfen = sfen.as_deref().unwrap_or(STARTPOS);
                if let Err(e) = self.position.set_sfen(&sfen) {
                    eprintln!("Error: couldn't set sfen position ({e})")
                }

                for m in moves.unwrap_or_default() {
                    if let Err(e) = self.position.make_move(convert_move(m)) {
                        eprintln!("Error: couldn't make all moves in the position ({e})");
                        break;
                    };
                }
            }
            GuiMessage::Go(params) => {}
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
