use crate::types::Position;
use crate::usi::{EngineMessage, GuiMessage, IdParam};

// Prints an engine message to stdout
fn print_msg(msg: &EngineMessage) {
    let serialised = msg.to_string();
    println!("{serialised}");
}

pub struct Engine {
    position: Position,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            position: Position::startpos(),
        }
    }
    pub fn handle_message(&mut self, msg: GuiMessage) -> bool {
        match msg {
            GuiMessage::Usi => self.handle_usi(),
            GuiMessage::Position(position, moves) => {
                dbg!(&position);
                self.position = position;
            }
            GuiMessage::Quit => return true,
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
