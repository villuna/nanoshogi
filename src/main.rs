use haitaka_usi::GuiMessage;

use crate::engine::Engine;

mod engine;
mod sfen;
mod types;

fn main() {
    eprintln!("Welcome to nanoshogi - the exceedingly okay shogi bot");
    let mut engine = Engine::new();

    for line in std::io::stdin().lines() {
        let msg = GuiMessage::parse_no_nl(&line.unwrap()).unwrap();
        let should_shutdown = engine.handle_message(msg);

        if should_shutdown {
            break;
        }
    }
}
