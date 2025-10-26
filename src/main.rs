use nanoshogi::engine::Engine;
use nanoshogi::usi::GuiMessage;

fn main() {
    eprintln!("Welcome to nanoshogi - the exceedingly okay shogi bot");
    let mut engine = Engine::new();

    for line in std::io::stdin().lines() {
        match GuiMessage::parse(line.unwrap()) {
            Ok(msg) => {
                let should_shutdown = engine.handle_message(msg);

                if should_shutdown {
                    break;
                }
            }
            Err(e) => eprintln!("Error: {e}"),
        };
    }
}
