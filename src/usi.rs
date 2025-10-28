//! Types for representing  USI (Universal Shogi Interface) commands, as well as functions to
//! parse them from string. These commands are either sent from the GUI to the engine
//! ([GuiCommand]), or from the engine to the GUI ([EngineCommand]).

// Unlike the sfen parser I've implemented this parser manually - this is because the USI is very
// ambiguous and designed to be parsed manually, token by token, rather than with any kind of
// sophisticated parsing technique.

use ahash::AHashSet;
use std::{iter::Peekable, str::SplitWhitespace, time::Duration};
use thiserror::Error;

use crate::{
    model::{Move, Position},
    sfen::{SFEN_STARTPOS, parse_moves, parse_sfen},
};

/// A message passed from the GUI to the engine.
#[derive(Clone, Debug)]
pub enum GuiMessage {
    /// Tells the engine to use the universal shogi interface
    Usi,
    /// Tells the engine to start thinking from the given position, optionally after a given set
    /// of moves.
    /// If no moves are given, the vector will be empty.
    Position(Position, Vec<Move>),
    /// Tells the engine to terminate as soon as possible.
    Quit,
    /// Tells the engine to start searching for the best move from this position.
    Go(GoParams),
    /// Tells the engine to stop searching and print the best move it found.
    Stop,
    /// Tells the engine to print the current position to the terminal. This is a custom command not
    /// in the usi spec that I use for debugging.
    PrintPosition,
}

/// Parameters that can be passed into the 'go' gui command.
#[derive(Clone, Debug, Default)]
pub struct GoParams {
    /// The moves to start searching from at the root of the tree.
    pub search_moves: Option<Vec<Move>>,
    /// The time left on black's clock.
    pub black_time: Option<Duration>,
    /// The time left on white's clock.
    pub white_time: Option<Duration>,
    /// Black's time increment (how much extra time they get per move).
    pub black_increment: Option<Duration>,
    /// White's time increment (how much extra time they get per move).
    pub white_increment: Option<Duration>,
    /// The number of moves deep the engine should search before stopping.
    pub depth: Option<u32>,
    /// The amount of time the engine has to think about its move.
    pub move_time: Option<Duration>,
}

/// Errors that could be encountered when parsing a USI message.
#[derive(Error, Debug, Clone)]
pub enum MessageParseError {
    #[error("expected message")]
    Empty,
    #[error("unknown command: {0:?}")]
    UnknownCommand(String),
    #[error("invalid parameters for command")]
    InvalidParameters,
    #[error("invalid argument for parameter {param:?}")]
    InvalidArgument { param: String },
}

impl GuiMessage {
    /// Parses a line containing a single gui message. Whitespace will be stripped from the
    /// beginning and end.
    pub fn parse(input: impl AsRef<str>) -> Result<Self, MessageParseError> {
        parse_gui_message(input.as_ref())
    }
}

fn get_params<'i>(
    input: &mut Peekable<impl Iterator<Item = &'i str>>,
    ids: &[&str],
) -> Option<Vec<(&'i str, String)>> {
    let mut res = vec![];
    // store which params we've parsed already to disallow duplicates
    let mut params_parsed = AHashSet::new();

    while let Some(token) = input.next() {
        if ids.contains(&token) {
            if params_parsed.contains(token) {
                return None;
            }
            params_parsed.insert(token);
            let mut arg = String::new();

            loop {
                let next = input.peek();
                if next.is_none() || next.is_some_and(|t| ids.contains(t)) {
                    break;
                }

                // built the argument token by token, putting spaces in between
                if !arg.is_empty() {
                    arg.push(' ');
                }
                arg.push_str(input.next().unwrap());
            }

            res.push((token, arg));
        } else {
            // This will only happen if the first token is not the name of a valid parameter
            return None;
        }
    }

    Some(res)
}

type TokenStream<'i> = Peekable<SplitWhitespace<'i>>;

fn parse_position_command<'i>(
    input: &mut TokenStream<'i>,
) -> Result<GuiMessage, MessageParseError> {
    let Some(params) = get_params(input, &["startpos", "sfen", "moves"]) else {
        return Err(MessageParseError::InvalidParameters);
    };

    let mut position = None;
    let mut moves = None;
    for (param, val) in params {
        match param {
            "startpos" => {
                position = Some(parse_sfen(SFEN_STARTPOS).unwrap());
            }
            "sfen" => match parse_sfen(&val) {
                Ok(pos) => position = Some(pos),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: "sfen".into(),
                    });
                }
            },
            "moves" => match parse_moves(&val) {
                Ok(m) => moves = Some(m),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: "moves".into(),
                    });
                }
            },
            _ => unreachable!(),
        }
    }

    Ok(GuiMessage::Position(
        position.unwrap(),
        moves.unwrap_or_default(),
    ))
}

fn parse_go_command<'i>(input: &mut TokenStream) -> Result<GuiMessage, MessageParseError> {
    let mut go_params = GoParams::default();
    let Some(params) = get_params(
        input,
        &[
            "searchmoves",
            "ponder",
            "btime",
            "wtime",
            "binc",
            "winc",
            "movestogo",
            "depth",
            "nodes",
            "movetime",
            "mate",
            "infinite",
        ],
    ) else {
        return Err(MessageParseError::InvalidParameters);
    };

    for (param, val) in params {
        match param {
            "searchmoves" => match parse_moves(&val) {
                Ok(moves) => go_params.search_moves = Some(moves),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "btime" => match val.parse::<u64>() {
                Ok(millis) => go_params.black_time = Some(Duration::from_millis(millis)),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "wtime" => match val.parse::<u64>() {
                Ok(millis) => go_params.white_time = Some(Duration::from_millis(millis)),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "binc" => match val.parse::<u64>() {
                Ok(millis) => go_params.black_increment = Some(Duration::from_millis(millis)),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "winc" => match val.parse::<u64>() {
                Ok(millis) => go_params.white_increment = Some(Duration::from_millis(millis)),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "depth" => match val.parse::<u32>() {
                Ok(depth) => go_params.depth = Some(depth),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "movetime" => match val.parse::<u64>() {
                Ok(millis) => go_params.move_time = Some(Duration::from_millis(millis)),
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: param.into(),
                    });
                }
            },
            "infinite" => {
                go_params.move_time = None;
            }
            "nodes" | "ponder" | "movestogo" | "byoyomi" | "mate" => {
                eprintln!("warning: {param} parameter not supported")
            }
            _ => unreachable!(),
        }
    }

    Ok(GuiMessage::Go(go_params))
}

// Checks if the command is empty (i.e. there are no more tokens in the stream), and if so returns
// the given result message. Otherwise returns an invalid parameters error.
fn parse_empty_command<'i>(
    input: &mut TokenStream<'i>,
    res: GuiMessage,
) -> Result<GuiMessage, MessageParseError> {
    // No arguments are allowed for this function
    if let Some(_) = input.next() {
        Err(MessageParseError::InvalidParameters)
    } else {
        Ok(res)
    }
}

fn parse_gui_message_inner<'i>(
    command: &'i str,
    input: &mut TokenStream<'i>,
) -> Result<GuiMessage, MessageParseError> {
    match command {
        "usi" => parse_empty_command(input, GuiMessage::Usi),
        "position" => parse_position_command(input),
        "quit" => parse_empty_command(input, GuiMessage::Quit),
        "go" => parse_go_command(input),
        "stop" => parse_empty_command(input, GuiMessage::Stop),
        "print" | "printposition" => parse_empty_command(input, GuiMessage::PrintPosition),
        _ => unreachable!(),
    }
}

const GUI_COMMANDS: &[&str] = &[
    "usi",
    "position",
    "quit",
    "go",
    "stop",
    "print",
    "printposition",
];

fn parse_gui_message(input: &str) -> Result<GuiMessage, MessageParseError> {
    let input = input.trim();
    if input.is_empty() {
        return Err(MessageParseError::Empty);
    }

    let mut tokens = input.split_whitespace().peekable();
    let mut command = None;

    while let Some(token) = tokens.next() {
        if GUI_COMMANDS.contains(&token) {
            command = Some(token);
            break;
        }
    }

    match command {
        None => Err(MessageParseError::UnknownCommand(input.to_owned())),
        Some(command) => parse_gui_message_inner(command, &mut tokens),
    }
}
