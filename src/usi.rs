use std::{iter::Peekable, str::SplitWhitespace};
use thiserror::Error;

use crate::{
    model::{Move, Position},
    sfen::{SFEN_STARTPOS, parse_moves, parse_sfen},
};

/// A message passed from the GUI to the engine.
pub enum GuiMessage {
    /// Tells the engine to use the universal shogi interface
    Usi,
    /// Tells the engine to start thinking from the given position, optionally after a given set
    /// of moves.
    /// If no moves are given, the vector will be empty.
    Position(Position, Vec<Move>),
    /// Tells the engine to stop as soon as possible.
    Quit,
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

/// Arguments that could be used with the `id` engine message
pub enum IdParam {
    /// The engine's name
    Name(String),
    /// The engine author's name
    Author(String),
}

impl IdParam {
    fn to_string(&self) -> String {
        match self {
            IdParam::Name(name) => format!("name {name}"),
            IdParam::Author(author) => format!("author {author}"),
        }
    }
}

/// A message passed from the GUI to the engine.
pub enum EngineMessage {
    Id(IdParam),
    UsiOk,
}

impl EngineMessage {
    /// Serialises the message into a string in USI format.
    pub fn to_string(&self) -> String {
        match self {
            EngineMessage::Id(id_param) => format!("id {}", id_param.to_string()),
            EngineMessage::UsiOk => "usiok".to_string(),
        }
    }
}

const GUI_COMMANDS: &[&str] = &["usi", "position", "quit"];

fn get_params<'i>(
    input: &mut Peekable<impl Iterator<Item = &'i str>>,
    ids: &[&str],
) -> Option<Vec<(&'i str, String)>> {
    let mut res = vec![];

    while let Some(token) = input.next() {
        if ids.contains(&token) {
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
                if !val.is_empty() || !position.is_none() {
                    return Err(MessageParseError::InvalidParameters);
                }
                position = Some(parse_sfen(SFEN_STARTPOS).unwrap());
            }
            "sfen" => match parse_sfen(&val) {
                Ok(pos) => {
                    if !position.is_none() {
                        return Err(MessageParseError::InvalidParameters);
                    }
                    position = Some(pos);
                }
                Err(_) => {
                    return Err(MessageParseError::InvalidArgument {
                        param: "sfen".into(),
                    });
                }
            },
            "moves" => match parse_moves(&val) {
                Ok(m) => {
                    if !moves.is_none() {
                        return Err(MessageParseError::InvalidParameters);
                    }
                    moves = Some(m)
                }
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
        _ => unreachable!(),
    }
}

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
