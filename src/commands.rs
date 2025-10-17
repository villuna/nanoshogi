/// A USI command sent from the GIU to the engine.
#[derive(Debug, PartialEq, Eq)]
pub enum EngineCommand<'a> {
    Usi,
    Debug(bool),
    SetOption {
        name: &'a str,
        value: Option<&'a str>,
    },
}

/// Errors that could be encountered while parsing a USI GUI to engine command
#[derive(Debug, PartialEq, Eq)]
pub enum EngineParseError<'a> {
    Empty,
    InvalidCommand(&'a str),
    ExpectedOption(&'a str),
    InvalidArgument(&'a str),
    ArgumentEmpty(&'a str),
}

/// Given a string slice `str` and a subslice `substr` of that slice, returns two subslices - the
/// first contains everything in `str` up until `substr`, the second contains everything after that.
///
/// SAFETY: This function is unsafe because it uses pointer arithmetic to find where to split the
/// string. `substr` must point into the same memory allocation as `str`. So this is ok:
/// ```rust
/// let str = "hello, world";
/// let (h, w) = split_at_substr(str, &str[6..]);
/// ```
/// This is not:
/// ```rust
/// let str = "undefined behaviour";
/// let (u, b) = split_at_substr(str, "behaviour");
/// ```
/// See [pointer::offset_from] for details.
unsafe fn split_at_substr<'s>(str: &'s str, substr: &'s str) -> (&'s str, &'s str) {
    let length = unsafe { substr.as_ptr().offset_from(str.as_ptr()) } as usize;
    (&str[..length], &str[length..])
}

/// Parses an expected option from the string. In USI, options are key-value pairs where the key
/// is a token and the value is an arbitrary number of tokens, only terminated by the end of the
/// line or by the start of a new option.
///
/// `follow_set` contains all the options that *could* follow this one - if we encounter any of
/// these tokens, parsing will stop there.
///
/// If it successfully parses the option, it will return a tuple containing the parsed value and
/// the rest of the string remaining to be parsed. If no such option exists, it returns Ok(None).
/// If the option name was given but the value is empty, returns Err.
fn parse_option<'i>(
    input: &'i str,
    option_name: &'i str,
    follow_set: &[&str],
) -> Result<Option<(&'i str, &'i str)>, EngineParseError<'i>> {
    // Since split_once will fail for strings that are just the option name, we should handle that
    // case first
    if input.trim_ascii() == option_name {
        return Err(EngineParseError::ArgumentEmpty(option_name));
    }

    let Some((_, rest)) = input
        .split_once([' ', '\t'])
        .filter(|(name, _)| *name == option_name)
    else {
        return Ok(None);
    };
    let rest = rest.trim_ascii_start();

    for token in rest.split_ascii_whitespace() {
        if follow_set.contains(&token) {
            // SAFETY: token is directly taken from rest so this is ok
            let (value, unparsed) = unsafe { split_at_substr(rest, token) };

            if value.is_empty() {
                dbg!(input);
                return Err(EngineParseError::ArgumentEmpty(option_name));
            } else {
                return Ok(Some((value.trim_ascii_end(), unparsed)));
            }
        }
    }
    Ok(Some((rest, "")))
}

/// Parses a single USI command.
pub fn parse_engine_command(line: &str) -> Result<EngineCommand, EngineParseError> {
    // I'm parsing this in a very blunt manual way because the UCI (and thus USI) protocol is very
    // ambiguous and more or less hostile towards sophistocated methods of parsing. So this is
    // (somehow) more fit for parsing the protocol
    let line = line.trim_ascii();
    if line.is_empty() {
        return Err(EngineParseError::Empty);
    }

    let (command, rest) = match line.split_once([' ', '\t']) {
        Some((command, rest)) => (command, Some(rest.trim_ascii_start())),
        None => (line, None),
    };

    match command {
        // I thought about making this command error if rest is nonempty but it seems stockfish
        // doesn't have a problem with it so ok
        "usi" => Ok(EngineCommand::Usi),
        "debug" => {
            let Some(on) = rest.and_then(|rest| match rest {
                "on" => Some(true),
                "off" => Some(false),
                _ => None,
            }) else {
                return Err(EngineParseError::InvalidArgument(rest.unwrap_or_default()));
            };

            Ok(EngineCommand::Debug(on))
        }
        "setoption" => {
            let Some(rest) = rest else {
                return Err(EngineParseError::ExpectedOption("name"));
            };
            let (name, rest) = parse_option(rest, "name", &["value"])
                .and_then(|res| res.ok_or(EngineParseError::ExpectedOption("name")))?;
            dbg!(parse_option(rest, "value", &[]));
            let value = parse_option(rest, "value", &[])?.map(|(val, _)| val);
            Ok(EngineCommand::SetOption { name, value })
        }
        _ => Err(EngineParseError::InvalidCommand(command)),
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn test_valid_commands() {
        assert_eq!(
            parse_engine_command("setoption name Shoginess value one hundred"),
            Ok(EngineCommand::SetOption {
                name: "Shoginess",
                value: Some("one hundred")
            })
        );

        assert_eq!(parse_engine_command("usi"), Ok(EngineCommand::Usi));
        assert_eq!(parse_engine_command("usi abaaab"), Ok(EngineCommand::Usi));
        assert_eq!(
            parse_engine_command("debug on"),
            Ok(EngineCommand::Debug(true))
        );
        assert_eq!(
            parse_engine_command("debug off"),
            Ok(EngineCommand::Debug(false))
        );
    }

    #[test]
    fn test_valid_commands_weird_whitespace() {
        assert_eq!(
            parse_engine_command("debug                                                on\t"),
            Ok(EngineCommand::Debug(true))
        );

        assert_eq!(
            parse_engine_command("setoption\t\tname\t\tshoginess\t\tvalue\t\tone hundred"),
            Ok(EngineCommand::SetOption {
                name: "shoginess",
                value: Some("one hundred"),
            })
        );
    }

    #[test]
    fn test_invalid_commands() {
        assert_eq!(
            parse_engine_command("abaaaab"),
            Err(EngineParseError::InvalidCommand("abaaaab"))
        );
        // The UCI/USI specs says this should be okay, but not even stockfish supports it.
        assert_eq!(
            parse_engine_command("joho usi"),
            Err(EngineParseError::InvalidCommand("joho"))
        );
        assert_eq!(
            parse_engine_command("setoption name value value 100"),
            Err(EngineParseError::ArgumentEmpty("name"))
        );
        assert_eq!(
            parse_engine_command("setoption name name value          "),
            Err(EngineParseError::ArgumentEmpty("value"))
        );
        assert_eq!(
            parse_engine_command("setoption"),
            Err(EngineParseError::ExpectedOption("name"))
        );
    }
}
