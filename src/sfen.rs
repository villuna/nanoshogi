use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, one_of, space1},
    combinator::{opt, recognize},
    multi::{many_m_n, separated_list1},
    sequence::preceded,
};

pub const SFEN_STARTPOS: &'static str =
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1";

use crate::model::{Board, Hands, Move, Piece, PieceType, Player, Position, Square};

fn hand_piece(piece_char: char) -> impl Fn(&str) -> IResult<&str, u8> {
    move |input| {
        opt(
            (opt(recognize(digit1)), char(piece_char)).map_res(|(num, _): (Option<&str>, _)| {
                if let Some(n) = num {
                    n.parse::<u8>()
                } else {
                    Ok(1)
                }
            }),
        )
        // If nothing was parsed that means zero of that piece
        .map(|n| n.unwrap_or_default())
        .parse(input)
    }
}

fn nonempty_hands(mut input: &str) -> IResult<&str, Hands> {
    let black = "RBGSNLP";
    let white = "rbgsnlp";
    let mut black_hand = [0; 7];
    let mut white_hand = [0; 7];

    for (idx, c) in black.chars().enumerate() {
        let (i, n) = hand_piece(c)(input)?;
        input = i;
        black_hand[idx] = n;
    }

    for (idx, c) in white.chars().enumerate() {
        let (i, n) = hand_piece(c)(input)?;
        input = i;
        white_hand[idx] = n;
    }

    Ok((input, Hands::new(black_hand, white_hand)))
}

fn hands(input: &str) -> IResult<&str, Hands> {
    alt((tag("-").map(|_| Hands::new([0; 7], [0; 7])), nonempty_hands)).parse(input)
}

fn player(input: &str) -> IResult<&str, Player> {
    alt((
        tag("b").map(|_| Player::Black),
        tag("w").map(|_| Player::White),
    ))
    .parse(input)
}

enum RowEntry {
    Piece(Piece),
    Empty(u8),
}

fn square(input: &str) -> IResult<&str, Square> {
    (
        one_of("123456789").map(|c| c as u8 - b'1'),
        one_of("abcdefghi").map(|c| c as u8 - b'a'),
    )
        .map(|(x, y)| Square::new(x, y).unwrap())
        .parse(input)
}

fn normal_move(input: &str) -> IResult<&str, Move> {
    (square, square, opt(tag("+")))
        .map(|(from, to, promote)| Move::Move {
            from,
            to,
            promote: promote.is_some(),
        })
        .parse(input)
}

fn drop_move(input: &str) -> IResult<&str, Move> {
    (one_of("PLNSGBRK"), tag("*"), square)
        .map(|(piece, _, to)| Move::Drop {
            ty: piece_char_to_type(piece).unwrap(),
            to,
        })
        .parse(input)
}

fn r#move(input: &str) -> IResult<&str, Move> {
    alt((normal_move, drop_move)).parse(input)
}

fn piece_char_to_type(c: char) -> Option<PieceType> {
    match c.to_ascii_lowercase() {
        'p' => Some(PieceType::Pawn),
        'l' => Some(PieceType::Lance),
        'n' => Some(PieceType::Knight),
        's' => Some(PieceType::Silver),
        'g' => Some(PieceType::Gold),
        'b' => Some(PieceType::Bishop),
        'r' => Some(PieceType::Rook),
        'k' => Some(PieceType::King),
        _ => None,
    }
}

fn piece(input: &str) -> IResult<&str, Piece> {
    (
        opt(tag("+")).map(|p| p.is_some()),
        one_of("PLNSGBRKplnsgbrk"),
    )
        .map(|(promoted, piece)| {
            let player = if piece.is_ascii_uppercase() {
                Player::Black
            } else {
                Player::White
            };

            let mut ty = piece_char_to_type(piece).unwrap();

            if promoted {
                ty = ty.promoted().unwrap();
            }

            Piece::new(ty, player)
        })
        .parse(input)
}

fn row_entry(input: &str) -> IResult<&str, RowEntry> {
    alt((
        piece.map(RowEntry::Piece),
        one_of("123456789")
            .map(|c| c.to_digit(10).unwrap() as u8)
            .map(RowEntry::Empty),
    ))
    .parse(input)
}

fn row(input: &str) -> IResult<&str, [Option<Piece>; 9]> {
    many_m_n(1, 9, row_entry)
        .map(|entries| {
            let mut row = [None; 9];
            let mut i = 0;

            for entry in entries {
                match entry {
                    RowEntry::Piece(p) => {
                        row[i] = Some(p);
                        i += 1;
                    }
                    RowEntry::Empty(n) => i += n as usize,
                }
            }

            assert_eq!(i, 9);
            row
        })
        .parse(input)
}

fn board(input: &str) -> IResult<&str, Board> {
    separated_list1(tag("/"), row)
        .map(|rows| {
            let mut board = [None; 81];
            if rows.len() != 9 {
                // TODO graceful error handling
                panic!("Board must have 9 rows");
            }

            for (i, row) in rows.iter().enumerate() {
                for (j, piece) in row.iter().enumerate() {
                    board[i * 9 + j] = *piece;
                }
            }
            Board::new(board)
        })
        .parse(input)
}

fn sfen(input: &str) -> IResult<&str, Position> {
    (
        board,
        space1,
        player,
        space1,
        hands,
        opt(preceded(space1, digit1)),
    )
        .map(|(board, _, player, _, hands, ply)| {
            let ply = ply.map(|c| c.parse::<u32>().unwrap());
            Position::new(board, hands, player, ply)
        })
        .parse(input)
}

/// Parses a board position in sfen notation
pub fn parse_sfen(input: &str) -> Result<Position, nom::error::Error<&str>> {
    Ok(sfen(input).finish()?.1)
}

/// Parses a whitespace-separated list of shogi moves (move or drop)
pub fn parse_moves(input: &str) -> Result<Vec<Move>, nom::error::Error<&str>> {
    Ok(separated_list1(space1, r#move).parse(input).finish()?.1)
}

/// Parses a single shogi move (move or drop)
pub fn parse_move(input: &str) -> Result<Move, nom::error::Error<&str>> {
    Ok(r#move(input).finish()?.1)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sfen() {
        let startpos = parse_sfen(SFEN_STARTPOS);
        assert!(startpos.is_ok());

        let board = parse_sfen(
            "8l/1l+R2P3/p2pBG1pp/kps1p4/Nn1P2G2/P1P1P2PP/1PS6/1KSG3+r1/LN2+p3L w Sbgn3p 124",
        );
        assert!(board.is_ok());
    }

    #[test]
    fn test_hands() {
        assert_eq!(hands("-"), Ok(("", Hands::new([0; 7], [0; 7]))));
        assert_eq!(
            hands("Sbgn3p"),
            Ok(("", Hands::new([0, 0, 0, 1, 0, 0, 0], [0, 1, 1, 0, 1, 0, 3])))
        );
    }
}
