use nom::{
    Finish, IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of, space1},
    combinator::opt,
    multi::{many_m_n, separated_list1},
    sequence::preceded,
};

use crate::types::{Board, Hand, Piece, PieceType, Player, Position};

fn hands(input: &str) -> IResult<&str, [Hand; 2]> {
    alt((tag("-").map(|_| [Hand::default(), Hand::default()]),)).parse(input)
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

            let mut ty = match piece.to_ascii_lowercase() {
                'p' => PieceType::Pawn,
                'l' => PieceType::Lance,
                'n' => PieceType::Knight,
                's' => PieceType::Silver,
                'g' => PieceType::Gold,
                'b' => PieceType::Bishop,
                'r' => PieceType::Rook,
                'k' => PieceType::King,
                _ => unreachable!(),
            };

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
            board
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

pub fn parse_sfen(input: &str) -> Result<Position, nom::error::Error<&str>> {
    Ok(sfen(input).finish()?.1)
}

#[cfg(test)]
mod test {
    use haitaka_usi::SFEN_STARTPOS;

    use super::*;

    #[test]
    fn test_sfen() {
        let startpos = parse_sfen(SFEN_STARTPOS);
        println!("{startpos:?}");
        assert!(false);
    }
}
