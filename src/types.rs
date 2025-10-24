use crate::sfen::{SFEN_STARTPOS, parse_sfen};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Board([Option<Piece>; 81]);

impl Board {
    pub fn new(board: [Option<Piece>; 81]) -> Self {
        Self(board)
    }
}

/// The coordinate of a square on the board.
/// x is the file and y is the rank. They are both 0-indexed, so x and y both go from 0-8.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Square {
    x: u8,
    y: u8,
}

impl Square {
    pub fn new(x: u8, y: u8) -> Option<Self> {
        (x < 9 && y < 9).then_some(Self { x, y })
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Move {
    Move {
        from: Square,
        to: Square,
        promote: bool,
    },
    Drop {
        ty: PieceType,
        to: Square,
    },
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Player {
    Black,
    White,
}

#[derive(Default, Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Hands {
    black_hand: [u8; 7],
    white_hand: [u8; 7],
}

impl Hands {
    pub fn new(black: [u8; 7], white: [u8; 7]) -> Self {
        Self {
            black_hand: black,
            white_hand: white,
        }
    }

    /// Returns the total evaluation of the material in each player's hand
    pub fn eval(&self) -> f32 {
        // Pieces in the hand are worth slightly more than pieces on the board
        let scale = 1.15;
        let mut total = 0.0;

        for i in 0u8..7u8 {
            // SAFETY: PieceType is repr(u8) and 0..7 is within range.
            let piece: PieceType = unsafe { std::mem::transmute(i) };

            total += piece.material() * self.black_hand[i as usize] as f32 * scale;
            total += -piece.material() * self.white_hand[i as usize] as f32 * scale;
        }

        total
    }
}

/// The type of a piece.
///
/// Encodes both the type and whether or not it is promoted (since not all pieces can promote).
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
#[repr(u8)]
pub enum PieceType {
    Pawn = 0,
    Lance,
    Knight,
    Silver,
    Gold,
    Bishop,
    Rook,
    ProPawn,
    ProLance,
    ProKnight,
    ProSilver, // tony hawk's pro silver
    ProBishop,
    ProRook,
    King,
}

impl PieceType {
    /// Attempts to convert this piece into a promoted version of itself.
    /// If the piece is already promoted, it just returns itself. If it cannot promote, it returns
    /// None.
    pub fn promoted(self) -> Option<Self> {
        match self {
            PieceType::Pawn | PieceType::ProPawn => Some(PieceType::ProPawn),
            PieceType::Lance | PieceType::ProLance => Some(PieceType::ProLance),
            PieceType::Knight | PieceType::ProKnight => Some(PieceType::ProKnight),
            PieceType::Silver | PieceType::ProSilver => Some(PieceType::ProSilver),
            PieceType::Bishop | PieceType::ProBishop => Some(PieceType::ProBishop),
            PieceType::Rook | PieceType::ProRook => Some(PieceType::ProRook),
            PieceType::King | PieceType::Gold => None,
        }
    }

    /// Returns the number of points of material this piece represents
    /// Based off of Kohji Tanigawa's 2006 scheme
    /// (https://en.wikipedia.org/wiki/Shogi_strategy#Relative_piece_value)
    pub fn material(&self) -> f32 {
        match self {
            PieceType::Pawn => 1.0,
            PieceType::Lance => 3.0,
            PieceType::Knight => 4.0,
            PieceType::Silver => 5.0,
            PieceType::Gold => 6.0,
            PieceType::Bishop => 8.0,
            PieceType::Rook => 10.0,
            PieceType::ProPawn => 6.0,
            PieceType::ProLance => 6.0,
            PieceType::ProKnight => 6.0,
            PieceType::ProSilver => 6.0,
            PieceType::ProBishop => 10.0,
            PieceType::ProRook => 12.0,
            // The king is priceless
            PieceType::King => 0.0,
        }
    }
}

/// Represents a piece on the board. Encodes the piece's type and the player who owns it.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Piece {
    ty: PieceType,
    player: Player,
}

impl Piece {
    pub fn new(ty: PieceType, player: Player) -> Self {
        Self { ty, player }
    }

    /// Returns the number of points of material this piece represents
    /// black's pieces get a positive evaluation, white's pieces are negative
    pub fn eval(&self) -> f32 {
        let coeff = if self.player == Player::White {
            -1.0
        } else {
            1.0
        };

        self.ty.material() * coeff
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Position {
    // I'll figure out how to do bitboards later
    board: Board,
    hands: Hands,
    player_to_move: Player,
    ply: Option<u32>,
}

impl Position {
    pub fn new(board: Board, hands: Hands, player_to_move: Player, ply: Option<u32>) -> Self {
        Self {
            board,
            hands,
            player_to_move,
            ply,
        }
    }
    pub fn startpos() -> Self {
        Self::from_sfen(SFEN_STARTPOS)
    }

    pub fn from_sfen(input: &str) -> Self {
        parse_sfen(input).unwrap()
    }

    pub fn possible_moves(&self) -> Vec<Move> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::types::Hands;

    #[test]
    fn test_eval_hand() {
        let hands = Hands::new([1, 0, 0, 0, 0, 0, 0], [0; 7]);
        assert_eq!(hands.eval(), 1.15);
    }
}
