use haitaka_usi::SFEN_STARTPOS;

use crate::sfen::parse_sfen;

pub type Board = [Option<Piece>; 81];

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Player {
    Black,
    White,
}

#[derive(Default, Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Hand {
    counts: [u8; 7],
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum PieceType {
    Pawn,
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
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Piece {
    ty: PieceType,
    player: Player,
}

impl Piece {
    pub fn new(ty: PieceType, player: Player) -> Self {
        Self { ty, player }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Position {
    // I'll figure out how to do bitboards later
    board: Board,
    hands: [Hand; 2],
    player_to_move: Player,
    ply: Option<u32>,
}

impl Position {
    pub fn new(board: Board, hands: [Hand; 2], player_to_move: Player, ply: Option<u32>) -> Self {
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
}
