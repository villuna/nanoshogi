use std::fmt::Display;

use crate::sfen::{SFEN_STARTPOS, parse_sfen};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Board([Option<Piece>; 81]);

impl Board {
    pub fn new(board: [Option<Piece>; 81]) -> Self {
        Self(board)
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..9 {
            write!(f, "|--|--|--|--|--|--|--|--|--|\n")?;
            for x in 0..9 {
                let idx = Square::new(x, y).unwrap().index();

                match self.0[idx] {
                    None => write!(f, "|  ")?,
                    Some(piece) => {
                        let mut str = piece.ty.to_string();
                        if piece.player == Player::Black {
                            str = str.to_ascii_uppercase();
                        }
                        write!(f, "|{str:2}")?;
                    }
                }
            }

            write!(f, "|\n")?;
        }
        write!(f, "|--|--|--|--|--|--|--|--|--|\n")
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

    pub fn index(&self) -> usize {
        self.y as usize * 9 + self.x as usize
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

    /// Adds a piece into the given player's hand.
    pub fn insert_piece(&mut self, player: Player, piece: PieceType) {
        self.get_hand_mut(player)[piece as usize] += 1;
    }

    /// Takes a piece from the given players hand. Panics if this is not possible.
    pub fn take_piece(&mut self, player: Player, piece: PieceType) {
        self.get_hand_mut(player)[piece as usize]
            .checked_sub(1)
            .unwrap();
    }

    fn get_hand_mut(&mut self, player: Player) -> &mut [u8; 7] {
        match player {
            Player::Black => &mut self.black_hand,
            Player::White => &mut self.white_hand,
        }
    }
}

impl Display for Hands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "black's hand:")?;

        for (i, c) in "RBGSNLP".chars().enumerate() {
            if self.black_hand[i] > 0 {
                write!(f, " {}{c}", self.black_hand[i])?;
            }
        }
        write!(f, "\n")?;

        write!(f, "white's hand:")?;
        for (i, c) in "RBGSNLP".chars().enumerate() {
            if self.white_hand[i] > 0 {
                write!(f, " {}{c}", self.white_hand[i])?;
            }
        }

        Ok(())
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

    /// Attempts to promote this piece. An in-place version of [PieceType::promoted].
    /// If the piece cannot promote, just silently does nothing.
    pub fn promote(&mut self) {
        if let Some(p) = self.promoted() {
            *self = p;
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

    /// Returns the lowercase string representation of this piece
    pub fn to_string(&self) -> String {
        let str = match self {
            PieceType::Pawn => "p",
            PieceType::Lance => "l",
            PieceType::Knight => "n",
            PieceType::Silver => "s",
            PieceType::Gold => "g",
            PieceType::Bishop => "b",
            PieceType::Rook => "r",
            PieceType::ProPawn => "p+",
            PieceType::ProLance => "l+",
            PieceType::ProKnight => "n+",
            PieceType::ProSilver => "s+",
            PieceType::ProBishop => "b+",
            PieceType::ProRook => "r+",
            PieceType::King => "k",
        };

        str.to_string()
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

    /// Attempts to make a move on the board without checking whether that move can be made.
    /// If an invalid move is attempted it may leave the board in an invalid state or it may panic
    /// (though, this function is still safe by rust standards).
    pub fn make_move_unchecked(&mut self, mve: Move) {
        match mve {
            Move::Move { from, to, promote } => {
                if let Some(piece) = self.board.0[to.index()].as_ref() {
                    self.hands.insert_piece(self.player_to_move, piece.ty);
                }
                self.board.0[to.index()] = self.board.0[from.index()];
                if promote {
                    self.board.0[to.index()].as_mut().unwrap().ty.promote();
                }
            }
            Move::Drop { ty, to } => {
                self.hands.take_piece(self.player_to_move, ty);
                self.board.0[to.index()] = Some(Piece {
                    ty,
                    player: self.player_to_move,
                })
            }
        }
    }

    /// Attempts to unmake a move on the board without checking whether that move was just made.
    /// If an invalid unmove is attempted it may leave the board in an invalid state or it may
    /// panic (though, this function is still safe by rust standards).
    pub fn unmake_move_unchecked(&mut self, mve: Move) {
        todo!();
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}\n", self.board, self.hands,)?;
        if let Some(ply) = self.ply {
            write!(f, "move {ply}, ")?;
        }

        write!(f, "{:?} to move", self.player_to_move)
    }
}

#[cfg(test)]
mod test {
    use crate::model::Hands;

    #[test]
    fn test_eval_hand() {
        let hands = Hands::new([1, 0, 0, 0, 0, 0, 0], [0; 7]);
        assert_eq!(hands.eval(), 1.15);
    }
}
