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

    /// Attempts to add an offset to this square's coordinates. Returns None if the result would be
    /// off the board.
    pub fn add(&self, offset: (i8, i8)) -> Option<Self> {
        let x = self.x.checked_add_signed(offset.0)?;
        let y = self.y.checked_add_signed(offset.1)?;
        Square::new(x, y)
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

impl Player {
    /// Returns the opposing player.
    pub fn opposite(&self) -> Player {
        match self {
            Player::Black => Player::White,
            Player::White => Player::Black,
        }
    }
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
    pub fn insert_piece(&mut self, player: Player, mut piece: PieceType) {
        piece.unpromote();
        self.get_hand_mut(player)[piece as usize] += 1;
    }

    /// Takes a piece from the given players hand. Panics if this is not possible.
    pub fn take_piece(&mut self, player: Player, mut piece: PieceType) {
        piece.unpromote();
        let count = &mut self.get_hand_mut(player)[piece as usize];
        *count = count.checked_sub(1).unwrap();
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
    Rook = 0,
    Bishop,
    Gold,
    Silver,
    Knight,
    Lance,
    Pawn,
    ProRook,
    ProBishop,
    ProSilver, // tony hawk's pro silver
    ProKnight,
    ProLance,
    ProPawn,
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

    pub fn unpromoted(&self) -> Option<Self> {
        match self {
            PieceType::ProPawn => Some(PieceType::Pawn),
            PieceType::ProLance => Some(PieceType::Lance),
            PieceType::ProKnight => Some(PieceType::Knight),
            PieceType::ProSilver => Some(PieceType::Silver),
            PieceType::ProBishop => Some(PieceType::Bishop),
            PieceType::ProRook => Some(PieceType::Rook),
            _ => None,
        }
    }

    /// Attempts to promote this piece. An in-place version of [PieceType::promoted].
    /// If the piece cannot promote, just silently does nothing.
    pub fn promote(&mut self) {
        if let Some(p) = self.promoted() {
            *self = p;
        }
    }

    pub fn unpromote(&mut self) {
        if let Some(p) = self.unpromoted() {
            *self = p;
        }
    }

    /// Returns whether a piece of this type can promote. Pieces that are already promoted cannot
    /// promote.
    pub fn can_promote(&self) -> bool {
        match self {
            PieceType::Pawn
            | PieceType::Lance
            | PieceType::Knight
            | PieceType::Silver
            | PieceType::Bishop
            | PieceType::Rook => true,
            _ => false,
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
    ply: u32,
    // A stack of pieces taken and the ply they were taken on. Used for unmaking moves.
    pieces_taken: Vec<(u32, Piece)>,
}

impl Position {
    pub fn new(board: Board, hands: Hands, player_to_move: Player, ply: Option<u32>) -> Self {
        Self {
            board,
            hands,
            player_to_move,
            ply: ply.unwrap_or(1),
            pieces_taken: vec![],
        }
    }
    pub fn startpos() -> Self {
        Self::from_sfen(SFEN_STARTPOS)
    }

    pub fn from_sfen(input: &str) -> Self {
        parse_sfen(input).unwrap()
    }

    /// Finds the position of the given player's king on the board
    fn find_king(&self, player: Player) -> Square {
        for x in 0..9 {
            for y in 0..9 {
                let square = Square::new(x, y).unwrap();
                if self.board.0[square.index()]
                    == Some(Piece {
                        ty: PieceType::King,
                        player,
                    })
                {
                    return square;
                }
            }
        }

        unreachable!()
    }

    /// Checks if the given player's king is in check
    fn king_in_check(&self, player: Player) -> bool {
        let attacker = player.opposite();
        let king_square = self.find_king(player);

        for x in 0..9 {
            for y in 0..9 {
                let square = Square::new(x, y).unwrap();

                if let Some(piece) = self.board.0[square.index()]
                    && piece.player == attacker
                {
                    if self.movable_squares(square).contains(&king_square) {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Returns a vector containing all the possible moves that can be made in this position.
    pub fn possible_moves(&mut self) -> Vec<Move> {
        let mut res = vec![];
        for x in 0..9 {
            for y in 0..9 {
                let square = Square::new(x, y).unwrap();
                if let Some(piece) = self.board.0[square.index()]
                    && piece.player == self.player_to_move
                {
                    res.extend(self.moves_for_piece(square));
                }
            }
        }

        // TODO drops

        // Only return the moves that would not leave the king in check

        let player = self.player_to_move;
        res.into_iter()
            .filter(|mve| {
                self.make_move_unchecked(*mve);
                let in_check = self.king_in_check(player);
                self.unmake_move_unchecked(*mve);
                !in_check
            })
            .collect()
    }

    /// Returns all the possible moves the given piece can make.
    fn moves_for_piece(&self, from: Square) -> Vec<Move> {
        let piece = self.board.0[from.index()].as_ref().unwrap();
        let squares = self.movable_squares(from);
        let mut moves = vec![];

        for square in squares {
            let is_promote_square = if piece.player == Player::Black {
                square.y <= 2
            } else {
                square.y >= 6
            };

            if is_promote_square && piece.ty.can_promote() {
                moves.push(Move::Move {
                    from,
                    to: square,
                    promote: true,
                });
            }

            // TODO test if a piece is physically able to keep going without promoting - if not,
            // it *has* to promote.

            moves.push(Move::Move {
                from,
                to: square,
                promote: false,
            });
        }

        moves
    }

    // helper function for [Position::movable_squares]. Given a list of squares (positions relative
    // to the current square), returns all the squares that a piece can move to. Used for pieces
    // that just have a set of squares they can move to.
    //
    // Note that the offsets will also be from the perspective of the moving player - so, the y
    // axis will be flipped if the moving player is black.
    fn movable_squares_offsets(
        &self,
        from: Square,
        offsets: &[(i8, i8)],
        moving_player: Player,
    ) -> Vec<Square> {
        let mut res = vec![];

        for &(mut offset) in offsets {
            if moving_player == Player::Black {
                offset.1 *= -1;
            };
            let Some(square) = from.add(offset) else {
                continue;
            };

            if self.square_is_available(square, moving_player) {
                res.push(square);
            }
        }

        res
    }

    // Helper function for [Position::movable_squares]. Given a list of offsets (or directions) from
    // the current square, attempts to move as far as possible in those directions until stopping.
    // Used for pieces that move any number of squares in a certain set of directions.
    //
    // Similarly to [Position::movable_squares_offsets], the y values of the offsets are
    // flipped for Black.
    fn movable_squares_march(
        &self,
        from: Square,
        offsets: &[(i8, i8)],
        moving_player: Player,
    ) -> Vec<Square> {
        let mut res = vec![];

        for &(mut offset) in offsets {
            if moving_player == Player::Black {
                offset.1 *= -1;
            };

            let mut square = from;

            loop {
                let Some(sq) = square.add(offset) else {
                    break;
                };
                square = sq;
                if !self.square_is_available(square, moving_player) {
                    break;
                }

                res.push(square);
                if self.board.0[square.index()].is_some() {
                    // There is an enemy piece here so we can't go any further
                    break;
                }
            }
        }

        res
    }

    /// Given the coordinate of a piece on the board, returns all the squares that piece can move
    /// to. If `disallow_check` is true, filters out all the moves that would leave the king in
    /// check.
    fn movable_squares(&self, from: Square) -> Vec<Square> {
        let piece = self.board.0[from.index()].as_ref().unwrap();

        match piece.ty {
            PieceType::Pawn => {
                // The pawn can only move one square forward (and, mercifully, captures forward
                // as well).
                self.movable_squares_offsets(from, &[(0, 1)], piece.player)
            }
            PieceType::Lance => {
                // The lance is like a rook that can only move forward.
                self.movable_squares_march(from, &[(0, 1)], piece.player)
            }
            PieceType::Knight => {
                // The knight moves two squares forward and one square either left or right.
                // Just like chess, it can jump over pieces.
                self.movable_squares_offsets(from, &[(-1, 2), (1, 2)], piece.player)
            }
            PieceType::Silver => self.movable_squares_offsets(
                from,
                &[(-1, 1), (0, 1), (1, 1), (-1, -1), (1, -1)],
                piece.player,
            ),
            PieceType::Gold
            | PieceType::ProPawn
            | PieceType::ProLance
            | PieceType::ProKnight
            | PieceType::ProSilver => self.movable_squares_offsets(
                from,
                &[(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (0, -1)],
                piece.player,
            ),
            PieceType::Bishop => self.movable_squares_march(
                from,
                &[(-1, -1), (-1, 1), (1, -1), (1, 1)],
                piece.player,
            ),
            PieceType::Rook => {
                self.movable_squares_march(from, &[(-1, 0), (1, 0), (0, -1), (0, 1)], piece.player)
            }
            PieceType::ProBishop => {
                // Like a bishop...
                let mut res = self.movable_squares_march(
                    from,
                    &[(-1, -1), (-1, 1), (1, -1), (1, 1)],
                    piece.player,
                );
                // but can move one square in the directions a rook can
                res.extend(self.movable_squares_offsets(
                    from,
                    &[(-1, 0), (1, 0), (0, -1), (0, 1)],
                    piece.player,
                ));
                res
            }
            PieceType::ProRook => {
                // Like a rook...
                let mut res = self.movable_squares_march(
                    from,
                    &[(-1, 0), (1, 0), (0, -1), (0, 1)],
                    piece.player,
                );
                // but can move one square in the directions a bishop can
                res.extend(self.movable_squares_offsets(
                    from,
                    &[(-1, -1), (-1, 1), (1, -1), (1, 1)],
                    piece.player,
                ));
                res
            }
            PieceType::King => self.movable_squares_offsets(
                from,
                &[
                    (-1, 1),
                    (0, 1),
                    (1, 1),
                    (-1, 0),
                    (1, 0),
                    (-1, -1),
                    (0, -1),
                    (1, -1),
                ],
                piece.player,
            ),
        }
    }

    /// Returns whether a player could theoretically move a piece to that square (i.e., the square
    /// is empty or contains an enemy piece)
    fn square_is_available(&self, target: Square, moving_player: Player) -> bool {
        self.board.0[target.index()].is_none_or(|piece| piece.player == moving_player.opposite())
    }

    /// Attempts to make a move on the board without checking whether that move can be made.
    /// If an invalid move is attempted it may leave the board in an invalid state or it may panic
    /// (though, this function is still safe by rust standards).
    pub fn make_move_unchecked(&mut self, mve: Move) {
        match mve {
            Move::Move { from, to, promote } => {
                // If we are taking a piece, remove it off the board and put it in the player's
                // hand.
                if let Some(piece) = self.board.0[to.index()].take() {
                    self.hands.insert_piece(self.player_to_move, piece.ty);
                    self.pieces_taken.push((self.ply, piece));
                }

                // Move the piece
                self.board.0[to.index()] = self.board.0[from.index()];
                self.board.0[from.index()] = None;

                // Promote if necessary
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

        // Update internal move counters
        self.ply += 1;
        self.player_to_move = self.player_to_move.opposite();
    }

    /// Attempts to unmake a move on the board without checking whether that move was just made.
    /// If an invalid unmove is attempted it may leave the board in an invalid state or it may
    /// panic (though, this function is still safe by rust standards).
    pub fn unmake_move_unchecked(&mut self, mve: Move) {
        self.ply -= 1;
        self.player_to_move = self.player_to_move.opposite();

        match mve {
            Move::Move { from, to, promote } => {
                // Unmove and unpromote
                self.board.0[from.index()] = self.board.0[to.index()];

                if promote {
                    self.board.0[from.index()].as_mut().unwrap().ty.unpromote();
                }

                if self
                    .pieces_taken
                    .last()
                    .is_some_and(|(p, _)| *p == self.ply)
                {
                    // If this move took a piece, replace it.
                    let (_, taken_piece) = self.pieces_taken.pop().unwrap();
                    self.board.0[to.index()] = Some(taken_piece);
                    self.hands.take_piece(self.player_to_move, taken_piece.ty);
                } else {
                    // Otherwise we should clear that square
                    self.board.0[to.index()] = None;
                }
            }
            Move::Drop { ty, to } => {
                self.board.0[to.index()] = None;
                self.hands.insert_piece(self.player_to_move, ty);
            }
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}\n{:?} to move",
            self.board, self.hands, self.player_to_move
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eval_hand() {
        let mut hands = Hands::new([0; 7], [0; 7]);
        hands.insert_piece(Player::Black, PieceType::Pawn);
        assert_eq!(hands.eval(), 1.15);
    }

    #[test]
    fn test_make_unmake() {
        // Test making and immediately unmaking a bunch of moves leaves the position unchanged
        let mut startpos = Position::from_sfen(SFEN_STARTPOS);
        let moves = startpos.possible_moves();

        let mut testpos = Position::from_sfen(SFEN_STARTPOS);

        for mve in moves {
            testpos.make_move_unchecked(mve);
            testpos.unmake_move_unchecked(mve);
            assert_eq!(startpos, testpos);
        }
    }

    #[test]
    fn test_take_untake() {
        let startpos = Position::from_sfen("9/4k4/9/9/9/9/4g4/4G4/4K4 b - 1");
        let mut testpos = startpos.clone();
        let mve = Move::Move {
            from: Square::new(4, 7).unwrap(),
            to: Square::new(4, 6).unwrap(),
            promote: false,
        };

        testpos.make_move_unchecked(mve);
        testpos.unmake_move_unchecked(mve);

        assert_eq!(startpos, testpos);
    }

    #[test]
    fn test_promote_unpromote() {
        // Test promoting and unpromoting
        let startpos = Position::from_sfen("9/4k4/9/P8/9/9/9/9/4K4 b - 1");
        let mut testpos = startpos.clone();
        let mve = Move::Move {
            from: Square::new(0, 3).unwrap(),
            to: Square::new(0, 2).unwrap(),
            promote: true,
        };

        testpos.make_move_unchecked(mve);

        assert_eq!(
            testpos.board.0[Square::new(0, 2).unwrap().index()],
            Some(Piece::new(PieceType::ProPawn, Player::Black))
        );

        testpos.unmake_move_unchecked(mve);

        assert_eq!(startpos, testpos);
    }

    #[test]
    fn test_drop_undrop() {
        let startpos = Position::from_sfen("1k7/9/1K7/9/9/9/9/9/9 b G 1");
        let mut testpos = startpos.clone();
        let mve = Move::Drop {
            ty: PieceType::Gold,
            to: Square::new(1, 1).unwrap(),
        };

        dbg!(&testpos);

        testpos.make_move_unchecked(mve);
        assert_eq!(
            testpos.board.0[Square::new(1, 1).unwrap().index()],
            Some(Piece::new(PieceType::Gold, Player::Black))
        );
        testpos.unmake_move_unchecked(mve);
        assert_eq!(startpos, testpos);
    }

    #[test]
    fn checkmate_no_moves() {
        let mut checkmate = Position::from_sfen("1k7/1G7/1K7/9/9/9/9/9/9 w - 1");
        assert_eq!(checkmate.possible_moves().len(), 0);
    }

    #[test]
    fn king_in_check() {
        let startpos = Position::from_sfen(SFEN_STARTPOS);
        assert!(!startpos.king_in_check(Player::Black));
        assert!(!startpos.king_in_check(Player::White));

        let checkmate = Position::from_sfen("1k7/1G7/1K7/9/9/9/9/9/9 w - 1");
        assert!(!checkmate.king_in_check(Player::Black));
        assert!(checkmate.king_in_check(Player::White));
    }
}
