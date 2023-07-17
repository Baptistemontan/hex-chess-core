use std::ops::Not;

use crate::hex_coord::HexVector;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
pub enum Color {
    Black,
    White,
}

impl Not for Color {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
pub struct Piece {
    pub color: Color,
    pub kind: PieceKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
pub enum PieceKind {
    OriginalPawn,
    Pawn,
    Knight,
    Bishop,
    Rook,
    King,
    Queen,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PieceMove {
    pub vector: HexVector,
    pub can_be_rotated: bool,
    pub can_be_scaled: bool,
}

impl PieceMove {
    pub const fn new(vector: HexVector, can_be_rotated: bool, can_be_scaled: bool) -> Self {
        PieceMove {
            vector,
            can_be_rotated,
            can_be_scaled,
        }
    }

    pub fn does_match(self, vector: HexVector) -> bool {
        let vectors = if self.can_be_rotated {
            let mov = self.vector;
            let clock = mov.rotate_clock();
            let anti = mov.rotate_anti();
            [mov, clock, anti, -mov, -clock, -anti]
        } else {
            return self.vector == vector;
        };

        if self.can_be_scaled {
            for v in vectors {
                for k in 1..10 {
                    if v * k == vector {
                        return true;
                    }
                }
            }
        } else {
            for v in vectors {
                if v == vector {
                    return true;
                }
            }
        }

        false
    }

    pub fn get_all_moves_from(self, pos: HexVector) -> Vec<HexVector> {
        let moves: Vec<HexVector> = if self.can_be_rotated {
            let mov = self.vector;
            let clock = mov.rotate_clock();
            let anti = mov.rotate_anti();
            [mov, clock, anti, -mov, -clock, -anti].into()
        } else {
            [self.vector].into()
        };

        let mut moves = if self.can_be_scaled {
            let mut scaled_moves = Vec::new();
            for mov in moves {
                for k in 1..=10 {
                    scaled_moves.push(mov * k)
                }
            }
            scaled_moves
        } else {
            moves
        };

        moves.iter_mut().for_each(|mov| *mov += pos);
        moves.retain(|mov| mov.mag() <= 5);
        moves
    }
}

impl Piece {
    pub const fn new(color: Color, kind: PieceKind) -> Self {
        Piece { color, kind }
    }

    pub fn get_moves_from(self, origin: HexVector) -> Vec<HexVector> {
        let mut dests = Vec::new();
        for mov in self.kind.get_moves(self.color) {
            let mut moves = mov.get_all_moves_from(origin);
            dests.append(&mut moves);
        }
        dests
    }

    pub fn is_vector_valid(self, vector: HexVector) -> bool {
        self.kind
            .get_moves(self.color)
            .iter()
            .any(|mov| mov.does_match(vector))
    }
}

macro_rules! piece_move {
    (($q:expr, $r:expr), $rotated:expr, $scaled:expr) => {
        PieceMove::new(HexVector::new_axial($q, $r), $rotated, $scaled)
    };
    ($q:expr, $r:expr) => {
        PieceMove::new(HexVector::new_axial($q, $r), false, false)
    };
    (($q:expr, $r:expr), $rotated:expr, $scaled:expr) => {
        PieceMove::new(HexVector::new_axial($q, $r), $rotated, $scaled)
    };
}

impl PieceKind {
    pub const WHITE_ORIGINAL_PAWN_MOVES: &[PieceMove] = &[
        piece_move!(0, -1),
        piece_move!(0, -2),
        piece_move!(-1, 0),
        piece_move!(1, -1),
    ];
    pub const WHITE_PAWN_MOVES: &[PieceMove] =
        &[piece_move!(0, -1), piece_move!(-1, 0), piece_move!(1, -1)];
    pub const BLACK_ORIGINAL_PAWN_MOVES: &[PieceMove] = &[
        piece_move!(0, 1),
        piece_move!(0, 2),
        piece_move!(1, 0),
        piece_move!(-1, 1),
    ];
    pub const BLACK_PAWN_MOVES: &[PieceMove] =
        &[piece_move!(0, 1), piece_move!(1, 0), piece_move!(-1, 1)];
    pub const KNIGHT_MOVES: &[PieceMove] = &[
        piece_move!((2, -3), true, false),
        piece_move!((3, -2), true, false),
    ];
    pub const BISHOP_MOVES: &[PieceMove] = &[piece_move!((1, -2), true, true)];
    pub const ROOK_MOVES: &[PieceMove] = &[piece_move!((0, -1), true, true)];
    pub const KING_MOVES: &[PieceMove] = &[
        piece_move!((0, -1), true, false),
        piece_move!((1, -2), true, false),
    ];
    pub const QUEEN_MOVES: &[PieceMove] = &[
        piece_move!((0, -1), true, true),
        piece_move!((1, -2), true, true),
    ];

    pub const fn get_moves(self, color: Color) -> &'static [PieceMove] {
        match (self, color) {
            (PieceKind::OriginalPawn, Color::White) => Self::WHITE_ORIGINAL_PAWN_MOVES,
            (PieceKind::Pawn, Color::White) => Self::WHITE_PAWN_MOVES,
            (PieceKind::OriginalPawn, Color::Black) => Self::BLACK_ORIGINAL_PAWN_MOVES,
            (PieceKind::Pawn, Color::Black) => Self::BLACK_PAWN_MOVES,
            (PieceKind::Knight, _) => Self::KNIGHT_MOVES,
            (PieceKind::Bishop, _) => Self::BISHOP_MOVES,
            (PieceKind::Rook, _) => Self::ROOK_MOVES,
            (PieceKind::King, _) => Self::KING_MOVES,
            (PieceKind::Queen, _) => Self::QUEEN_MOVES,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_does_match() {
        for piece_kind in [
            PieceKind::Pawn,
            PieceKind::OriginalPawn,
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::King,
            PieceKind::Queen,
        ] {
            for color in [Color::White, Color::Black] {
                let piece = Piece::new(color, piece_kind);
                for vector in piece.get_moves_from(HexVector::new_axial(0, 0)) {
                    assert!(
                        piece.is_vector_valid(vector),
                        "color: {:?}, kind: {:?}, vector: {:?}",
                        color,
                        piece_kind,
                        vector
                    );
                }
            }
        }

        let rook_moves =
            Piece::new(Color::White, PieceKind::Rook).get_moves_from(HexVector::new_axial(0, 0));
        let bishop = Piece::new(Color::White, PieceKind::Bishop);
        for vector in rook_moves {
            assert!(!bishop.is_vector_valid(vector), " vector: {:?}", vector);
        }
    }
}
