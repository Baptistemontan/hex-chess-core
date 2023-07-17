use serde::{Deserialize, Serialize};

use crate::{
    hex_coord::HexVector,
    piece::{Color, Piece, PieceKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Move {
    pub(crate) from: HexVector,
    pub(crate) to: HexVector,
    pub(crate) original_piece: Piece,
    pub(crate) becomes: PieceKind,
    pub(crate) take: Option<(Piece, HexVector)>,
}

impl Move {
    pub fn new(
        from: HexVector,
        to: HexVector,
        original_piece: Piece,
        becomes: PieceKind,
        take: Option<(Piece, HexVector)>,
    ) -> Self {
        Move {
            from,
            to,
            becomes,
            original_piece,
            take,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CanPromoteMove {
    from: HexVector,
    to: HexVector,
    color: Color,
    take: Option<(Piece, HexVector)>,
}

impl CanPromoteMove {
    pub(crate) fn promote_unchecked(self, promote_to: PieceKind) -> Move {
        Move::new(
            self.from,
            self.to,
            Piece::new(self.color, PieceKind::Pawn),
            promote_to,
            self.take,
        )
    }

    pub fn promote(self, promote_to: PieceKind) -> Result<Move, IllegalMove> {
        match promote_to {
            PieceKind::King | PieceKind::Pawn | PieceKind::OriginalPawn => {
                Err(IllegalMove::CantPromoteTo(promote_to))
            }
            _ => Ok(self.promote_unchecked(promote_to)),
        }
    }

    pub fn new(
        from: HexVector,
        to: HexVector,
        color: Color,
        take: Option<(Piece, HexVector)>,
    ) -> Self {
        CanPromoteMove {
            from,
            to,
            color,
            take,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MaybePromoteMove {
    CanPromote(CanPromoteMove),
    Move(Move),
}

impl MaybePromoteMove {
    pub(crate) fn promote_unchecked(self, promote_to: PieceKind) -> Move {
        match self {
            MaybePromoteMove::Move(mov) => mov,
            MaybePromoteMove::CanPromote(mov) => mov.promote_unchecked(promote_to),
        }
    }

    pub fn from(self) -> HexVector {
        match self {
            MaybePromoteMove::CanPromote(mov) => mov.from,
            MaybePromoteMove::Move(mov) => mov.from,
        }
    }

    pub fn to(self) -> HexVector {
        match self {
            MaybePromoteMove::CanPromote(mov) => mov.to,
            MaybePromoteMove::Move(mov) => mov.to,
        }
    }

    pub fn new_move(
        from: HexVector,
        to: HexVector,
        original_piece: Piece,
        becomes: PieceKind,
        take: Option<(Piece, HexVector)>,
    ) -> Self {
        MaybePromoteMove::Move(Move::new(from, to, original_piece, becomes, take))
    }

    pub fn new_can_promote(
        from: HexVector,
        to: HexVector,
        color: Color,
        take: Option<(Piece, HexVector)>,
    ) -> Self {
        MaybePromoteMove::CanPromote(CanPromoteMove::new(from, to, color, take))
    }

    pub fn is_promote_move(self) -> Result<CanPromoteMove, Move> {
        match self {
            MaybePromoteMove::CanPromote(mov) => Ok(mov),
            MaybePromoteMove::Move(mov) => Err(mov),
        }
    }

    pub fn is_normal_move(self) -> Result<Move, CanPromoteMove> {
        match self {
            MaybePromoteMove::Move(mov) => Ok(mov),
            MaybePromoteMove::CanPromote(mov) => Err(mov),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IllegalMove {
    SrcOutOfBound(HexVector),
    DestOutOfBound(HexVector),
    PieceInTheWay(Piece, HexVector),
    SrcPieceNotYours(Piece, HexVector),
    DestPieceYours(Piece, HexVector),
    SrcPieceNotPresent(HexVector),
    CantPromoteThisPiece(PieceKind),
    CantPromoteTo(PieceKind),
    InvalidMovement(Piece, HexVector),
    ResultToSelfInCheck,
}
