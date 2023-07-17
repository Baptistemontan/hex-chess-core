use std::collections::{HashMap, HashSet};

use crate::{
    hex_coord::{HexMap, HexVector},
    mov::{CanPromoteMove, IllegalMove, MaybePromoteMove, Move},
    piece::{Color, Piece, PieceKind, PieceMove},
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Board {
    map: HexMap<Option<Piece>>,
    history: Vec<Move>,
    current_color_turn: Color,
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Board {
    const fn original_piece_at(pos: HexVector) -> Option<Piece> {
        let (color, pos) = if pos.get_r() >= 0 {
            (Color::White, pos)
        } else {
            (Color::Black, pos.neg())
        };

        match (pos.get_q(), pos.get_r(), color) {
            (0, 1, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (1, 1, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (2, 1, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (3, 1, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (4, 1, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (-1, 2, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (-2, 3, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (-3, 4, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (-4, 5, _) => Some(Piece::new(color, PieceKind::OriginalPawn)),
            (-3, 5, _) => Some(Piece::new(color, PieceKind::Rook)),
            (3, 2, _) => Some(Piece::new(color, PieceKind::Rook)),
            (-2, 5, _) => Some(Piece::new(color, PieceKind::Knight)),
            (2, 3, _) => Some(Piece::new(color, PieceKind::Knight)),
            (0, 3, _) => Some(Piece::new(color, PieceKind::Bishop)),
            (0, 4, _) => Some(Piece::new(color, PieceKind::Bishop)),
            (0, 5, _) => Some(Piece::new(color, PieceKind::Bishop)),
            // Queen and queens are not reversed on an hex board
            (-1, 5, Color::White) => Some(Piece::new(color, PieceKind::Queen)),
            (-1, 5, Color::Black) => Some(Piece::new(color, PieceKind::King)),
            (1, 4, Color::White) => Some(Piece::new(color, PieceKind::King)),
            (1, 4, Color::Black) => Some(Piece::new(color, PieceKind::Queen)),
            _ => None,
        }
    }

    fn get_default_map() -> HexMap<Option<Piece>> {
        HexMap::new_with_init(5, Self::original_piece_at)
    }

    pub fn new() -> Self {
        Board {
            map: Self::get_default_map(),
            history: Vec::new(),
            current_color_turn: Color::White,
        }
    }

    pub fn get_player_turn(&self) -> Color {
        self.current_color_turn
    }

    pub fn get_current_player_pieces(&self) -> impl Iterator<Item = (HexVector, Piece)> + '_ {
        let filter_fn = |(coord, piece): (HexVector, &Option<Piece>)| match piece {
            Some(piece) if piece.color == self.current_color_turn => Some((coord, *piece)),
            _ => None,
        };
        self.map.iter().filter_map(filter_fn)
    }

    pub fn get_all_taken_pieces(&self) -> impl Iterator<Item = Piece> + '_ {
        self.history
            .iter()
            .filter_map(|mov| mov.take.map(|(piece, _)| piece))
    }

    pub fn get_taken_pieces_for(&self, color: Color) -> impl Iterator<Item = PieceKind> + '_ {
        let filter_fn = move |piece: Piece| {
            if piece.color != color {
                Some(piece.kind)
            } else {
                None
            }
        };

        self.get_all_taken_pieces().filter_map(filter_fn)
    }

    pub fn get_piece_at(&self, pos: HexVector) -> Option<Piece> {
        self.map.get(pos).copied().flatten()
    }

    fn handle_promotion(
        from: HexVector,
        to: HexVector,
        color: Color,
        promote_to: Option<PieceKind>,
        take: Option<(Piece, HexVector)>,
    ) -> Result<MaybePromoteMove, IllegalMove> {
        match promote_to {
            // check invalid promotion
            Some(promote_to @ (PieceKind::King | PieceKind::OriginalPawn | PieceKind::Pawn)) => {
                Err(IllegalMove::CantPromoteTo(promote_to))
            }
            Some(promote_to) => Ok(MaybePromoteMove::Move(Move::new(
                from,
                to,
                Piece::new(color, PieceKind::Pawn),
                promote_to,
                take,
            ))),
            None => Ok(MaybePromoteMove::new_can_promote(from, to, color, take)),
        }
    }

    fn can_go_from_to(
        &mut self,
        piece: Piece,
        from: HexVector,
        to: HexVector,
        promote_to: Option<PieceKind>,
    ) -> Result<MaybePromoteMove, IllegalMove> {
        let take_piece = self.get_piece_at(to).map(|p| (p, to));

        match take_piece {
            Some((dest_piece, _)) if dest_piece.color == piece.color => {
                return Err(IllegalMove::DestPieceYours(dest_piece, to))
            }
            _ => {}
        }

        let default_move = MaybePromoteMove::new_move(from, to, piece, piece.kind, take_piece);

        let vector = to - from;
        // println!("normalized vec: {:?}", normalized_vec);

        let mov: MaybePromoteMove = match (
            piece.kind,
            piece.color,
            take_piece,
            self.history.last(),
            promote_to,
        ) {
            // as long as the destination piece is not the same color, knight and king can move.
            (PieceKind::King | PieceKind::Knight, _, _, _, None) => Ok(default_move),
            // white pawn advance
            (PieceKind::Pawn | PieceKind::OriginalPawn, Color::White, None, _, promote_to)
                if vector == HexVector::new_axial(0, -1) =>
            {
                if to.mag() == 5 && to.distance(HexVector::new_axial(0, -5)) <= 5 {
                    // can promote
                    Self::handle_promotion(from, to, Color::White, promote_to, take_piece)
                } else {
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::Pawn,
                        None,
                    ))
                }
            }
            // white pawn advance twice
            (PieceKind::OriginalPawn, Color::White, None, _, None)
                if vector == HexVector::new_axial(0, -2)
                    && self
                        .get_piece_at(from + HexVector::new_axial(0, -1))
                        .is_none() =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    PieceKind::Pawn,
                    None,
                ))
            }
            // black pawn advance
            (PieceKind::Pawn | PieceKind::OriginalPawn, Color::Black, None, _, promote_to)
                if vector == HexVector::new_axial(0, 1) =>
            {
                if to.mag() == 5 && to.distance(HexVector::new_axial(0, 5)) <= 5 {
                    // can promote
                    Self::handle_promotion(from, to, Color::Black, promote_to, take_piece)
                } else {
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::Pawn,
                        None,
                    ))
                }
            }
            // black pawn advance twice
            (PieceKind::OriginalPawn, Color::Black, None, _, None)
                if vector == HexVector::new_axial(0, 2)
                    && self
                        .get_piece_at(from + HexVector::new_axial(0, 1))
                        .is_none() =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    PieceKind::Pawn,
                    None,
                ))
            }
            // white pawn takes
            (PieceKind::Pawn | PieceKind::OriginalPawn, Color::White, Some(_), _, promote_to)
                if vector == HexVector::new_axial(-1, 0)
                    || vector == HexVector::new_axial(1, -1) =>
            {
                if to.mag() == 5 && to.distance(HexVector::new_axial(0, -5)) <= 5 {
                    // can promote
                    Self::handle_promotion(from, to, Color::White, promote_to, take_piece)
                } else if Self::original_piece_at(to)
                    .is_some_and(|original_piece| original_piece.color == Color::White)
                {
                    // if the pawn takes and land on a place where there is originally a piece of it's color, it means
                    // that it's an orginal pawn and land on an original pawn position, so it stays an original pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::OriginalPawn,
                        take_piece,
                    ))
                } else {
                    // else it becomes a normal pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::Pawn,
                        take_piece,
                    ))
                }
            }
            // black pawn takes
            (PieceKind::Pawn | PieceKind::OriginalPawn, Color::Black, Some(_), _, promote_to)
                if vector == HexVector::new_axial(-1, 1)
                    || vector == HexVector::new_axial(1, 0) =>
            {
                if to.mag() == 5 && to.distance(HexVector::new_axial(0, 5)) <= 5 {
                    // can promote
                    Self::handle_promotion(from, to, Color::Black, promote_to, take_piece)
                } else if Self::original_piece_at(to)
                    .is_some_and(|original_piece| original_piece.color == Color::Black)
                {
                    // if the pawn takes and land on a place where there is originally a piece of it's color, it means
                    // that it's an orginal pawn and land on an original pawn position, so it stays an original pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::OriginalPawn,
                        take_piece,
                    ))
                } else {
                    // else it becomes a normal pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        PieceKind::Pawn,
                        take_piece,
                    ))
                }
            }
            // white pawn en passant
            (PieceKind::Pawn, Color::White, None, Some(last_move), None)
                if last_move.original_piece.kind == PieceKind::OriginalPawn
                    && ((vector == HexVector::new_axial(-1, 0)
                        && last_move.to == (from + HexVector::new_axial(-1, 1)))
                        || (vector == HexVector::new_axial(1, -1)
                            && last_move.to == (from + HexVector::new_axial(1, 0)))) =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    piece.kind,
                    Some((last_move.original_piece, last_move.to)),
                ))
            }
            // black pawn en passant
            (PieceKind::Pawn, Color::Black, None, Some(last_move), None)
                if last_move.original_piece.kind == PieceKind::OriginalPawn
                    && ((vector == HexVector::new_axial(-1, 1)
                        && last_move.to == (from + HexVector::new_axial(-1, 0)))
                        || (vector == HexVector::new_axial(1, 0)
                            && last_move.to == (from + HexVector::new_axial(1, -1)))) =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    PieceKind::Pawn,
                    Some((last_move.original_piece, last_move.to)),
                ))
            }

            // invalid pawn move
            (PieceKind::Pawn, _, _, _, None) => Err(IllegalMove::InvalidMovement(piece, vector)),
            // for any other piece check if a piece is in the way
            (_, _, _, _, None) => {
                let normalized_vec = vector.normalize();
                let mut mov = Ok(default_move);
                let gcd = vector.gcd();
                for k in 1..gcd {
                    let dest = from + normalized_vec * k;
                    if let Some(dest_piece) = self.get_piece_at(dest) {
                        mov = Err(IllegalMove::PieceInTheWay(dest_piece, dest));
                        break;
                    }
                }
                mov
            }
            // invalid promote
            (_, _, _, _, Some(_)) => Err(IllegalMove::CantPromoteThisPiece(piece.kind)),
        }?;

        // if the mov is valid, check if moving result in check
        if self.check_move_is_not_check(mov) {
            Ok(mov)
        } else {
            Err(IllegalMove::ResultToSelfInCheck)
        }
    }

    pub fn is_move_legal(
        &mut self,
        from: HexVector,
        to: HexVector,
        color: Color,
        promote_to: Option<PieceKind>,
    ) -> Result<MaybePromoteMove, IllegalMove> {
        if from.mag() > 5 {
            return Err(IllegalMove::SrcOutOfBound(from));
        } else if to.mag() > 5 {
            return Err(IllegalMove::DestOutOfBound(to));
        }
        let piece = self
            .get_piece_at(from)
            .ok_or(IllegalMove::SrcPieceNotPresent(from))?;
        if piece.color != color {
            return Err(IllegalMove::SrcPieceNotYours(piece, from));
        }
        let movement = to - from;
        if !piece.is_vector_valid(movement) {
            return Err(IllegalMove::InvalidMovement(piece, movement));
        }
        self.can_go_from_to(piece, from, to, promote_to)
    }

    pub fn get_legal_moves(&mut self) -> HashMap<HexVector, HashSet<MaybePromoteMove>> {
        let mut legal_moves: HashMap<HexVector, HashSet<MaybePromoteMove>> = HashMap::new();

        for (from, piece) in self.get_current_player_pieces().collect::<Vec<_>>() {
            for to in piece.get_moves_from(from) {
                if let Ok(mov) = self.can_go_from_to(piece, from, to, None) {
                    legal_moves.entry(from).or_default().insert(mov);
                }
            }
        }

        legal_moves
    }

    pub fn play_move(
        &mut self,
        from: HexVector,
        to: HexVector,
        promote_to: Option<PieceKind>,
    ) -> Result<Option<CanPromoteMove>, IllegalMove> {
        match self.is_move_legal(from, to, self.current_color_turn, promote_to)? {
            MaybePromoteMove::Move(mov) => {
                self.unchecked_move(mov);
                Ok(None)
            }
            MaybePromoteMove::CanPromote(mov) => Ok(Some(mov)),
        }
    }

    pub fn is_in_check(&self, color: Color) -> bool {
        let find_fn = |(_, piece): &(HexVector, Piece)| -> bool {
            *piece == Piece::new(color, PieceKind::King)
        };

        let Some((king_pos, _)) = self
            .map
            .iter()
            .filter_map(|(pos, piece)| piece.map(|piece| (pos, piece)))
            .find(find_fn) else {
                panic!("something went wrong: can't find {:?} king", color)
            };

        // instead of going through every opponent piece and check if they have line of sight on the king, start from king and check every line of sight.

        // check pawns (vectors from the king pos)
        for pos_to_check in Piece::new(color, PieceKind::Pawn).get_moves_from(king_pos) {
            if let Some(piece) = self.get_piece_at(pos_to_check) {
                if piece.color == color {
                    continue;
                }
                // we check for every piece that would check if in the pawn position for optimisations
                // we do check for king cause this function is used to decide if a move is legal or not,
                // and moving the king next to another king is illegal and would be cheked by this function
                if let PieceKind::Bishop
                | PieceKind::King
                | PieceKind::Pawn
                | PieceKind::Queen
                | PieceKind::OriginalPawn = piece.kind
                {
                    return true;
                }
            }
        }

        let map_fn = |piece_mov: &PieceMove| {
            [
                piece_mov.vector,
                piece_mov.vector.rotate_anti(),
                piece_mov.vector.rotate_clock(),
            ]
        };

        // checks diagonals (bishop and queen)
        for vector in PieceKind::BISHOP_MOVES.iter().flat_map(map_fn) {
            for k in 1.. {
                let pos_to_check = vector * k + king_pos;
                if pos_to_check.mag() > 5 {
                    break;
                }
                if let Some(piece) = self.get_piece_at(pos_to_check) {
                    if piece.color == color {
                        break;
                    }
                    if let PieceKind::Bishop | PieceKind::Queen = piece.kind {
                        return true;
                    }
                    break;
                }
            }

            for k in 1.. {
                let pos_to_check = -vector * k + king_pos;
                if pos_to_check.mag() > 5 {
                    break;
                }
                if let Some(piece) = self.get_piece_at(pos_to_check) {
                    if piece.color == color {
                        break;
                    }
                    if let PieceKind::Bishop | PieceKind::Queen = piece.kind {
                        return true;
                    }
                    break;
                }
            }
        }

        // checks axes (rook and queen)
        for vector in PieceKind::ROOK_MOVES.iter().flat_map(map_fn) {
            for k in 1.. {
                let pos_to_check = vector * k + king_pos;
                if pos_to_check.mag() > 5 {
                    break;
                }
                if let Some(piece) = self.get_piece_at(pos_to_check) {
                    if piece.color == color {
                        break;
                    }
                    if let PieceKind::Rook | PieceKind::Queen = piece.kind {
                        return true;
                    }
                    break;
                }
            }

            for k in 1.. {
                let pos_to_check = -vector * k + king_pos;
                if pos_to_check.mag() > 5 {
                    break;
                }
                if let Some(piece) = self.get_piece_at(pos_to_check) {
                    if piece.color == color {
                        break;
                    }
                    if let PieceKind::Rook | PieceKind::Queen = piece.kind {
                        return true;
                    }
                    break;
                }
            }
        }

        // checks knight
        for pos_to_check in Piece::new(color, PieceKind::Knight).get_moves_from(king_pos) {
            if let Some(piece) = self.get_piece_at(pos_to_check) {
                if piece.color == color {
                    continue;
                }
                if let PieceKind::Knight = piece.kind {
                    return true;
                }
            }
        }

        // lastly check for the other king pos
        for pos_to_check in Piece::new(color, PieceKind::King).get_moves_from(king_pos) {
            if let Some(piece) = self.get_piece_at(pos_to_check) {
                if piece.color == color {
                    continue;
                }
                if let PieceKind::King = piece.kind {
                    return true;
                }
            }
        }

        false
    }

    fn check_move_is_not_check(&mut self, mov: MaybePromoteMove) -> bool {
        let color = self.current_color_turn;
        self.unchecked_move(mov.promote_unchecked(PieceKind::Queen));
        let can = !self.is_in_check(color);
        self.back_one_turn();
        can
    }

    pub(crate) fn unchecked_move(&mut self, mov: Move) {
        if let Some(from) = self.map.get_mut(mov.from) {
            from.take();
        };

        if let Some((_, take_at)) = mov.take {
            if let Some(take) = self.map.get_mut(take_at) {
                take.take();
            };
        }

        if let Some(to) = self.map.get_mut(mov.to) {
            let piece = Piece::new(mov.original_piece.color, mov.becomes);
            to.replace(piece);
        }

        self.history.push(mov);
        self.current_color_turn = !self.current_color_turn;
    }

    pub fn back_one_turn(&mut self) {
        let Some(last_move) = self.history.pop() else {
            return;
        };

        if let Some(from) = self.map.get_mut(last_move.from) {
            from.replace(last_move.original_piece);
        }

        if let Some(to) = self.map.get_mut(last_move.to) {
            *to = None;
        }

        if let Some((take_piece, take_at)) = last_move.take {
            if let Some(take) = self.map.get_mut(take_at) {
                take.replace(take_piece);
            };
        }

        self.current_color_turn = !self.current_color_turn
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test() {
        let mut b1 = Board::new();

        let b2 = b1.clone();

        assert_eq!(b1, b2);

        let legal_moves = b1.get_legal_moves();

        let (_, movs) = legal_moves.iter().next().unwrap();
        let mov = movs.iter().next().copied().unwrap();

        assert!(b1.play_move(mov.from(), mov.to(), None).unwrap().is_none());

        assert_ne!(b1, b2);

        b1.back_one_turn();

        assert_eq!(b1, b2);
    }

    #[test]
    fn test_check() {
        let mut board = Board::new();

        assert!(!board.is_in_check(Color::White));
        assert!(!board.is_in_check(Color::Black));

        assert!(board
            .play_move(
                HexVector::new_axial(1, 1),
                HexVector::new_axial(1, -1),
                None,
            )
            .unwrap()
            .is_none());

        assert!(!board.is_in_check(Color::White));
        assert!(!board.is_in_check(Color::Black));

        assert!(board
            .play_move(
                HexVector::new_axial(0, -1),
                HexVector::new_axial(0, 0),
                None,
            )
            .unwrap()
            .is_none());

        assert!(!board.is_in_check(Color::White));
        assert!(!board.is_in_check(Color::Black));

        assert!(board
            .play_move(
                HexVector::new_axial(0, 3),
                HexVector::new_axial(3, -3),
                None,
            )
            .unwrap()
            .is_none());

        assert!(!board.is_in_check(Color::White));
        assert!(board.is_in_check(Color::Black));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_ser_de() {
        let board = Board::new();

        let str = serde_json::to_string_pretty(&board).unwrap();

        let de_board: Board = serde_json::from_str(&str).unwrap();

        assert_eq!(board, de_board);
    }

    #[cfg(feature = "serde")]
    #[ignore = "Just output the json of a serialiazed default board"]
    #[test]
    fn result_serialization() {
        let board = Board::new();

        let str = serde_json::to_string_pretty(&board).unwrap();

        println!("{}", str);
    }
}
