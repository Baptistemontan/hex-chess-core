use std::collections::{HashMap, HashSet};

use crate::{
    hex_coord::{HexMap, HexVector},
    mov::{CanPromoteMove, FromHistoryError, IllegalMove, MaybePromoteMove, Move, MoveBecomes},
    piece::{Color, Piece, PieceKind, PieceMove},
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct History {
    /// past_moves are in chronological order
    past_moves: Vec<Move>,
    /// next_moves are in reverse chronological order
    next_moves: Vec<Move>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Board {
    map: HexMap<Option<Piece>>,
    /// history of moves leading to the current map,
    history: History,
}

#[cfg(feature = "serde")]
impl serde::Serialize for Board {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.history.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Board {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let history = History::deserialize(deserializer)?;
        Board::from_history(history).map_err(D::Error::custom)
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameEnd {
    Win(Color),
    /// Draw, by lack of mat
    Draw,
    /// Glinskli's chess variant as a winner in case of stalemate, but win 3/4 of points and the loser 1/4 of points.
    Stalemate {
        winner: Color,
    },
}

impl History {
    const fn new() -> Self {
        History {
            past_moves: Vec::new(),
            next_moves: Vec::new(),
        }
    }

    /// get the turn at the moment the history is at
    fn local_turn(&self) -> Color {
        self.past_moves
            .last()
            .map(|mov| !mov.original_piece.color)
            .unwrap_or(Color::White)
    }

    /// get the next turn regardless of where the history is at
    fn turn(&self) -> Color {
        self.next_moves
            .first()
            .map(|mov| !mov.original_piece.color)
            .unwrap_or_else(|| self.local_turn())
    }
}

impl Board {
    /// return the piece (or lack of) that should be at the given position at the start of the game
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

    /// check if two position are equal, only check past moves.
    pub fn is_position_eq(&self, other: &Self) -> bool {
        self.map == other.map && self.history.local_turn() == other.history.local_turn()
    }

    /// Series of moves that lead to the current board
    pub fn get_played_moves(&self) -> &[Move] {
        &self.history.past_moves
    }

    /// Next moves in the history (in reverse chronological order)
    pub fn get_next_moves(&self) -> &[Move] {
        &self.history.next_moves
    }

    /// if the game already started
    pub fn has_started(&self) -> bool {
        !self.history.past_moves.is_empty() || !self.history.next_moves.is_empty()
    }

    /// get the last played move, relative to history
    pub fn get_last_played_move(&self) -> Option<Move> {
        self.history.past_moves.last().copied()
    }

    /// Create a Board based on the given history
    /// Check all moves, even next moves, but roll them back to a board relative to the history
    pub fn from_history(history: History) -> Result<Self, FromHistoryError> {
        let next_moves_len = history.next_moves.len();

        let past_moves_iter = history.past_moves.into_iter();
        let next_moves_iter = history.next_moves.into_iter().rev();

        // all moves in chronological order
        let moves_iter = past_moves_iter.chain(next_moves_iter).enumerate();

        let mut board = Board::new();

        for (i, mov) in moves_iter {
            let promote_to = match mov.becomes {
                Some(MoveBecomes::Promote(kind)) => Some(kind),
                _ => None,
            };

            let res = board.play_move(mov.from, mov.to, promote_to);

            match res {
                Ok(None) => {}
                Ok(Some(promote_move)) => {
                    return Err(FromHistoryError::PromotionError(i, promote_move))
                }
                Err(illegal_move) => return Err(FromHistoryError::IllegalMove(i, illegal_move)),
            }
        }

        // rollback moves
        for _ in 0..next_moves_len {
            board.back_one_turn();
        }

        Ok(board)
    }

    /// create the default board
    fn get_default_map() -> HexMap<Option<Piece>> {
        HexMap::new_with_init(5, Self::original_piece_at)
    }

    pub fn new() -> Self {
        Board {
            map: Self::get_default_map(),
            history: History::new(),
        }
    }

    /// Return which player turn it is regardless of where the history is at
    pub fn get_player_turn(&self) -> Color {
        self.history.turn()
    }

    /// Return which player turn it is relative to the history
    pub fn get_local_turn(&self) -> Color {
        self.history.local_turn()
    }

    pub fn get_current_player_pieces(&self) -> impl Iterator<Item = (HexVector, Piece)> + '_ {
        self.get_player_pieces_for(self.get_local_turn())
    }

    pub fn get_player_pieces_for(
        &self,
        color: Color,
    ) -> impl Iterator<Item = (HexVector, Piece)> + '_ {
        let filter_fn = move |(coord, piece): (HexVector, &Option<Piece>)| match piece {
            Some(piece) if piece.color == color => Some((coord, *piece)),
            _ => None,
        };
        self.map.iter().filter_map(filter_fn)
    }

    pub fn get_all_taken_pieces(&self) -> impl Iterator<Item = Piece> + '_ {
        self.history
            .past_moves
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
                Some(MoveBecomes::Promote(promote_to)),
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
        color: Color,
    ) -> Result<MaybePromoteMove, IllegalMove> {
        let take_piece = self.get_piece_at(to).map(|p| (p, to));

        match take_piece {
            Some((dest_piece, _)) if dest_piece.color == piece.color => {
                return Err(IllegalMove::DestPieceYours(dest_piece, to))
            }
            _ => {}
        }

        let default_move = MaybePromoteMove::new_move(from, to, piece, None, take_piece);

        let vector = to - from;
        // println!("normalized vec: {:?}", normalized_vec);

        let mov: MaybePromoteMove = match (
            piece.kind,
            piece.color,
            take_piece,
            self.history.past_moves.last(),
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
                    let becomes = (piece.kind == PieceKind::OriginalPawn)
                        .then_some(MoveBecomes::Else(PieceKind::Pawn));
                    Ok(MaybePromoteMove::new_move(from, to, piece, becomes, None))
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
                    Some(MoveBecomes::Else(PieceKind::Pawn)),
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
                    let becomes = (piece.kind == PieceKind::OriginalPawn)
                        .then_some(MoveBecomes::Else(PieceKind::Pawn));
                    Ok(MaybePromoteMove::new_move(from, to, piece, becomes, None))
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
                    Some(MoveBecomes::Else(PieceKind::Pawn)),
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
                        from, to, piece, None, take_piece,
                    ))
                } else {
                    // else it becomes a normal pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        Some(MoveBecomes::Else(PieceKind::Pawn)),
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
                        from, to, piece, None, take_piece,
                    ))
                } else {
                    // else it becomes a normal pawn
                    Ok(MaybePromoteMove::new_move(
                        from,
                        to,
                        piece,
                        Some(MoveBecomes::Else(PieceKind::Pawn)),
                        take_piece,
                    ))
                }
            }
            // white pawn en passant
            (PieceKind::Pawn, Color::White, None, Some(last_move), None)
                if last_move.original_piece.kind == PieceKind::OriginalPawn
                    && (last_move.from.distance(last_move.to) == 2)
                    && ((vector == HexVector::new_axial(-1, 0)
                        && last_move.to == (from + HexVector::new_axial(-1, 1)))
                        || (vector == HexVector::new_axial(1, -1)
                            && last_move.to == (from + HexVector::new_axial(1, 0)))) =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    None,
                    Some((last_move.original_piece, last_move.to)),
                ))
            }
            // black pawn en passant
            (PieceKind::Pawn, Color::Black, None, Some(last_move), None)
                if last_move.original_piece.kind == PieceKind::OriginalPawn
                    && (last_move.from.distance(last_move.to) == 2)
                    && ((vector == HexVector::new_axial(-1, 1)
                        && last_move.to == (from + HexVector::new_axial(-1, 0)))
                        || (vector == HexVector::new_axial(1, 0)
                            && last_move.to == (from + HexVector::new_axial(1, -1)))) =>
            {
                Ok(MaybePromoteMove::new_move(
                    from,
                    to,
                    piece,
                    None,
                    Some((last_move.original_piece, last_move.to)),
                ))
            }

            // invalid pawn move
            (PieceKind::Pawn | PieceKind::OriginalPawn, _, _, _, None) => {
                Err(IllegalMove::InvalidMovement(piece, vector))
            }
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
        if self.check_move_is_not_check_for(mov, color) {
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
        if color != self.get_local_turn() {
            return Err(IllegalMove::NotYourTurn);
        }
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
        self.can_go_from_to(piece, from, to, promote_to, color)
    }

    /// Return the legal moves for the current player
    pub fn get_legal_moves(&mut self) -> HashMap<HexVector, HashSet<MaybePromoteMove>> {
        self.get_legal_moves_for(self.get_local_turn())
    }

    /// Return the legal moves for the given player
    pub fn get_legal_moves_for(
        &mut self,
        color: Color,
    ) -> HashMap<HexVector, HashSet<MaybePromoteMove>> {
        let mut legal_moves: HashMap<HexVector, HashSet<MaybePromoteMove>> = HashMap::new();

        for (from, piece) in self.get_player_pieces_for(color).collect::<Vec<_>>() {
            for to in piece.get_moves_from(from) {
                if let Ok(mov) = self.can_go_from_to(piece, from, to, None, color) {
                    legal_moves.entry(from).or_default().insert(mov);
                }
            }
        }

        legal_moves
    }

    pub fn branch_history(&self) -> Board {
        let map = self.map.clone();
        let past_moves = self.history.past_moves.clone();
        let history = History {
            past_moves,
            next_moves: vec![],
        };
        Board { map, history }
    }

    pub fn play_move(
        &mut self,
        from: HexVector,
        to: HexVector,
        promote_to: Option<PieceKind>,
    ) -> Result<Option<CanPromoteMove>, IllegalMove> {
        match self.is_move_legal(from, to, self.get_local_turn(), promote_to)? {
            MaybePromoteMove::Move(mov) => {
                self.history.next_moves.clear();
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
            .find(find_fn)
        else {
            panic!("something went wrong: can't find {:?} king", color)
        };

        // instead of going through every opponent piece and check if they have line of sight on the king, start from king and check every line of sight.

        let pawn_moves = match color {
            Color::White => [
                HexVector::new_axial(-1, 0) + king_pos,
                HexVector::new_axial(1, -1) + king_pos,
            ],
            Color::Black => [
                HexVector::new_axial(-1, 1) + king_pos,
                HexVector::new_axial(1, 0) + king_pos,
            ],
        };

        // check pawns (vectors from the king pos)
        for pos_to_check in pawn_moves {
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

    fn check_move_is_not_check_for(&mut self, mov: MaybePromoteMove, color: Color) -> bool {
        self.unchecked_move(mov.promote_unchecked(PieceKind::Queen));
        let can = !self.is_in_check(color);
        self.back_one_turn_untracked();
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
            let kind = match mov.becomes {
                None => mov.original_piece.kind,
                Some(MoveBecomes::Else(kind) | MoveBecomes::Promote(kind)) => kind,
            };
            let piece = Piece::new(mov.original_piece.color, kind);
            to.replace(piece);
        }

        self.history.past_moves.push(mov);
    }

    pub fn back_one_turn_untracked(&mut self) -> Option<Move> {
        let last_move = self.history.past_moves.pop()?;

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

        Some(last_move)
    }

    pub fn back_one_turn(&mut self) {
        if let Some(last_move) = self.back_one_turn_untracked() {
            self.history.next_moves.push(last_move);
        }
    }

    pub fn advance_history(&mut self) {
        if let Some(next_move) = self.history.next_moves.pop() {
            self.unchecked_move(next_move);
        }
    }

    pub fn unwind_history(&mut self) {
        let next_moves = std::mem::take(&mut self.history.next_moves);
        for mov in next_moves.into_iter().rev() {
            self.unchecked_move(mov)
        }
    }

    fn get_remaining_pieces_for(&self, color: Color) -> impl Iterator<Item = PieceKind> + '_ {
        self.map
            .iter()
            .filter_map(|(_, piece)| piece.as_ref())
            .filter(move |piece| piece.color == color)
            .map(|piece| piece.kind)
    }

    fn has_enough_mat(&self, color: Color) -> bool {
        // TODO: verify those informations
        let mut bishop_count = 0;
        let mut knight_count = 0;

        // can mate with a rook and a king, so a queen can and a pawn potentially can.

        for kind in self.get_remaining_pieces_for(color) {
            match kind {
                PieceKind::OriginalPawn | PieceKind::Pawn | PieceKind::Queen | PieceKind::Rook => {
                    return true
                }
                PieceKind::Knight => bishop_count += 1,
                PieceKind::Bishop => knight_count += 1,
                PieceKind::King => (),
            }
        }

        // can mate with 2 knight, a knight and a bishop, or 3 bishops

        knight_count >= 2 || knight_count >= 1 && bishop_count >= 1 || bishop_count >= 3
    }

    pub fn is_end(&mut self) -> Option<GameEnd> {
        // check if enough materials
        let enough_mat = self.has_enough_mat(Color::White) || self.has_enough_mat(Color::Black);
        if !enough_mat {
            return Some(GameEnd::Draw);
        }
        // check if has legal moves
        self.get_legal_moves().is_empty().then_some(())?;
        // check if in check
        let current_player = self.get_local_turn();
        if self.is_in_check(current_player) {
            Some(GameEnd::Win(!current_player))
        } else {
            Some(GameEnd::Stalemate {
                winner: !current_player,
            })
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn board_with_moves_and_rollback(forward_moves: usize, rollbacks: usize) -> Option<Board> {
        let mut board = Board::new();

        for _ in 0..(forward_moves + rollbacks) {
            let legal_moves = board.get_legal_moves();
            let (_, movs) = legal_moves.iter().next()?;
            let mov = movs.iter().next().copied()?;
            let promote_to = match mov {
                MaybePromoteMove::CanPromote(_) => Some(PieceKind::Queen),
                MaybePromoteMove::Move(_) => None,
            };
            board.play_move(mov.from(), mov.to(), promote_to).unwrap();
        }

        for _ in 0..rollbacks {
            board.back_one_turn();
        }

        Some(board)
    }

    #[test]
    fn test() {
        let mut b1 = Board::new();

        let b2 = b1.clone();

        assert_eq!(b1, b2);

        let legal_moves = b1.get_legal_moves();

        let (_, movs) = legal_moves.iter().next().unwrap();
        let mov = movs.iter().next().copied().unwrap();

        assert!(b1.play_move(mov.from(), mov.to(), None).unwrap().is_none());

        let b3 = b1.clone();

        assert!(!b1.is_position_eq(&b2));

        b1.back_one_turn();

        assert!(b1.is_position_eq(&b2));

        b1.advance_history();

        assert!(b1.is_position_eq(&b3));
    }

    #[test]
    fn test_illegal_original_pawn_move() {
        let mut board = Board::new();

        let err = board
            .play_move(HexVector::new_axial(3, 1), HexVector::new_axial(4, 0), None)
            .unwrap_err();

        assert_eq!(
            err,
            IllegalMove::InvalidMovement(
                Piece::new(Color::White, PieceKind::OriginalPawn),
                HexVector::new_axial(1, -1)
            )
        )
    }

    #[test]
    fn test_illegal_rook_move() {
        let mut board = Board::new();

        let err = board
            .play_move(
                HexVector::new_axial(3, 2),
                HexVector::new_axial(3, -3),
                None,
            )
            .unwrap_err();

        assert_eq!(
            err,
            IllegalMove::PieceInTheWay(
                Piece::new(Color::White, PieceKind::OriginalPawn),
                HexVector::new_axial(3, 1)
            )
        )
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
    fn test_ser_de_with_moves() {
        let board = board_with_moves_and_rollback(5, 0).unwrap();

        let str = serde_json::to_string_pretty(&board).unwrap();

        let de_board: Board = serde_json::from_str(&str).unwrap();

        assert_eq!(board, de_board);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_ser_de_with_moves_and_history_move() {
        let rollback_counts = 6;
        let mut board = board_with_moves_and_rollback(10, rollback_counts).unwrap();

        let str = serde_json::to_string_pretty(&board).unwrap();

        let mut de_board: Board = serde_json::from_str(&str).unwrap();

        assert_eq!(board, de_board);

        for _ in 0..rollback_counts {
            board.advance_history();
            de_board.advance_history();
        }

        assert_eq!(board, de_board);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_ser_de_no_moves() {
        let board = Board::new();

        let str = serde_json::to_string_pretty(&board).unwrap();

        let de_board: Board = serde_json::from_str(&str).unwrap();

        assert_eq!(board, de_board);
    }

    #[cfg(feature = "serde")]
    #[ignore = "Just output the json of a serialized default board"]
    #[test]
    fn result_serialization() {
        let board = Board::new();

        let str = serde_json::to_string_pretty(&board).unwrap();

        println!("{}", str);
    }

    #[cfg(feature = "serde")]
    #[ignore = "Just output the json of a serialized board with some moves"]
    #[test]
    fn result_serialization_with_moves() {
        let board = board_with_moves_and_rollback(10, 10);

        let str = serde_json::to_string_pretty(&board).unwrap();

        println!("{}", str);
    }
}
