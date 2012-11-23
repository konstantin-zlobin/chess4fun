package ru.zconstz.chess

import GameDef._

case class GamePosition(color: Color.Value, piecesPlacement: GameBoard) {

  lazy val weight: Double =
    (for {
      (place, piece) <- piecesPlacement
      weight = (if (piece.color == color) 1 else -1) * (piece match {
        case King(_) => 100.0
        case Queen(_) => 8.0 //TODO: Implement dynamic weight calculation, taking into account current position of the piece
        case Rook(_) => 4.0
        case Bishop(_) => 2.0
        case Knight(_) => 2.0
        case Pawn(_) => 1.0
      })
    } yield weight).reduce(_ + _)

  lazy val isCheck: Boolean = {
    val (kingPlace, king) = piecesPlacement.find {
      case (place, King(this.color)) => true
      case _ => false
    }.get
    (for {
      (place, piece) <- piecesPlacement
      if piece.color != color
      newPlace <- piece.moves(place, piecesPlacement)
    } yield newPlace).find(p => p == kingPlace).isDefined
  }

  lazy val possiblePositions: Iterable[GamePosition] =
    for {
      (place, piece) <- piecesPlacement
      if piece.color == color
      newPlace <- piece.moves(place, piecesPlacement)
      newPosition = piece match {
        //TODO: Implement castling here
        //case King(_) =>
        //        case Pawn(_) => //TODO: Implement "en passant" and "promotion" here
        case _ => (piecesPlacement - place).updated(newPlace, piece)
      }
    } yield GamePosition(color, newPosition)
}
