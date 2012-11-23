package ru.zconstz.chess

object IoUtils {
  import GameDef._
  import GameDef.Color._

  def readGameBoard(str: String): GameBoard =
    (for {
      (row, rowIndex) <- str.split('\n').slice(1, 9).zipWithIndex
      (char, charIndex) <- row.slice(1, 9).zipWithIndex
      optPiece: Option[Piece] = char match {
        case 'P' => Some(Pawn(white))
        case 'R' => Some(Rook(white))
        case 'N' => Some(Knight(white))
        case 'B' => Some(Bishop(white))
        case 'Q' => Some(Queen(white))
        case 'K' => Some(King(white))
        case 'p' => Some(Pawn(black))
        case 'r' => Some(Rook(black))
        case 'n' => Some(Knight(black))
        case 'b' => Some(Bishop(black))
        case 'q' => Some(Queen(black))
        case 'k' => Some(King(black))
        case _ => None
      }
      if optPiece.isDefined
      place = Place(charIndex + 1, 8 - rowIndex)
      if place.onBoard
    } yield (place, optPiece.get)).toMap
}
