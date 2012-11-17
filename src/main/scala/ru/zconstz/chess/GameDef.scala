package ru.zconstz.chess


object GameDef {

  implicit def tuple2Pos(tuple: (Int, Int)): Pos = new Pos(tuple._1, tuple._2)

  implicit def pos2List(pos: Pos): List[Int] = List(pos.letter, pos.number)

  case class Pos(letter: Int, number: Int) {
    def +(direction: Direction): Pos = {
      val newMove = this.zip(direction).map {
        case (a, b) => a + b
      }
      Pos(newMove.head, newMove.tail.head)
    }
  }

  object Color extends Enumeration {
    val white, black = Value
  }

  import Color._

  val A = 1
  val B = 2
  val C = 3
  val D = 4
  val E = 5
  val F = 6
  val G = 7
  val H = 8

  type GameBoard = Map[Pos, Piece]

  sealed case class Direction(deltaLetter: Int, deltaNumber: Int) {
    val unitDelta = (deltaLetter, deltaNumber)
  }

  implicit def direction2List(direction: Direction): List[Int] = List(direction.deltaLetter, direction.deltaNumber)

  case object North extends Direction(0, 1)

  case object South extends Direction(0, -1)

  case object West extends Direction(1, 0)

  case object East extends Direction(-1, 0)

  class DiagonalDirection(vertical: Direction, horizontal: Direction)
    extends Direction(horizontal.deltaLetter, vertical.deltaNumber)

  case object NorthWest extends DiagonalDirection(North, West)

  case object NorthEast extends DiagonalDirection(North, East)

  case object SouthWest extends DiagonalDirection(South, West)

  case object SouthEast extends DiagonalDirection(South, East)

  val diagonalDirections = List(NorthEast, NorthWest, SouthEast, SouthWest)
  val verticalDirections = List(North, South)
  val horizontalDirections = List(West, East)
  val whitePawnsDirection = North
  val blackPawnsDirection = South
  val knightDirections = List(Direction(1, 2), Direction(-1, 2), Direction(1, -2), Direction(-1, -2),
    Direction(2, 1), Direction(-2, 1), Direction(2, -1), Direction(-2, -1))


  sealed trait Piece {
    def color: Color.Value

    def infinity: Boolean

    def directions: List[Direction]

    def moves(pos: Pos, gameBoard: GameBoard): List[Pos] = movesAux(pos, gameBoard, directions)

    protected def movesAux(pos: Pos, gameBoard: GameBoard, directions: List[Direction]): List[Pos] = {
      val more = for {
        deltaMove <- directions
        newPosition = pos + deltaMove
        if validPos(newPosition, gameBoard)
      } yield if (infinity) (newPosition :: movesAux(newPosition, gameBoard, List(deltaMove))) else List(newPosition)
      more.flatten
    }

    protected def validPos(pos: Pos, gameBoard: GameBoard): Boolean =
      (!gameBoard.contains(pos) || gameBoard(pos).color != color) &&
        (A to H).contains(pos.letter) && (1 to 8).contains(pos.number)
  }

  case class King(color: Color.Value) extends Piece {
    val infinity = false
    val directions = verticalDirections ::: horizontalDirections ::: diagonalDirections
  }

  case class Queen(color: Color.Value) extends Piece {
    val infinity = true
    val directions = verticalDirections ::: horizontalDirections ::: diagonalDirections
  }

  case class Rook(color: Color.Value) extends Piece {
    val infinity = true
    val directions = verticalDirections ::: horizontalDirections
  }

  case class Bishop(color: Color.Value) extends Piece {
    val infinity = true
    val directions = diagonalDirections
  }

  case class Knight(color: Color.Value) extends Piece {
    val infinity = false
    val directions = knightDirections
  }

  case class Pawn(color: Color.Value) extends Piece {
    val infinity = false
    val direction = if (color == white) whitePawnsDirection else blackPawnsDirection
    val directions = List(direction)

    private def isStartPosition(pos: Pos): Boolean = if (color == Color.white) pos.number == 2 else pos.number == 7

    override def moves(pos: Pos, gameBoard: GameBoard) = {
      def fightMove(pos: Pos, direction: Direction) = {
        val newPos = pos + direction
        if (gameBoard.contains(newPos) && validPos(newPos, gameBoard)) List(newPos) else Nil
      }

      val fightingMoves =
        if (color == Color.white) fightMove(pos, NorthEast) ++ fightMove(pos, NorthWest)
        else fightMove(pos, SouthEast) ++ fightMove(pos, SouthWest)

      val friendlyMoves = super.moves(pos, gameBoard) ++ (if (isStartPosition(pos))
        super.moves(pos + direction, gameBoard) else Nil)

      friendlyMoves ++ fightingMoves
    }
  }

  val initialState: GameBoard =
    Map(
      Pos(A, 1) -> Rook(white), Pos(B, 1) -> Knight(white), Pos(C, 1) -> Bishop(white), Pos(D, 1) -> Queen(white),
      Pos(E, 1) -> King(white), Pos(F, 1) -> Bishop(white), Pos(G, 1) -> Knight(white), Pos(H, 1) -> Rook(white),
      Pos(A, 2) -> Pawn(white), Pos(B, 2) -> Pawn(white), Pos(C, 2) -> Pawn(white), Pos(D, 2) -> Pawn(white),
      Pos(E, 2) -> Pawn(white), Pos(F, 2) -> Pawn(white), Pos(G, 2) -> Pawn(white), Pos(H, 2) -> Pawn(white),
      Pos(A, 7) -> Pawn(black), Pos(B, 7) -> Pawn(black), Pos(C, 7) -> Pawn(black), Pos(D, 7) -> Pawn(black),
      Pos(E, 7) -> Pawn(black), Pos(F, 7) -> Pawn(black), Pos(G, 7) -> Pawn(black), Pos(H, 7) -> Pawn(black),
      Pos(A, 8) -> Rook(black), Pos(B, 8) -> Knight(black), Pos(C, 8) -> Bishop(black), Pos(D, 8) -> Queen(black),
      Pos(E, 8) -> King(black), Pos(F, 8) -> Bishop(black), Pos(G, 8) -> Knight(black), Pos(H, 8) -> Rook(black)
    )

  case class Move(from: GameBoard, to: GameBoard)

}

