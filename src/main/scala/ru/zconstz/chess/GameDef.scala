package ru.zconstz.chess


object GameDef {

  val A = 1
  val B = 2
  val C = 3
  val D = 4
  val E = 5
  val F = 6
  val G = 7
  val H = 8

  object Color extends Enumeration {
    val white, black = Value

    def opposite(color: Color.Value) = if (color == white) black else white
  }

  import Color._

  implicit def tuple2Pos(tuple: (Int, Int)): Place = new Place(tuple._1, tuple._2)

  case class Place(letter: Int, number: Int) {
    lazy val onBoard = (A to H).contains(letter) && (1 to 8).contains(number)

    implicit def pos2List(pos: Place): List[Int] = List(pos.letter, pos.number)

    def +(direction: Direction): Place = {
      val newMove = this.zip(direction).map {
        case (a, b) => a + b
      }
      Place(newMove.head, newMove.tail.head)
    }

    override def toString = "Place(" + ('A' + letter - 1).asInstanceOf[Char] + ", " + number + ")"
  }

  def baseNumber(color: Color.Value): Int = if (color == white) 1 else 8
  def baseNextNumber(color: Color.Value): Int = if (color == white) 2 else 7

  type GameBoard = Map[Place, Piece]

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

    def moves(pos: Place, gameBoard: GameBoard): List[Place] = movesAux(pos, gameBoard, directions)

    protected def movesAux(pos: Place, gameBoard: GameBoard, directions: List[Direction]): List[Place] = {
      val more = for {
        deltaMove <- directions
        newPosition = pos + deltaMove
        if validPos(newPosition, gameBoard)
      } yield if (infinity) (newPosition :: movesAux(newPosition, gameBoard, List(deltaMove))) else List(newPosition)
      more.flatten
    }

    protected def validPos(pos: Place, gameBoard: GameBoard): Boolean =
      (!gameBoard.contains(pos) || gameBoard(pos).color != color) &&
        pos.onBoard
  }

  case class King(color: Color.Value) extends Piece {
    val infinity = false
    val directions = verticalDirections ::: horizontalDirections ::: diagonalDirections

    def castlingMoves(pos: Place, gameBoard: GameBoard): List[Place] = {
      def isRookOfSameColor(piece: Option[Piece]): Boolean = piece match {
        case Some(Rook(c)) if c == color => true
        case _ => false
      }
      (if (isRookOfSameColor(gameBoard.get((H, baseNumber(color)))) &&
        !gameBoard.contains((F, baseNumber(color))) &&
        !gameBoard.contains((G, baseNumber(color)))) List(Place(G, baseNumber(color)))
      else Nil) ++
        (if (isRookOfSameColor(gameBoard.get((A, baseNumber(color)))) &&
          (B to D).foldLeft(true)((prev: Boolean, letter: Int) => prev && !gameBoard.contains((letter, baseNumber(color)))))
          List(Place(C, baseNumber(color)))
        else Nil)
    }

    override def moves(pos: Place, gameBoard: GameBoard) = super.moves(pos, gameBoard) ++
      castlingMoves(pos, gameBoard)
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

    private def isStartPosition(pos: Place): Boolean = pos.number == baseNextNumber(color)

    override def moves(pos: Place, gameBoard: GameBoard) = {
      def fightMove(pos: Place, direction: Direction) = {
        val newPos = pos + direction
        if (gameBoard.contains(newPos) && validPos(newPos, gameBoard)) List(newPos) else Nil
      }

      val fightingMoves =
        if (color == Color.white) fightMove(pos, NorthEast) ++ fightMove(pos, NorthWest)
        else fightMove(pos, SouthEast) ++ fightMove(pos, SouthWest)

      val friendlyMoves = super.moves(pos, gameBoard) ++ (if (isStartPosition(pos))
        super.moves(pos + direction, gameBoard)
      else Nil)

      friendlyMoves ++ fightingMoves
    }
  }

  val initialState: GameBoard =
    Map(
      Place(A, 1) -> Rook(white), Place(B, 1) -> Knight(white), Place(C, 1) -> Bishop(white), Place(D, 1) -> Queen(white),
      Place(E, 1) -> King(white), Place(F, 1) -> Bishop(white), Place(G, 1) -> Knight(white), Place(H, 1) -> Rook(white),
      Place(A, 2) -> Pawn(white), Place(B, 2) -> Pawn(white), Place(C, 2) -> Pawn(white), Place(D, 2) -> Pawn(white),
      Place(E, 2) -> Pawn(white), Place(F, 2) -> Pawn(white), Place(G, 2) -> Pawn(white), Place(H, 2) -> Pawn(white),
      Place(A, 7) -> Pawn(black), Place(B, 7) -> Pawn(black), Place(C, 7) -> Pawn(black), Place(D, 7) -> Pawn(black),
      Place(E, 7) -> Pawn(black), Place(F, 7) -> Pawn(black), Place(G, 7) -> Pawn(black), Place(H, 7) -> Pawn(black),
      Place(A, 8) -> Rook(black), Place(B, 8) -> Knight(black), Place(C, 8) -> Bishop(black), Place(D, 8) -> Queen(black),
      Place(E, 8) -> King(black), Place(F, 8) -> Bishop(black), Place(G, 8) -> Knight(black), Place(H, 8) -> Rook(black)
    )
}

