package ru.zconstz.chess

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import ru.zconstz.chess.GameDef._
import ru.zconstz.chess.GameDef.King
import ru.zconstz.chess.GameDef.Rook
import ru.zconstz.chess.GameDef.Queen
import ru.zconstz.chess.GameDef.Knight
import ru.zconstz.chess.GameDef.Place
import ru.zconstz.chess.GameDef.Pawn
import ru.zconstz.chess.GameDef.Bishop

@RunWith(classOf[JUnitRunner])
class PieceMovementsSuite extends FunSuite {

  test("possible movements from initial state") {
    def testInitialState(color: Color.Value) {
      val baseNumber = if (color == Color.white) 1 else 8
      val forward = if (color == Color.white) (a: Int, b: Int) => a + b else (a: Int, b: Int) => a - b
      assert((
        Rook(color).moves((A, baseNumber), boardAtStart) ++
          Bishop(color).moves((C, baseNumber), boardAtStart) ++
          Queen(color).moves((D, baseNumber), boardAtStart) ++
          King(color).moves((E, baseNumber), boardAtStart) ++
          Bishop(color).moves((F, baseNumber), boardAtStart) ++
          Rook(color).moves((H, baseNumber), boardAtStart)).toSet === Set())

      val knightTargetNumber = forward(baseNumber, 2)
      assert(Knight(color).moves((G, baseNumber), boardAtStart).toSet ===
        Set(Place(F, knightTargetNumber), Place(H, knightTargetNumber)))
      assert(Knight(color).moves((B, baseNumber), boardAtStart).toSet ===
        Set(Place(A, knightTargetNumber), Place(C, knightTargetNumber)))

      assert((A to H).map(
        (letter: Int) => Pawn(color).moves((letter, forward(baseNumber, 1)), boardAtStart).toSet ===
          Set[Place]((letter, forward(baseNumber, 2)), (letter, forward(baseNumber, 3)))).reduce((a, b) => a.map(_ + b)))
    }
    testInitialState(Color.white)
    testInitialState(Color.black)
  }

  test("Queen in the center of the board") {
    def testQueenMoves(color: Color.Value): Option[String] = {
      Queen(color).moves((D, 4), Map(Place(D, 4) -> Queen(color))).toSet ===
        Set[Place](
          (D, 1), (D, 2), (D, 3), (D, 5), (D, 6), (D, 7), (D, 8),
          (A, 4), (B, 4), (C, 4), (E, 4), (F, 4), (G, 4), (H, 4),
          (A, 1), (B, 2), (C, 3), (E, 5), (F, 6), (G, 7), (H, 8),
          (G, 1), (F, 2), (E, 3), (C, 5), (B, 6), (A, 7)
        )
    }
    assert(testQueenMoves(Color.white))
    assert(testQueenMoves(Color.black))
  }


  def wKingMovesNoCastling(color: Color.Value) = Set[Place](
    (E, baseNextNumber(color)), (D, baseNumber(color)), (F, baseNumber(color)),
    (D, baseNextNumber(color)), (F, baseNextNumber(color)))

  test("King's moves including the Castling: king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(H, bn) -> Rook(color))).toSet ===
        wKingMovesNoCastling(color) + Place(G, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(A, bn) -> Rook(color))).toSet ===
        wKingMovesNoCastling(color) + Place(C, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and both rooks") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(H, bn) -> Rook(color),
        Place(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) + Place(G, bn) + Place(C, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and no rooks") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: bishop between king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(F, bn) -> Bishop(color),
        Place(H, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) - Place(F, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: knight between king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(G, bn) -> Knight(color),
        Place(H, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: queen between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(D, bn) -> Queen(color),
        Place(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) - Place(D, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: bishop between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(C, bn) -> Bishop(color),
        Place(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: knight between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Place(E, bn) -> King(color), Place(B, bn) -> Knight(color),
        Place(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king in the center") {
    def validFor(color: Color.Value): Option[String] = {
      King(color).moves((E, 4), Map(Place(E, 4) -> King(color))).toSet ===
        Set[Place]((E, 3), (E, 5), (D, 4), (F, 4), (D, 5), (F, 5), (D, 3), (F, 3))
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }

  test("Rook's moves: left rook with king") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      Rook(color).moves((A, bn), Map(Place(A, bn) -> Rook(color), Place(E, bn) -> King(color))).toSet ===
        Set[Place](
          (A, 2), (A, 3), (A, 4), (A, 5), (A, 6), (A, 7), (A, baseNumber(Color.opposite(color))),
          (B, bn), (C, bn), (D, bn)
        )
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("Rook's moves: right rook with king") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      Rook(color).moves((H, bn), Map(Place(H, bn) -> Rook(color), Place(E, bn) -> King(color))).toSet ===
        Set[Place](
          (H, 2), (H, 3), (H, 4), (H, 5), (H, 6), (H, 7), (H, baseNumber(Color.opposite(color))),
          (F, bn), (G, bn)
        )
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("Rook's moves: rook in the center") {
    def validFor(color: Color.Value): Option[String] = {
      Rook(color).moves((E, 4), Map(Place(E, 4) -> Rook(color))).toSet ===
        Set[Place](
          (E, 1), (E, 2), (E, 3), (E, 5), (E, 6), (E, 7), (E, 8),
          (A, 4), (B, 4), (C, 4), (D, 4), (F, 4), (G, 4), (H, 4)
        )
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }

  test("Bishop's moves") {
    def validFor(color: Color.Value): Option[String] = {
      Bishop(color).moves((E, 4), Map(Place(E, 4) -> Bishop(color))).toSet ===
        Set[Place](
          (F, 5), (G, 6), (H, 7), (A, 8), (B, 7), (C, 6), (D, 5),
          (F, 3), (G, 2), (H, 1), (B, 1), (C, 2), (D, 3)
        )
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }

  test("Knight's moves") {
    def validFor(color: Color.Value): Option[String] = {
      Knight(color).moves((E, 4), Map(Place(E, 4) -> Knight(color))).toSet ===
        Set[Place]((F, 6), (G, 5), (D, 6), (C, 5), (F, 2), (G, 3), (D, 2), (C, 3))
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }

  test("White pawn at starting position") {
    Pawn(Color.white).moves((E, 2),
      Map(Place(E, 2) -> Pawn(Color.white), Place(D, 3) -> Knight(Color.black),
        Place(F, 3) -> Knight(Color.black))).toSet === Set[Place]((E, 3), (E, 4), (D, 3), (F, 3))
  }

  test("White pawn not in starting position") {
    Pawn(Color.white).moves((E, 3),
      Map(Place(E, 3) -> Pawn(Color.white), Place(D, 4) -> Knight(Color.black),
        Place(F, 4) -> Knight(Color.black))).toSet === Set[Place]((E, 4), (D, 4), (F, 4))
  }

  test("White pawn not in starting position with no enemies around") {
    Pawn(Color.white).moves((E, 5), Map(Place(E, 3) -> Pawn(Color.white))).toSet === Set[Place]((E, 6))
  }

  test("Black pawn at starting position") {
    assert(Pawn(Color.black).moves((E, 7),
      Map(Place(E, 7) -> Pawn(Color.black), Place(D, 6) -> Knight(Color.white),
        Place(F, 6) -> Knight(Color.white))).toSet === Set[Place]((E, 6), (E, 5), (D, 6), (F, 6))
    )
  }

  test("Black pawn not in starting position") {
    Pawn(Color.black).moves((E, 6),
      Map(Place(E, 6) -> Pawn(Color.black), Place(D, 7) -> Knight(Color.white),
        Place(F, 7) -> Knight(Color.white))).toSet === Set[Place]((E, 7), (D, 7), (F, 7))
  }

  test("Black pawn not in starting position with no enemies around") {
    Pawn(Color.black).moves((E, 4), Map(Place(E, 4) -> Pawn(Color.white))).toSet === Set[Place]((E, 3))
  }
}