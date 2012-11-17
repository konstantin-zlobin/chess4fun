package ru.zconstz.chess

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import ru.zconstz.chess.GameDef._
import ru.zconstz.chess.GameDef.King
import ru.zconstz.chess.GameDef.Rook
import ru.zconstz.chess.GameDef.Queen
import ru.zconstz.chess.GameDef.Knight
import ru.zconstz.chess.GameDef.Pos
import ru.zconstz.chess.GameDef.Pawn
import ru.zconstz.chess.GameDef.Bishop

@RunWith(classOf[JUnitRunner])
class PieceMovementsSuite extends FunSuite {

  test("possible movements from initial state") {
    def testInitialState(color: Color.Value) {
      val baseNumber = if (color == Color.white) 1 else 8
      val forward = if (color == Color.white) (a: Int, b: Int) => a + b else (a: Int, b: Int) => a - b
      assert((
        Rook(color).moves((A, baseNumber), initialState) ++
          Bishop(color).moves((C, baseNumber), initialState) ++
          Queen(color).moves((D, baseNumber), initialState) ++
          King(color).moves((E, baseNumber), initialState) ++
          Bishop(color).moves((F, baseNumber), initialState) ++
          Rook(color).moves((H, baseNumber), initialState)).toSet === Set())

      val knightTargetNumber = forward(baseNumber, 2)
      assert(Knight(color).moves((G, baseNumber), initialState).toSet ===
        Set(Pos(F, knightTargetNumber), Pos(H, knightTargetNumber)))
      assert(Knight(color).moves((B, baseNumber), initialState).toSet ===
        Set(Pos(A, knightTargetNumber), Pos(C, knightTargetNumber)))

      assert((A to H).map(
        (letter: Int) => Pawn(color).moves((letter, forward(baseNumber, 1)), initialState).toSet ===
          Set[Pos]((letter, forward(baseNumber, 2)), (letter, forward(baseNumber, 3)))).reduce((a, b) => a.map(_ + b)))
    }
    testInitialState(Color.white)
    testInitialState(Color.black)
  }

  test("Queen in the center of the board") {
    def testQueenMoves(color: Color.Value): Option[String] = {
      Queen(color).moves((D, 4), Map(Pos(D, 4) -> Queen(color))).toSet ===
        Set[Pos](
          (D, 1), (D, 2), (D, 3), (D, 5), (D, 6), (D, 7), (D, 8),
          (A, 4), (B, 4), (C, 4), (E, 4), (F, 4), (G, 4), (H, 4),
          (A, 1), (B, 2), (C, 3), (E, 5), (F, 6), (G, 7), (H, 8),
          (G, 1), (F, 2), (E, 3), (C, 5), (B, 6), (A, 7)
        )
    }
    assert(testQueenMoves(Color.white))
    assert(testQueenMoves(Color.black))
  }


  def wKingMovesNoCastling(color: Color.Value) = Set[Pos](
    (E, baseNextNumber(color)), (D, baseNumber(color)), (F, baseNumber(color)),
    (D, baseNextNumber(color)), (F, baseNextNumber(color)))

  test("King's moves including the Castling: king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(H, bn) -> Rook(color))).toSet ===
        wKingMovesNoCastling(color) + Pos(G, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(A, bn) -> Rook(color))).toSet ===
        wKingMovesNoCastling(color) + Pos(C, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and both rooks") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(H, bn) -> Rook(color),
        Pos(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) + Pos(G, bn) + Pos(C, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king and no rooks") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: bishop between king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(F, bn) -> Bishop(color),
        Pos(H, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) - Pos(F, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: knight between king and right rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(G, bn) -> Knight(color),
        Pos(H, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: queen between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(D, bn) -> Queen(color),
        Pos(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color) - Pos(D, bn)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: bishop between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(C, bn) -> Bishop(color),
        Pos(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: knight between king and left rook") {
    def validFor(color: Color.Value): Option[String] = {
      val bn = baseNumber(color)
      King(color).moves((E, bn), Map(Pos(E, bn) -> King(color), Pos(B, bn) -> Knight(color),
        Pos(A, bn) -> Rook(color))).toSet === wKingMovesNoCastling(color)
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }
  test("King's moves including the Castling: king in the center") {
    def validFor(color: Color.Value): Option[String] = {
      King(color).moves((E, 4), Map(Pos(E, 4) -> King(color))).toSet ===
        Set[Pos]((E, 3), (E, 5), (D, 4), (F, 4), (D, 5), (F, 5), (D, 3), (F, 3))
    }
    assert(validFor(Color.white))
    assert(validFor(Color.black))
  }

  test("Rook's moves including the Castling") {
    //TODO: implement
  }


}