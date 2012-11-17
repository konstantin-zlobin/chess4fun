package ru.zconstz.chess

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import ru.zconstz.chess.GameDef._
import ru.zconstz.chess.GameDef.Rook
import ru.zconstz.chess.GameDef.Knight
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

      val  knightTargetNumber = forward(baseNumber, 2)
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

//  test("King's moves including the Castling") {
//    assert(King(Color.white).moves((E, 1), Map(Pos(E, 1) -> King(Color.white), Pos(H, 1) -> Rook(Color.white)))
//      === Seq[Pos]())
//  }
}