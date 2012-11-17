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

  test("possible movements from initial positions") {
    assert(Bishop(Color.white).moves((C, 1), initialState) === List())

    assert(Rook(Color.white).moves((A, 1), initialState) === List())

    assert(Knight(Color.white).moves((B, 1), initialState) === List(Pos(3,3), Pos(1,3)))

    assert(Pawn(Color.white).moves((A, 2), initialState) === List(Pos(1,3), Pos(1,4)))

    //TODO: other pieces
  }
}