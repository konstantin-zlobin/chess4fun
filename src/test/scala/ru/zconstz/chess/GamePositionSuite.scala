package ru.zconstz.chess

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import GameDef._

@RunWith(classOf[JUnitRunner])
class GamePositionSuite extends FunSuite {

  test("a weight of the initial state is zero for both sides black and white") {
    assert(GamePosition(Color.white, boardAtStart).weight === 0)
    assert(GamePosition(Color.black, boardAtStart).weight === 0)
  }

  test("a weight of the initial satate without a piece is less than the weight of the initial state") {
    assert(GamePosition(Color.white, (boardAtStart - Place(E, 2))).weight < GamePosition(Color.white, boardAtStart).weight)
    assert(GamePosition(Color.white, (boardAtStart - Place(A, 1))).weight < GamePosition(Color.white, boardAtStart).weight)
    assert(GamePosition(Color.white, (boardAtStart - Place(B, 1))).weight < GamePosition(Color.white, boardAtStart).weight)
    assert(GamePosition(Color.white, (boardAtStart - Place(C, 1))).weight < GamePosition(Color.white, boardAtStart).weight)
    assert(GamePosition(Color.white, (boardAtStart - Place(D, 1))).weight < GamePosition(Color.white, boardAtStart).weight)
    assert(GamePosition(Color.white, (boardAtStart - Place(E, 1))).weight < GamePosition(Color.white, boardAtStart).weight)

    assert(GamePosition(Color.black, (boardAtStart - Place(E, 7))).weight < GamePosition(Color.black, boardAtStart).weight)
    assert(GamePosition(Color.black, (boardAtStart - Place(A, 8))).weight < GamePosition(Color.black, boardAtStart).weight)
    assert(GamePosition(Color.black, (boardAtStart - Place(B, 8))).weight < GamePosition(Color.black, boardAtStart).weight)
    assert(GamePosition(Color.black, (boardAtStart - Place(C, 8))).weight < GamePosition(Color.black, boardAtStart).weight)
    assert(GamePosition(Color.black, (boardAtStart - Place(D, 8))).weight < GamePosition(Color.black, boardAtStart).weight)
    assert(GamePosition(Color.black, (boardAtStart - Place(E, 8))).weight < GamePosition(Color.black, boardAtStart).weight)
  }

  test("ckeck detection") {
    import Color._
    assert(new GamePosition(black, IoUtils.readGameBoard(
      """|ABCDEFGH
        |8----k---8
        |7--------7
        |6----R---6
        |5--------5
        |4--------4
        |3--------3
        |2--------2
        |1--------1
        |-ABCDEFGH""".stripMargin)).isCheck === true)
    assert(new GamePosition(black, IoUtils.readGameBoard(
      """|ABCDEFGH
        |8---PkN--8
        |7-Q------7
        |6---R----6
        |5--------5
        |4--------4
        |3--------3
        |2--------2
        |1--------1
        |-ABCDEFGH""".stripMargin)).isCheck === false)
  }

  test("next positions from a specified one") {
    import Color._
    val position = IoUtils.readGameBoard(
      """|ABCDEFGH
        |8r---k---8
        |7--------7
        |6--------6
        |5--------5
        |4pP------4
        |3--------3
        |2--------2
        |1----K--R1
        |-ABCDEFGH""".stripMargin)
    val pPos1 = IoUtils.readGameBoard(
      """|ABCDEFGH
        |8r---k---8
        |7--------7
        |6--------6
        |5--------5
        |4pP------4
        |3--------3
        |2--------2
        |1-----RK-1
        |-ABCDEFGH""".stripMargin)
    ///    assert(GamePosition(white, position).
    //      possiblePositions.find((p: GamePosition) => p.piecesPlacement == pPos1).isDefined)
  }
}
