package ru.zconstz.chess

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IoUtilsSuite extends FunSuite {
  test("read game board") {
    assert(IoUtils.readGameBoard(
      """|ABCDEFGH
        |8rnbqkbnr8
        |7pppppppp7
        |6--------6
        |5--------5
        |4--------4
        |3--------3
        |2PPPPPPPP2
        |1RNBQKBNR1
        |-ABCDEFGH""".stripMargin) === GameDef.boardAtStart)
  }
}
