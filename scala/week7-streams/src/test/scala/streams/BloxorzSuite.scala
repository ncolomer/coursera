package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level3 extends SolutionChecker {
    /* terrain for level 1*/
    val level =
      """---oooo---
        |---oSoo---
        |---oooo---
        |---oooo---
        |-----o----
        |-----o----
        |-----ooo--
        |-----ooo--
        |-----ooo--
        |-------o--
        |-------o--
        |---ooooo--
        |--oooooo--
        |--ooTo----
        |--oooo----""".stripMargin
    //R, U, Rx3, U, L, D, R, Ux2, Rx3, Dx3, R, U
    val optsolution = List(Right, Up, Right, Right, Right, Up, Left, Down,
      Right, Up, Up, Right, Right, Right, Down, Down, Down, Right, Up)
  }

  trait Unsolvable extends SolutionChecker {
    val level =
      """SoT""".stripMargin
    val optsolution = List()
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for unsolvable level") {
    new Unsolvable {
      assert(solution == optsolution)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level 3") {
    new Level3 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 3") {
    new Level3 {
      assert(solution.length == optsolution.length)
    }
  }

  test("finding neighbors") {
    new Level1 {
      val actual = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).toStream
      assert(actual === expected)
    }
  }

  test("avoiding circles") {
    new Level1 {
      val actual = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      )
      val expected = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).toStream
      assert(actual === expected)
    }
  }

}
