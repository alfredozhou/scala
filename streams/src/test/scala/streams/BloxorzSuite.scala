package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
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

  trait NoSolution extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """------
        |--Soo-
        |--oTo-
        |--ooo-
        |------""".stripMargin
  }

  trait Level6 extends SolutionChecker {
    /* terrain for level 6*/

 val level = // 524383
      """-----oooooo----
        |-----o--ooo----
        |-----o--ooooo--
        |Sooooo-----oooo
        |----ooo----ooTo
        |----ooo-----ooo
        |------o--oo----
        |------ooooo----
        |------ooooo----
        |-------ooo-----""".stripMargin

    val optsolution = List(Right, Right, Right, Down, Right, Down, Down, Right, Down, Down, Right, Up, Left, Left, Left, Up, Up, Left, Up, Up, Up, Right, Right, Right, Down, Down, Left, Up, Right, Right, Down, Right, Down, Down, Right)
  }

  test("very hard") {
    new Level6 {
      assert(solution === optsolution)
    }
  }
  
  test("No Solution Possible") {
    new NoSolution {
      assert(solution.isEmpty == true)
    }
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

  test("test isSolved") {
    new Level1 {
      assert(done(Block(Pos(4, 7), Pos(4, 7))) === true)
    }
  }

  test("test can generate legal moves") {
    new Level1 {
      val streamOfMoves = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val expected = List(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(streamOfMoves.toList === expected)
    }
  }

  test("test can generate legal moves without repeats") {
    new Level1 {
      val streamOfMoves = newNeighborsOnly(
        Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))

      val expected = List((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(streamOfMoves.toList === expected)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
