package simulations

import math.random
import scala.util.Random._

//import scala.util.Random._


class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25
  }

  import SimConfig._

  val persons: List[Person] = (1 to population).map(i => {
    val person = new Person(i)
    if (i <= population * prevalenceRate) person.infect
    afterDelay(1 + randomBelow(4))(person.move)
    person
  }).toList

  def randomDraw(rate: Double) = randomBelow(100) - rate * 100 < 0
  def personsInRoom(row: Int, col: Int) = persons.filter(p => p.row == row && p.col == col)
  def isRoomInfected(row: Int, col: Int) = personsInRoom(row, col).count(_.infected) > 0

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def visiblyInfected = sick || dead
    override def toString = s"Person($id)"

    private val MOVES = List((-1, 0), (1, 0), (0, -1), (0, 1))
    private def availableMoves = MOVES.map({ case (dr, dc) => (
      (row + dr + roomRows) % roomRows,
      (col + dc + roomColumns) % roomColumns)
    }).filterNot({ case (row, col) =>
      personsInRoom(row, col).exists(_.visiblyInfected)
    })

    def move: Unit = {
      if (!dead) {
        // Evaluate a position
        shuffle(availableMoves) match {
          case (nr, nc) :: xs =>
            // Set the new position
            row = nr
            col = nc
            // Does an infection occur?
            if (!infected && isRoomInfected(row, col) && randomDraw(transRate)) infect
          case _ => // Nothing happen
        }
        // Schedule the next move
        afterDelay(1 + randomBelow(4))(move)
      }
    }

    def infect: Unit = {
      infected = true
      afterDelay(incubationTime)({if (!dead) sick = true})
      afterDelay(dieTime)({
        if (dead || randomDraw(dieRate)) {
          dead = true
        } else {
          afterDelay(immuneTime - dieTime)({immune = true; sick = false})
          afterDelay(healTime - dieTime)({infected = false; immune = false})
        }
      })
    }

  }

}
