package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite {
  trait Cell{
    def isDead:Boolean
  }
  object DeadCell extends Cell{
    def isDead = true
  }

  object LivingCell extends Cell{
    def isDead = false

    def nextState(neighbors: Int) = {
        if(neighbors<2 || neighbors > 3) DeadCell
        else LivingCell
    }
  }

  test("we have a dead cell") {
    assert(DeadCell isDead, "Cell is not dead")
  }

  test("we can have a living cell") {
    assert(LivingCell.isDead === false, "Cell is not living")
  }

  test("a living cell dies when it has no neighbor") {
    assert(LivingCell.nextState(0).isDead, "Cell should have died of loneliness")
  }

  test("a living cell dies when it has one neighbor") {
    assert(LivingCell.nextState(1).isDead, "Cell should have died of loneliness")
  }

  test("a living cell stays alive when it has two neighbors") {
    assert(LivingCell.nextState(2).isDead === false, "Cell should still be alive")
  }

  test("a living cell stays alive when it has three neighbors") {
    assert(LivingCell.nextState(3).isDead === false, "Cell should have still be alive")
  }

  test("a living cell dies when it has four neighbors") {
    assert(LivingCell.nextState(4).isDead === true, "Cell should have died of overpopulation")
  }

  test("a living cell dies when it has five neighbors") {
    assert(LivingCell.nextState(5).isDead === true, "Cell should have died of overpopulation")
  }

  test("a dead cell is revived when it has 3 neighbors") {
    assert(DeadCell.nextState(3).isDead === false, "Cell should have been reborn")
  }


}
