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
        if(neighbors<2) DeadCell
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
    assert(LivingCell.nextState(2).isDead === false, "Cell should have died of loneliness")
  }


}
