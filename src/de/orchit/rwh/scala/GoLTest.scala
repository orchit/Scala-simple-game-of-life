package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite{
  object DeadCell{val isDead=true}
  object LivingCell{val isDead=false}

  test("we have a dead cell"){
    assert(DeadCell isDead,"Cell is not dead")
  }

  test("we can have a living cell"){
    assert(LivingCell.isDead == false, "Cell is not living")
  }
}
