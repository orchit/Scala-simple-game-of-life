package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite{
  class DeadCell{val isDead=true}

  test("we have a dead cell"){
    val cell = new DeadCell
    assert(cell isDead,"Cell is not dead")
  }
}
