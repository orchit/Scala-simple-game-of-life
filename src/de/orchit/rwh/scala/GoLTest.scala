package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite{
  class GolCell{val isDead=true}

  test("we have a dead cell"){
    val cell = new GolCell
    assert(cell isDead,"Cell is not dead")
  }
}
