package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite{
  object DeadCell{val isDead=true}

  test("we have a dead cell"){
    assert(DeadCell isDead,"Cell is not dead")
  }

}
