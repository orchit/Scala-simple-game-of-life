package de.orchit.rwh.scala

import org.scalatest.FunSuite


class GoLTest extends FunSuite {

  trait Cell {
    def isDead = {
      this == DeadCell
    }

    def nextState(neighbors: Int): Cell
  }

  object DeadCell extends Cell {
    def nextState(neighbors: Int) = {
      if (neighbors == 3)
        LivingCell
      else
        this
    }
  }

  object LivingCell extends Cell {
    def nextState(neighbors: Int) = {
      if (neighbors < 2 || neighbors > 3) DeadCell
      else LivingCell
    }
  }

  class Field {
    val rows = 9
    val cols = 9
    private val map:Array[Array[Cell]] = Array.ofDim(rows,cols)
    for (row <- 0 to 8; col <- 0 to 8)
      map(row)(col)=DeadCell

    def getCell(row: Int)(col: Int) = {
      map(row)(col)
    }

    def setCell(row: Int)(col: Int)(cell:Cell){
      map(row)(col)=cell
    }

    def getNeighborCount(row:Int)(col:Int)={
      var count=0;
      for (row <- 0 to 8; col <- 0 to 8 if getCell(row)(col)==LivingCell)
        count+=1
      count
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

  test("a dead cell is still dead when it has 2 neighbors") {
    assert(DeadCell.nextState(2).isDead === true, "Cell should have still be dead")
  }

  test("a dead cell is still dead when it has 4 neighbors") {
    assert(DeadCell.nextState(4).isDead === true, "Cell should have still be dead")
  }

  test("we have a field with 9 rows and 9 cols") {
    assert((new Field).rows === 9)
    assert((new Field).cols === 9)
  }

  test("the field is initially filled with dead cells") {
    val field = new Field
    for (row <- 0 to 8; col <- 0 to 8)
      assert(field.getCell(row)(col) === DeadCell)
  }

  test("We can set values in the field boundaries"){
    val field = new Field
    field.setCell(2)(3)(LivingCell)
    field.setCell(0)(3)(LivingCell)
    field.setCell(2)(8)(LivingCell)
    assert(field.getCell(2)(3)===LivingCell)
    assert(field.getCell(0)(3)===LivingCell)
    assert(field.getCell(2)(8)===LivingCell)
  }

  test("we can ask for the neighborcount of the cell at 1,2 with 2 neighbors"){
    val field = new Field
    field.setCell(0)(1)(LivingCell)
    field.setCell(1)(3)(LivingCell)
    assert(field.getNeighborCount(1)(2) === 2)
  }

  test("we can ask for the neighborcount of the cell at 1,2 with 4 neighbors"){
    val field = new Field
    field.setCell(0)(1)(LivingCell)
    field.setCell(1)(1)(LivingCell)
    field.setCell(0)(2)(LivingCell)
    field.setCell(1)(3)(LivingCell)
    assert(field.getNeighborCount(1)(2) === 4)
  }
}
