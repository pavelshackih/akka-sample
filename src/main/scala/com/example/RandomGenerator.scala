package com.example

/**
 *
 */
object RandomGenerator extends Generator {

  var generator: Generator = new SimpleGenerator

  override def getRandom: Int = generator.getRandom

  override def getActualSum: Int = generator.getActualSum

  def apply(newGenerator: Generator) = this.generator = newGenerator
}

trait Generator {

  def getRandom: Int

  def getActualSum: Int

}

class SimpleGenerator extends Generator {

  var sum = 0
  val randomUpperBound = 10
  val random = new java.util.Random()

  def getRandom = {
    var tmp = 0
    // we need make sure that the value is relevant for all threads
    synchronized {
      tmp = random.nextInt(randomUpperBound)
      sum += tmp
    }
    tmp
  }

  def getActualSum = sum

}
