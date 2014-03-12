package com.example

import akka.actor._
import scala.Some

object EntryPoint extends App {

  val system = ActorSystem("system")

  val n = 16
  val broadNodes = n / 2

  val randomUpperBound = 10
  val random = util.Random

  def getRandom = random.nextInt(randomUpperBound)

  val randomValues = 0 until n map(x => getRandom)
  val expectedSum = randomValues.reduce(_ + _)
  println(s"Expected sum: $expectedSum")

  val listener = system.actorOf(Props(new Listener(n)), name = "listener")

  var cluster = new collection.mutable.ArrayBuffer[ActorRef]()
  for (i <- 0 until n) {
    cluster += system.actorOf(Props(new BiNode(listener, n, i)))
  }

  // start calculating from childs that to parents
  for (i <- (n - 1) to broadNodes by -1) {
    cluster(i) ! SendToANode(0)
  }

  class BiNode(listener: ActorRef, val n: Int, id: Int) extends Actor with ActorLogging {

    var i = randomValues(id)

    val childs = new collection.mutable.HashMap[Child, ActorRef]
    var visited = 0

    left.map(childs.put(Left, _))
    right.map(childs.put(Right, _))
    val hasParent = id != 0

    var globalSum = 0
    var propagated = false

    override def receive = {
      case SendToANode(value) =>
        i += value
        if (propagated) {
          globalSum = value
          log.info(s"Sum: $globalSum")
          childs.values.foreach(_ ! SendToANode(globalSum))
          fireSumUpdated()
          context stop self
        } else {
          visited += 1
          if (visited >= childs.size) { // waiting when both of childs returns they values
            propagated = true
            if (hasParent) {
              parent.map(_ ! SendToANode(i))
            } else {
              // sum calculated, propagate value to childs
              globalSum = i
              log.info(s"Sum: $globalSum")
              fireSumUpdated()
              childs.values.foreach(_ ! SendToANode(globalSum))
              context stop self
            }
          }
        }
    }

    def fireSumUpdated() = listener ! Finished

    def parent = {
      val inc = if (id % 2 == 0) 2 else 1
      val index = (id - inc) / 2
      if (index > -1) Some(cluster(index)) else None
    }

    def left = {
      val ix = 2 * id + 1
      if (ix > cluster.size - 1) None else Some(cluster(ix))
    }

    def right = {
      val ix = 2 * id + 2
      if (ix > cluster.size - 1) None else Some(cluster(ix))
    }

    def hasChilds = id < n / 2 + 1
  }

  class Listener(n: Int) extends Actor with ActorLogging {

    var counter = 0

    override def receive = {
      case Finished =>
        counter += 1
        if (counter == n) {
          log.info("Completed")
          system.shutdown()
        }
    }
  }

  case class SendToANode(message: Int)
  case object Finished
  trait Child
  case object Left extends Child
  case object Right extends Child

}
