package com.example

import akka.actor._

object EntryPoint extends App {

  val system = ActorSystem("system")

  val n = 10

  val randomUpperBound = 100
  val random = util.Random
  def getRandom = random.nextInt(randomUpperBound)

  val master = system.actorOf(Props(new Master(n)), name = "master")
  master ! StartSumCalc

  class Master(n: Int) extends Actor {

    var cluster = new collection.mutable.ArrayBuffer[ActorRef]()
    for (i <- 0 to n) {
      cluster += context.actorOf(Props[Node])
    }

    def broadcast(obj: Any) = cluster.foreach(_ ! obj)

    var sum = 0
    var messages = 0

    override def receive = {
      case StartSumCalc => broadcast(ReportPrivateNum)
      case SendToANode(i) =>
        messages += 1
        sum += i
        if (messages == n) {
          messages = 0
          broadcast(SendToANode(sum))
        }
      case SumAccepted =>
        messages += 1
        if (messages == n) {
          system.shutdown()
        }
    }
  }

  class Node extends Actor with ActorLogging {

    val i = getRandom
    var sum: Int = 0

    override def receive = {
      case ReportPrivateNum =>
        sender ! SendToANode(i)
      case SendToANode(sumFromMaster) =>
        sum = sumFromMaster
        log.info(s"Sum of all nodes: $sum")
        sender ! SumAccepted
        context stop self
    }
  }

  case class SendToANode(message: Int)
  case object StartSumCalc
  case object ReportPrivateNum
  case object SumAccepted

}
