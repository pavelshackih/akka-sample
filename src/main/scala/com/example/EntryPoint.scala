package com.example

import akka.actor._

object EntryPoint extends App {

  case class Start(n: Int)
  case class SendToANode(message: Int)
  case object Finished
  case class Failure(exc: Exception)

  val system = ActorSystem("system")

  val master = system.actorOf(Props[Master], name = "master")
  master ! Start(8)

  object Cluster {

    var list: List[ActorRef] = Nil

    def apply(int: Int): ActorRef = list(int)

    def apply() = list

    def build(context: ActorContext, n: Int, listener: ActorRef) {
      var cluster = new collection.mutable.ArrayBuffer[ActorRef]()
      for (i <- 0 until n) {
        cluster += context.actorOf(Props(new NodeActor(Some(listener), n, i)))
      }
      list = cluster.toList
    }
  }

  class Master extends Actor {

    override def receive = {
      case Start(n) =>
        if (n < 1) {
          context.sender ! Failure(new IllegalArgumentException(s"Node count should be positive"))
        }
        val broadNodes = n / 2
        val listener = context.actorOf(Props(new ListenerActor(n)), name = "listener")
        Cluster.build(context, n, listener)
        for (i <- (n - 1) to broadNodes by -1) {
          Cluster(i) ! SendToANode(0)
        }
        context stop self
    }
  }

  class NodeActor(listener: Option[ActorRef], val n: Int, val id: Int) extends Actor with ActorLogging with Node {

    var i = RandomGenerator.getRandom
    var visited = 0
    var globalSum = 0

    override def receive = {
      case SendToANode(value) =>
        i += value
        visited += 1
        if (visited >= childCount) {
          if (hasParent) {
            parentRef.map(_ ! SendToANode(i))
          } else {
            globalSum = i
            log.info(s"Sum: $globalSum")
            fireSumUpdated()
            sendToChilds(globalSum)
            context stop self
          }
          context become receiveResult
        }
    }

    def receiveResult: Receive = {
      case SendToANode(value) =>
        globalSum = value
        log.info(s"Sum: $globalSum")
        sendToChilds(globalSum)
        fireSumUpdated()
        context stop self
    }

    def sendToChilds(v: Int) {
      leftRef.map(_ ! SendToANode(v))
      rightRef.map(_ ! SendToANode(v))
    }

    def leftRef = if (left < 0) None else Some(Cluster(left))

    def rightRef = if (right < 0) None else Some(Cluster(right))

    def parentRef = if (hasParent) Some(Cluster(parent)) else None

    def fireSumUpdated() = listener.map(_ ! Finished)
  }

  trait Node {

    def n(): Int

    def id(): Int

    def globalSum(): Int

    def parent = {
      val inc = if (id % 2 == 0) 2 else 1
      val index = (id - inc) / 2
      if (index > -1) index else -1
    }

    def left = {
      val ix = 2 * id + 1
      if (ix > n - 1) -1 else ix
    }

    def right = {
      val ix = 2 * id + 2
      if (ix > n - 1) -1 else ix
    }

    def childCount = {
      var childs = 0
      if (left > -1) childs += 1
      if (right > -1) childs += 1
      childs
    }

    def hasParent = parent != -1

    def hasChild = id < n / 2 + 1
  }

  class ListenerActor(n: Int) extends Actor with ActorLogging {

    var counter = 0

    override def receive = {
      case Finished =>
        counter += 1
        if (counter == n) {
          log.info(s"Completed, expected sum: ${RandomGenerator.getActualSum}")
          if (system != null) system.shutdown()
        }
    }
  }
}
