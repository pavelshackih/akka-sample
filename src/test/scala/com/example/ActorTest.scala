package com.example

import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import akka.testkit.{TestProbe, TestKit, ImplicitSender}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterAll

class ActorTest(_system: ActorSystem) extends TestKit(_system) with FunSuite with ShouldMatchers with BeforeAndAfterAll with ImplicitSender {

  def this() = this(ActorSystem("systemtest"))

  override def afterAll(): Unit = system.shutdown()

  import EntryPoint._

  test("Cluster with wrong number of nodes") {
    val probe = TestProbe()
    val master = system.actorOf(Props[Master])
    probe.send(master, Start(-1))
    probe.expectMsgType[Failure]
  }

  test("Nodes should be created properly with n == 3") {
    val n = 3
    val master = system.actorOf(Props[Master])
    master ! Start(3)
    expectNoMsg()
    assert(Cluster().size == n)
  }

  test("Cluster with n == 1") {
    prepareGenerator()
    val n = 1
    val probe = TestProbe()
    val listener = system.actorOf(Props(new Actor {
      override def receive = {
        case Finished => probe.ref forward Finished
      }
    }), name = "listener")
    val node = system.actorOf(Props(new NodeActor(Some(listener), n, 0)))
    probe.send(node, SendToANode(0))
    probe.expectMsg(Finished)
  }

  test("Node without childs") {
    prepareGenerator()
    val n = 2
    val probe = TestProbe()
    val mockParent = system.actorOf(Props(new Actor {
      override def receive = {
        case SendToANode(v) => probe.ref forward SendToANode(v)
      }
    }))
    val node = system.actorOf(Props(new NodeActor(Some(listener(probe)), n, 1) {
      override def parentRef: Option[ActorRef] = Some(mockParent)
    }))
    node ! SendToANode(0)
    probe.expectMsg(SendToANode(1))
  }

  class MockGenerator extends Generator {
    override def getActualSum: Int = 1

    override def getRandom: Int = 1
  }

  def prepareGenerator() {
    val generator = new MockGenerator
    RandomGenerator(generator)
  }

  def listener(probe: TestProbe) = system.actorOf(Props(new Actor {
    override def receive = {
      case Finished => probe.ref forward Finished
    }
  }), name = "listener")
}
