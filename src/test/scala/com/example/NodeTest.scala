package com.example

import org.scalatest.{Assertions, WordSpec}

/**
 *
 */
class NodeTest extends WordSpec with Assertions {

  import com.example.EntryPoint._

  "Node" should {
    "has no parent if id == 0" in {
      val node = new NodeImpl(0, 0)
      assert(node.hasParent === false)
    }
  }

  "Node" should {
    "return two childs in case if it root and n == 3" in {
      val node = new NodeImpl(0, 3)
      assert(node.childCount === 2)
    }
  }

  "Node" should {
    "return one childs in case if it root and n == 2" in {
      val node = new NodeImpl(0, 2)
      assert(node.childCount === 1)
    }
  }

  "Node index for left child " should {
    "be equal 1 for id == 0 " in {
      val node = new NodeImpl(0, 2)
      assert(node.left === 1)
    }
  }

  "Node index for right child " should {
    "be equal 2 for id == 0 " in {
      val node = new NodeImpl(0, 3)
      assert(node.right === 2)
    }
  }


  class NodeImpl(val id: Int, val n: Int) extends Node {

    override def globalSum(): Int = ???

  }

}
