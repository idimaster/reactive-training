/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeSet.Contains
import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root=createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case o:Operation =>
      root ! o
    case GC =>
      println("GCn")
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case o:Operation =>
      newRoot ! o
    case CopyFinished =>
      println("copyFinished")
      root = newRoot
      context.unbecome()
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with Stash {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, el) =>
      println("insert", id, el)
      if(elem == el) {
        if (removed) removed = false
        requester ! OperationFinished(id)
      } else {
        val position = if (el > elem) Right else Left
        if (subtrees.contains(position)) {
          subtrees(position) ! Insert(requester, id, el)
        } else {
          subtrees += position -> context.actorOf(props(el, false))
          requester ! OperationFinished(id)
        }
      }
    case Contains(requester, id, el) =>
      if(elem == el){
        requester ! ContainsResult(id, !removed)
      }
      else {
        val position = if(el > elem) Right else Left
          if (subtrees.contains(position)) {
            subtrees(position) ! Contains(requester, id, el)
          } else {
            requester ! ContainsResult(id, false)
          }
      }
    case Remove(requester, id, el) =>
      if(elem == el){
        removed = true;
        requester ! OperationFinished(id)
      }
      else {
        val position = if(el > elem) Right else Left
        if (subtrees.contains(position)) {
          subtrees(position) ! Remove(requester, id, el)
        } else {
          requester ! OperationFinished(id)
        }
      }
    case r:OperationReply =>
      context.parent ! r
    case CopyTo(node) =>
      println("copyTo", subtrees, removed)
      if(subtrees.isEmpty && removed) {
        sender ! CopyFinished
      } else {
        context.become(copying(subtrees.values.toSet, removed))
        if (!removed) node ! Insert(self, -1, elem)
        subtrees.values.foreach(action => action ! CopyTo(node))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished =>
      println("node CopyFinished", expected, insertConfirmed)
      val nodes = expected - sender
      if(nodes.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        println("stop node")
        context.stop(self)
      } else {
        context.become(copying(nodes, insertConfirmed))
      }
    case r:OperationReply =>
      println("node", r, expected, insertConfirmed)
      if(expected.isEmpty) {
        context.parent ! CopyFinished
        println("stop node")
        context.stop(self)
      } else {
        context.become(copying(expected, false))
      }
  }
}
