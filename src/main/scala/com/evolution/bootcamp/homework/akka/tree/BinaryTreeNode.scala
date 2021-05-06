package com.evolution.bootcamp.homework.akka.tree


import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case insert: Insert => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove => doRemove(remove)
  }

  private def doInsert(m: Insert): Unit = {
    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else if (m.elem > elem) {
      sendInsertMessageToChild(Right, m)
    } else {
      sendInsertMessageToChild(Left, m)
    }
  }

  private def sendInsertMessageToChild(position: Position, m: Insert): Unit = {
    subtrees.get(position) match {
      case Some(value) => value ! m
      case None =>
        subtrees += (position -> context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false)))
        m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) {
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      sendContainsMessageToChild(m)
    }
  }

  private def sendContainsMessageToChild(m: Contains): Unit = {
    if (m.elem > elem && subtrees.contains(Right))
      subtrees(Right) ! m
    else if (m.elem < elem && subtrees.contains(Left))
      subtrees(Left) ! m
    else
      m.requester ! ContainsResult(m.id, result = false)
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else if (m.elem > elem && subtrees.contains(Right)) {
      subtrees(Right) ! m
    } else if (m.elem < elem && subtrees.contains(Left)) {
      subtrees(Left) ! m
    } else {
      m.requester ! OperationFinished(m.id)
    }
  }

}

