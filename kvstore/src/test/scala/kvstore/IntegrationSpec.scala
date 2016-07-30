/**
 * Copyright (C) 2013-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package kvstore

import akka.actor.{ Actor, Props, ActorRef, ActorSystem }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers }
import scala.concurrent.duration._
import org.scalatest.FunSuiteLike
import org.scalactic.ConversionCheckedTripleEquals

class IntegrationSpec(_system: ActorSystem) extends TestKit(_system)
    with FunSuiteLike
        with Matchers
    with BeforeAndAfterAll
    with ConversionCheckedTripleEquals
    with ImplicitSender
    with Tools {

  import Replica._
  import Replicator._
  import Arbiter._

  def this() = this(ActorSystem("ReplicatorSpec"))

  override def afterAll: Unit = system.shutdown()

  /*
   * Recommendation: write a test case that verifies proper function of the whole system,
   * then run that with flaky Persistence and/or unreliable communication (injected by
   * using an Arbiter variant that introduces randomly message-dropping forwarder Actors).
   */

  test("case1: Integration") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-primary")
    val user = session(primary)
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-secondary")
    val second = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    user.setAcked("k1", "v1")
    arbiter.send(primary, Replicas(Set(primary, secondary)))

    val ack1 = user.set("k1", "v2")
    user.waitAck(ack1)

    val ack2 = user.set("k2", "v22")
    user.waitAck(ack2)

    val ack3 = user.set("k3", "v3")
    user.waitAck(ack3)

    val ack4 = user.set("k5", "v5")
    user.waitAck(ack4)

    val ack5 = user.set("k6", "v6")
    user.waitAck(ack5)

    val ack6 = user.remove("k1")
    user.waitAck(ack6)
    second.getAndVerify("k1")
  }

  test("case2: Integration2") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-primary")
    val user = session(primary)
    val secondary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case1-secondary")
    val second = session(secondary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    arbiter.expectMsg(Join)
    arbiter.send(secondary, JoinedSecondary)

    user.setAcked("k1", "v1")
    arbiter.send(primary, Replicas(Set(primary, secondary)))

    val ack1 = user.set("k1", "v2")
    user.waitAck(ack1)

    val ack2 = user.set("k2", "v22")
    user.waitAck(ack2)

    val ack3 = user.set("k3", "v3")
    user.waitAck(ack3)

    val ack4 = user.set("k5", "v5")
    user.waitAck(ack4)

    val ack5 = user.set("k6", "v6")
    user.waitAck(ack5)

    val ack6 = user.remove("k1")
    user.waitAck(ack6)
    second.getAndVerify("k1")
  }
  }
