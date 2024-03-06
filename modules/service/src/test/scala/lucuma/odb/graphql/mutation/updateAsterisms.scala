// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.show.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.odb.data.OdbError

class updateAsterisms extends OdbSuite {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("update add to empty asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      tid <- createTargetAs(pi, pid, "Larry")
      _   <- updateAsterisms(
        user = pi,
        oids = List(oid),
        add  = List(tid),
        del  = Nil,
        exp  = List((oid, List(tid)))
      )
      _   <- chronAsterismUpdates(pid).assertEquals(
        List(
          json"""
          {
            "c_user"           : ${pi.id},
            "c_operation"      : "INSERT",
            "c_target_id"      : $tid,
            "c_program_id"     : $pid,
            "c_observation_id" : $oid
          }
          """
        )
      )
    } yield ()
  }

  test("update add to non-empty asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      _   <- updateAsterisms(
        user = pi,
        oids = List(oid),
        add  = List(t0),
        del  = Nil,
        exp  = List((oid, List(t0)))
      )
      _   <- updateAsterisms(
        user = pi,
        oids = List(oid),
        add  = List(t1),
        del  = Nil,
        exp  = List((oid, List(t0, t1)))
      )
      _   <- chronAsterismUpdates(pid).assertEquals(
        List(
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t0,
            "c_program_id" : $pid,
            "c_observation_id" : $oid
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t1,
            "c_program_id" : $pid,
            "c_observation_id" : $oid
          }
          """
        )
      )
    } yield ()
  }

  test("remove from asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0 <- createTargetAs(pi, pid, "Larry")
      t1 <- createTargetAs(pi, pid, "Curly")
      _ <- updateAsterisms(
        user = pi,
        oids = List(oid),
        add  = List(t0, t1),
        del  = Nil,
        exp  = List((oid, List(t0, t1)))
      )
      _ <- updateAsterisms(
        user = pi,
        oids = List(oid),
        add  = Nil,
        del  = List(t0),
        exp  = List((oid, List(t1)))
      )
      _   <- chronAsterismUpdates(pid).assertEquals(
        List(
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t0,
            "c_program_id" :$pid,
            "c_observation_id" : $oid
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t1,
            "c_program_id" :$pid,
            "c_observation_id" : $oid
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "DELETE",
            "c_target_id" : $t0,
            "c_program_id" :$pid,
            "c_observation_id" : $oid
          }
          """
        )
      )
    } yield ()
  }

  test("multi-observation") {
    for {
      pid <- createProgramAs(pi)
      o0 <- createObservationAs(pi, pid)
      o1 <- createObservationAs(pi, pid)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      _   <- updateAsterisms(
        user = pi,
        oids = List(o0),
        add  = List(t0, t1),
        del  = Nil,
        exp  = List((o0, List(t0, t1)))
      )
      _ <- updateAsterisms(
        user = pi,
        oids = List(o1),
        add  = List(t1, t2),
        del  = Nil,
        exp  = List((o1, List(t1, t2)))
      )
      _   <- updateAsterisms(
        user = pi,
        oids = List(o0, o1),
        add  = Nil,
        del  = List(t1),
        exp  = List((o0, List(t0)), (o1, List(t2)))
      )
      _   <- chronAsterismUpdates(pid).map(_.map(_.spaces2)).assertEquals(
        List(
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t0,
            "c_program_id" : $pid,
            "c_observation_id" : $o0
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t1,
            "c_program_id" : $pid,
            "c_observation_id" : $o0
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t1,
            "c_program_id" : $pid,
            "c_observation_id" : $o1
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "INSERT",
            "c_target_id" : $t2,
            "c_program_id" : $pid,
            "c_observation_id" : $o1
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "DELETE",
            "c_target_id" : $t1,
            "c_program_id" : $pid,
            "c_observation_id" : $o0
          }
          """,
          json"""
          {
            "c_user" : ${pi.id},
            "c_operation" : "DELETE",
            "c_target_id" : $t1,
            "c_program_id" : $pid,
            "c_observation_id" : $o1
          }
          """
        ).map(_.spaces2)
      )
    } yield ()
  }

  private def gidList[A: Gid](as: List[A]): String =
    s"[ ${as.map(_.show).mkString("\"", "\",\"", "\"")} ]"

  test("fail multi-program observations") {

    def expectFail(
      oids: List[Observation.Id],
      add:  List[Target.Id]
    ): IO[Unit] =
      expect(
        user = pi,
        query =
          s"""
          mutation {
            updateAsterisms(input: {
              SET: { ADD: ${gidList(add)} }
              WHERE: { id: { IN: ${gidList(oids)} } }
            }) { observations { id } }
          }
        """,
        expected = List(OdbError.InvalidObservationList(NonEmptyList.fromListUnsafe(oids)).message).asLeft
      )

    for {
      p0 <- createProgramAs(pi)
      p1 <- createProgramAs(pi)
      o0 <- createObservationAs(pi, p0)
      o1 <- createObservationAs(pi, p1)
      t0  <- createTargetAs(pi, p0, "Larry")
      t1  <- createTargetAs(pi, p0, "Curly")
      _   <- expectFail(List(o0, o1), List(t0, t1))
    } yield ()
  }

  test("fail multi-program targets") {

    def expectFail(
      pid:  Program.Id,
      oids: List[Observation.Id],
      add:  List[Target.Id]
    ): IO[Unit] =
      expect(
        user = pi,
        query =
          s"""
          mutation {
            updateAsterisms(input: {
              SET: { ADD: ${gidList(add)} }
              WHERE: { id: { IN: ${gidList(oids)} } }
            }) { observations { id } }
          }
        """,
        expected = List(OdbError.InvalidTargetList(pid, NonEmptyList.fromListUnsafe(add)).message).asLeft
      )

    for {
      p0 <- createProgramAs(pi)
      p1 <- createProgramAs(pi)
      o0 <- createObservationAs(pi, p0)
      o1 <- createObservationAs(pi, p0)
      t0  <- createTargetAs(pi, p0, "Larry")
      t1  <- createTargetAs(pi, p1, "Curly")
      _   <- expectFail(p0, List(o0, o1), List(t0, t1))
    } yield ()
  }
}
