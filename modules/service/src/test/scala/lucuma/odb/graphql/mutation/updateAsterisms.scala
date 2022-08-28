// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.show.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class updateAsterisms extends OdbSuite
                         with CreateProgramOps
                         with CreateObservationOps
                         with UpdateAsterismOps {

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("update add to empty asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      tid <- createEmptyTargetAs(pi, pid, "Larry")
      _   <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(oid),
        add  = List(tid),
        del  = Nil,
        exp  = List((oid, List(tid)))
      )
    } yield ()
  }

  test("update add to non-empty asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0  <- createEmptyTargetAs(pi, pid, "Larry")
      t1  <- createEmptyTargetAs(pi, pid, "Curly")
      _   <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(oid),
        add  = List(t0),
        del  = Nil,
        exp  = List((oid, List(t0)))
      )
      _   <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(oid),
        add  = List(t1),
        del  = Nil,
        exp  = List((oid, List(t0, t1)))
      )
    } yield ()
  }

  test("remove from asterism") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      t0 <- createEmptyTargetAs(pi, pid, "Larry")
      t1 <- createEmptyTargetAs(pi, pid, "Curly")
      _ <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(oid),
        add  = List(t0, t1),
        del  = Nil,
        exp  = List((oid, List(t0, t1)))
      )
      _ <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(oid),
        add  = Nil,
        del  = List(t0),
        exp  = List((oid, List(t1)))
      )
    } yield ()
  }

  test("multi-observation") {
    for {
      pid <- createProgramAs(pi)
      o0 <- createObservationAs(pi, pid)
      o1 <- createObservationAs(pi, pid)
      t0  <- createEmptyTargetAs(pi, pid, "Larry")
      t1  <- createEmptyTargetAs(pi, pid, "Curly")
      t2  <- createEmptyTargetAs(pi, pid, "Moe")
      _   <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(o0),
        add  = List(t0, t1),
        del  = Nil,
        exp  = List((o0, List(t0, t1)))
      )
      _ <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(o1),
        add  = List(t1, t2),
        del  = Nil,
        exp  = List((o1, List(t1, t2)))
      )
      _   <- updateAsterisms(
        user = pi,
        pid  = pid,
        oids = List(o0, o1),
        add  = Nil,
        del  = List(t1),
        exp  = List((o0, List(t0)), (o1, List(t2)))
      )
    } yield ()
  }

}

trait UpdateAsterismOps { this: OdbSuite =>

  def updateAsterisms(
    user: User,
    pid:  Program.Id,
    oids: List[Observation.Id],
    add:  List[Target.Id],
    del:  List[Target.Id],
    exp:  List[(Observation.Id, List[Target.Id])]
  ): IO[Unit] =
    expect(
      user = user,
      query =
        s"""
        mutation {
          updateAsterisms(input: {
            programId: ${pid.asJson}
            SET: {
              ${
                 add match {
                   case Nil => ""
                   case ts  => s"ADD: [ ${ts.map(_.show).mkString("\"", "\",\"", "\"")} ]"
                 }
              }
              ${
                del match {
                  case Nil => ""
                  case ts  => s"DELETE: [ ${ts.map(_.show).mkString("\"", "\",\"", "\"")} ]"
                }
              }
            }
            WHERE: {
              id: { IN: [ ${oids.map(_.show).mkString("\"", "\",\"", "\"")} ] }
            }
          }) {
            id
            targetEnvironment {
              asterism {
                id
              }
            }
          }
        }
      """,
      expected =
        json"""
        {
          "updateAsterisms":
            ${exp.map { case (oid, ts) =>
              json"""
                {
                   "id": ${oid.asJson},
                   "targetEnvironment": {
                     "asterism":
                       ${ts.map { tid =>
                         json"""
                           {
                             "id": ${tid.asJson}
                           }
                         """
                       }}
                   }
                }
              """
            }}
        }
      """.asRight
    )

}
