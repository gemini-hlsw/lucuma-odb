// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.util.Codecs.*
import natchez.Trace.Implicits.noop
import org.http4s.*
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

class obsAttachmentsAssignments extends ObsAttachmentsSuite {

  enum UpdateInput:
    case Null extends UpdateInput
    case Skip(existing: List[(ObsAttachment.Id, TestAttachment)]) extends UpdateInput
    case Values(values: List[(ObsAttachment.Id, TestAttachment)]) extends UpdateInput

    def set: String = this match
      case Null => "obsAttachments: null"
      case Skip(_) => "subtitle: \"a subtitle\"" // set something unrelated
      case Values(values) => s"obsAttachments: ${values.map(_._1.asJson).mkString("[", ", ", "]")}"

    def expect: Json = this match
      case Null => expectedAttachments(List.empty)
      case Skip(existing) => expectedAttachments(existing.toList)
      case Values(values) => expectedAttachments(values.toList)

  def assertAttachmentsWithObs(
    user:           User,
    programId:      Program.Id,
    expectedTas:    (ObsAttachment.Id, TestAttachment, List[Observation.Id])*
  ): IO[Unit] =
    assertAttachmentsWithObs(user, programId, false, expectedTas: _*)

  def assertAttachmentsWithObs(
    user:           User,
    programId:      Program.Id,
    includeDeleted: Boolean,
    expectedTas:    (ObsAttachment.Id, TestAttachment, List[Observation.Id])*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
          query {
            program(programId: "$programId") {
              ${ObsAttachmentsWithObsGraph(includeDeleted)}
            }
          }
        """,
      expected = Right(
        Json.obj(
          "program" -> expectedAttachmentsWithObs(expectedTas.toList)
        )
      )
    )


  def assertObservation(
    user:        User,
    pid:         Program.Id,
    oid:         Observation.Id,
    attachments: (ObsAttachment.Id, TestAttachment)*
  ): IO[Unit] = 
    expect(
      user: User,
      query = s"""
        query {
          observation(
            observationId: ${oid.asJson}
          ) {
            $ObsAttachmentsGraph
          }
        }
      """,
      expected = Right(
        Json.obj(
          "observation" -> expectedAttachments(attachments.toList)
        )
      )
    )

  def createObservation(
    user:        User,
    pid:         Program.Id,
    attachments: (ObsAttachment.Id, TestAttachment)*
  ): IO[Observation.Id] = 
    query(
      user = user,
      query = s"""
        mutation {
          createObservation(
            input: {
              programId: ${pid.asJson}
              SET: {
                obsAttachments: [${attachments.map(_._1.asJson).mkString(", ")}]
              }
            }
          ) {
            observation {
             id 
            }
          }
        }
      """
    ).map { json => 
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def deleteObservation(
    user: User,
    pid:  Program.Id,
    oid:  Observation.Id
  ): IO[Unit] = 
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              programId: ${pid.asJson}
              WHERE: {
                id: {
                  EQ: ${oid.asJson}
                }
              }
              SET: {
                existence: DELETED
              }
            }
          ) {
            observations {
             id 
            }
          }
        }
      """,
      expected = Right(
        Json.obj(
          "updateObservations" -> Json.obj(
            "observations" -> Json.arr(
              Json.obj("id" -> oid.asJson)
            )
          )
        )
      )
    )

  def updateObservation(
    user:  User,
    pid:   Program.Id,
    oid:   Observation.Id,
    input: UpdateInput
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              programId: ${pid.asJson}
              WHERE: {
                id: {
                  EQ: ${oid.asJson}
                }
              }
              SET: {
                ${input.set}
              }
            }
          ) {
            observations {
              $ObsAttachmentsGraph
            }
          }
        }
      """,
      expected = Right(
        Json.obj(
          "updateObservations" -> Json.obj(
            "observations" -> Json.arr(
              input.expect
            )
          )
        )
      )
    )

  val file1 = TestAttachment("file1.fits", "mos_mask", "A description".some, "Hopeful")
  val file2 = TestAttachment("file2.jpg", "finder", "jpg file".some, "A finder JPG file")

  val updateEmpty: UpdateInput.Values = UpdateInput.Values(List.empty)
  def updateFile1(aid: ObsAttachment.Id): UpdateInput.Values = 
    UpdateInput.Values(List((aid, file1)))
  def updateBothFiles(aid1: ObsAttachment.Id, aid2: ObsAttachment.Id): UpdateInput.Values =
    UpdateInput.Values(List((aid1, file1), (aid2, file2)))
  def skipFile1(aid: ObsAttachment.Id): UpdateInput =
    UpdateInput.Skip(List((aid, file1)))

  test("can successfully create an observation with an assigned attachment") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, file1).toAttachmentId
      oid <- createObservation(pi, pid, (aid, file1))
      _   <- assertObservation(pi, pid, oid, (aid, file1))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
    } yield ()
  }

  test("can successfully create an observation with 2 assigned attachments") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, file1).toAttachmentId
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      oid  <- createObservation(pi, pid, (aid1, file1), (aid2, file2))
      _    <- assertObservation(pi, pid, oid, (aid1, file1), (aid2, file2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List(oid)), (aid2, file2, List(oid)))
    } yield ()
  }

  test("can successfully assign an attachment to 2 observations") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, file1).toAttachmentId
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, file1), (aid2, file2))
      oid2 <- createObservation(pi, pid, (aid2, file2))
      _    <- assertObservation(pi, pid, oid1, (aid1, file1), (aid2, file2))
      _    <- assertObservation(pi, pid, oid2, (aid2, file2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List(oid1)), (aid2, file2, List(oid1, oid2)))
    } yield ()
  }

  test("can successfully create an observation with no assigned attachments") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservation(pi, pid)
      _   <- assertObservation(pi, pid, oid)
    } yield ()
  }

  test("can successfully update an observation with an assignment") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      aid <- insertAttachment(pi, pid, file1).toAttachmentId
      _   <- updateObservation(pi, pid, oid, updateFile1(aid))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
    } yield ()
  }

  test("update with null removes the assignments") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, file1).toAttachmentId
      oid <- createObservation(pi, pid, (aid, file1))
      _   <- assertObservation(pi, pid, oid, (aid, file1))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
      _   <- updateObservation(pi, pid, oid, UpdateInput.Null)
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List.empty))
    } yield ()
  }

  test("update with empty list removes the assignments") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, file1).toAttachmentId
      oid <- createObservation(pi, pid, (aid, file1))
      _   <- assertObservation(pi, pid, oid, (aid, file1))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
      _   <- updateObservation(pi, pid, oid, updateEmpty)
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List.empty))
    } yield ()
  }

  test("update without obsAttachments leaves the assignments") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      aid <- insertAttachment(pi, pid, file1).toAttachmentId
      _   <- updateObservation(pi, pid, oid, updateFile1(aid))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
      _   <- updateObservation(pi, pid, oid, skipFile1(aid))
      _   <- assertAttachmentsWithObs(pi, pid, (aid, file1, List(oid)))
    } yield ()
  }

  test("can successfully update an observation with 2 assignments") {
    for {
      pid  <- createProgramAs(pi)
      oid  <- createObservationAs(pi, pid)
      aid1 <- insertAttachment(pi, pid, file1).toAttachmentId
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List.empty), (aid2, file2, List.empty))
      _    <- updateObservation(pi, pid, oid, updateBothFiles(aid1, aid2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List(oid)), (aid2, file2, List(oid)))
    } yield ()
  }

  test("can delete an assigned attachment") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, file1).toAttachmentId
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      oid  <- createObservation(pi, pid, (aid1, file1), (aid2, file2))
      _    <- assertObservation(pi, pid, oid, (aid1, file1), (aid2, file2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List(oid)), (aid2, file2, List(oid)))
      _    <- deleteAttachment(pi, pid, aid1).expectOk
      _    <- assertObservation(pi, pid, oid, (aid2, file2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid2, file2, List(oid)))
    } yield ()
  }

  test("can delete an observation with assigned attachments") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, file1).toAttachmentId
      aid2 <- insertAttachment(pi, pid, file2).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, file1), (aid2, file2))
      oid2 <- createObservation(pi, pid, (aid2, file2))
      _    <- assertObservation(pi, pid, oid1, (aid1, file1), (aid2, file2))
      _    <- assertObservation(pi, pid, oid2, (aid2, file2))
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List(oid1)), (aid2, file2, List(oid1, oid2)))
      _    <- deleteObservation(pi, pid, oid1)
      _    <- assertAttachmentsWithObs(pi, pid, (aid1, file1, List.empty), (aid2, file2, List(oid2)))
      // _    <- assertAttachmentsWithObs(pi, pid, true, (aid1, file1, List(oid1)), (aid2, file2, List(oid1)))
    } yield ()
  }
}
