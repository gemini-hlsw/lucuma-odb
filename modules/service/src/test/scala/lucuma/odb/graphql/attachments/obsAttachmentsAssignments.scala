// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.service.ObsAttachmentAssignmentService

class obsAttachmentsAssignments extends AttachmentsSuite {

  enum UpdateInput:
    case Null extends UpdateInput
    case Skip(existing: List[(Attachment.Id, TestAttachment)]) extends UpdateInput
    case Values(values: List[(Attachment.Id, TestAttachment)]) extends UpdateInput

    def set: String = this match
      case Null => "attachments: null"
      case Skip(_) => "subtitle: \"a subtitle\"" // set something unrelated
      case Values(values) => s"attachments: ${values.map(_._1.asJson).mkString("[", ", ", "]")}"

    def expect: Json = this match
      case Null => expectedAttachments(List.empty)
      case Skip(existing) => expectedAttachments(existing.toList)
      case Values(values) => expectedAttachments(values.toList)

  def assertObservation(
    user:        User,
    pid:         Program.Id,
    oid:         Observation.Id,
    attachments: (Attachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user: User,
      query = s"""
        query {
          observation(
            observationId: ${oid.asJson}
          ) {
            $AttachmentsGraph
          }
        }
      """,
      expected = Right(
        Json.obj(
          "observation" -> expectedAttachments(attachments.toList)
        )
      )
    )

  def observationQuery(
    pid: Program.Id,
    attachments: (Attachment.Id, TestAttachment)*
  ): String =
    s"""
      mutation {
        createObservation(
          input: {
            programId: ${pid.asJson}
            SET: {
              attachments: [${attachments.map(_._1.asJson).mkString(", ")}]
            }
          }
        ) {
          observation {
            id
          }
        }
      }
    """

  def createObservation(
    user:        User,
    pid:         Program.Id,
    attachments: (Attachment.Id, TestAttachment)*
  ): IO[Observation.Id] =
    query(
      user = user,
      query = observationQuery(pid, attachments*)
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createObservationWithError(
    user:        User,
    pid:         Program.Id,
    error:       String,
    attachments: (Attachment.Id, TestAttachment)*
  ): IO[Unit] =
    expect(
      user = user,
      query = observationQuery(pid, attachments*),
      expected = List(error).asLeft
    )

  def updateObservation(
    user:  User,
    oid:   Observation.Id,
    input: UpdateInput,
    error: Option[String] = None
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
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
              $AttachmentsGraph
            }
          }
        }
      """,
      expected = 
        error.fold(
          Json.obj(
            "updateObservations" -> Json.obj(
              "observations" -> Json.arr(
                input.expect
              )
            )
          ).asRight
        )(List(_).asLeft)
    )

  def updateMultipleObservations(
    user:  User,
    input: UpdateInput,
    oids:   Observation.Id*
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              WHERE: {
                id: {
                  IN: ${oids.asJson}
                }
              }
              SET: {
                ${input.set}
              }
            }
          ) {
            observations {
              $AttachmentsGraph
            }
          }
        }
      """,
      expected = 
          Json.obj(
            "updateObservations" -> Json.obj(
              "observations" -> 
                oids.map(_ => input.expect).asJson
            )
          ).asRight
    )

  def cloneObservationQuery(oid: Observation.Id, aids: Attachment.Id*): String =
    s"""
      mutation {
        cloneObservation(input: {
          observationId: "$oid"
          SET: {
            attachments: ${aids.map(_.asJson).mkString("[", ", ","]")}
          }
        }) {
          newObservation { id }
        }
      }
    """


  def cloneObservationWithAttachments(user: User, oid: Observation.Id, aids: Attachment.Id*): IO[Observation.Id] =
    query(
      user = user,
      query = cloneObservationQuery(oid, aids*)
    ).map(_.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id])

  def cloneObservationWithAttachmentsWithError(
    user: User,
    oid: Observation.Id,
    error: String,
    aids: Attachment.Id*
  ): IO[Unit] =
    expect(
      user = user,
      query = cloneObservationQuery(oid, aids*),
      expected = List(error).asLeft
    )

  val mosMask = TestAttachment("file1.fits", "mos_mask", "A description".some, "Hopeful")
  val finder  = TestAttachment("file2.jpg", "finder", "jpg file".some, "A finder JPG file")
  val preImaging  = TestAttachment("preImaging.fits", "pre_imaging", none, "A pre imaging file")
  val science = TestAttachment("science.pdf", "science", none, "science file")
  val team = TestAttachment("team.pdf", "science", none, "team file")
  val customSed = TestAttachment("sed.dat", "custom_sed", none, "custom sed file")

  val updateEmpty: UpdateInput.Values = UpdateInput.Values(List.empty)
  def updateFile1(aid: Attachment.Id): UpdateInput.Values =
    UpdateInput.Values(List((aid, mosMask)))
  def updateBothFiles(aid1: Attachment.Id, aid2: Attachment.Id): UpdateInput.Values =
    UpdateInput.Values(List((aid1, mosMask), (aid2, finder)))
  def skipFile1(aid: Attachment.Id): UpdateInput =
    UpdateInput.Skip(List((aid, mosMask)))

  test("can successfully create an observation with an assigned attachment") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask).toAttachmentId
      oid <- createObservation(pi, pid, (aid, mosMask))
      _   <- assertObservation(pi, pid, oid, (aid, mosMask))
    } yield ()
  }

  test("can successfully create an observation with 2 assigned attachments") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid  <- createObservation(pi, pid, (aid1, mosMask), (aid2, finder))
      _    <- assertObservation(pi, pid, oid, (aid1, mosMask), (aid2, finder))
    } yield ()
  }

  test("can successfully assign an attachment to 2 observations") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, mosMask), (aid2, finder))
      oid2 <- createObservation(pi, pid, (aid2, finder))
      _    <- assertObservation(pi, pid, oid1, (aid1, mosMask), (aid2, finder))
      _    <- assertObservation(pi, pid, oid2, (aid2, finder))
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
      aid <- insertAttachment(pi, pid, mosMask).toAttachmentId
      _   <- updateObservation(pi, oid, updateFile1(aid))
    } yield ()
  }

  test("updating an observation with assignments replaces previous ones") {
    for {
      pid <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, preImaging).toAttachmentId
      aid2 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid3 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid <- createObservation(pi, pid, (aid1, finder))
      _   <- updateObservation(pi, oid, updateBothFiles(aid2, aid3))
    } yield ()
  }

  test("update with null removes the assignments") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask).toAttachmentId
      oid <- createObservation(pi, pid, (aid, mosMask))
      _   <- assertObservation(pi, pid, oid, (aid, mosMask))
      _   <- updateObservation(pi, oid, UpdateInput.Null)
    } yield ()
  }

  test("update with empty list removes the assignments") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, mosMask).toAttachmentId
      oid <- createObservation(pi, pid, (aid, mosMask))
      _   <- assertObservation(pi, pid, oid, (aid, mosMask))
      _   <- updateObservation(pi, oid, updateEmpty)
    } yield ()
  }

  test("update without attachments leaves the assignments") {
    for {
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid)
      aid <- insertAttachment(pi, pid, mosMask).toAttachmentId
      _   <- updateObservation(pi, oid, updateFile1(aid))
      _   <- updateObservation(pi, oid, skipFile1(aid))
    } yield ()
  }

  test("can successfully update an observation with 2 assignments") {
    for {
      pid  <- createProgramAs(pi)
      oid  <- createObservationAs(pi, pid)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      _    <- updateObservation(pi, oid, updateBothFiles(aid1, aid2))
    } yield ()
  }

  test("can successfully update multiple observations at a time") {
    for {
      pid  <- createProgramAs(pi)
      aid0 <- insertAttachment(pi, pid, preImaging).toAttachmentId
      oid1 <- createObservationAs(pi, pid)
      oid2 <- createObservation(pi, pid, (aid0, preImaging))
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      _    <- updateMultipleObservations(pi, updateBothFiles(aid1, aid2), oid1, oid2)
    } yield ()
  }

  test("can delete an assigned attachment") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid  <- createObservation(pi, pid, (aid1, mosMask), (aid2, finder))
      _    <- assertObservation(pi, pid, oid, (aid1, mosMask), (aid2, finder))
      _    <- deleteAttachment(pi, aid1).expectOk
      _    <- assertObservation(pi, pid, oid, (aid2, finder))
    } yield ()
  }

  test("can delete an observation with assigned attachments") {
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, mosMask), (aid2, finder))
      oid2 <- createObservation(pi, pid, (aid2, finder))
      _    <- assertObservation(pi, pid, oid1, (aid1, mosMask), (aid2, finder))
      _    <- assertObservation(pi, pid, oid2, (aid2, finder))
      _    <- deleteObservation(pi, oid1)
    } yield ()
  }

  test("cloning an observation clones the assignments"){
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, mosMask), (aid2, finder))
      _    <- assertObservation(pi, pid, oid1, (aid1, mosMask), (aid2, finder))
      oid2 <- cloneObservationAs(pi, oid1)
      _    <- assertObservation(pi, pid, oid2, (aid1, mosMask), (aid2, finder))
    } yield ()
  }

  test("new attachments can be assigned when cloning an observation"){
    for {
      pid  <- createProgramAs(pi)
      aid1 <- insertAttachment(pi, pid, mosMask).toAttachmentId
      aid2 <- insertAttachment(pi, pid, finder).toAttachmentId
      oid1 <- createObservation(pi, pid, (aid1, mosMask))
      _    <- assertObservation(pi, pid, oid1, (aid1, mosMask))
      oid2 <- cloneObservationWithAttachments(pi, oid1, aid2)
      _    <- assertObservation(pi, pid, oid2, (aid2, finder))
    } yield ()
  }

  test("cannot create an observation with an attachment from a different program") {
    for {
      pid1 <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid1, mosMask).toAttachmentId
      pid2 <- createProgramAs(pi)
      _    <- createObservationWithError(pi, pid2, ObsAttachmentAssignmentService.ForeignKeyViolationMessage(pid2, NonEmptyList.one(aid)), (aid, mosMask))
    } yield ()
  }

  test("cannot assign an attachment from a different program") {
    for {
      pid1 <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid1, mosMask).toAttachmentId
      pid2 <- createProgramAs(pi)
      oid  <- createObservationAs(pi, pid2)
      _    <- updateObservation(pi, oid, updateFile1(aid), ObsAttachmentAssignmentService.ForeignKeyViolationMessage(pid2, NonEmptyList.one(aid)).some)
    } yield ()
  }

  test("attachments from another program cannot be assigned when cloning an observation"){
    for {
      pid1 <- createProgramAs(pi)
      aid  <- insertAttachment(pi, pid1, mosMask).toAttachmentId
      pid2 <- createProgramAs(pi)
      oid  <- createObservationAs(pi, pid2)
      _    <- assertObservation(pi, pid2, oid)
      _    <- cloneObservationWithAttachmentsWithError(pi, oid, ObsAttachmentAssignmentService.ForeignKeyViolationMessage(pid2, NonEmptyList.one(aid)), aid)
    } yield ()
  }

  test("cannot create an observation with a science attachment") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, science).toAttachmentId
      _   <- createObservationWithError(pi, pid, ObsAttachmentAssignmentService.NonObservationAttachmentMessage(NonEmptyList.one(aid)), (aid, science))
    } yield ()
  }

  test("cannot assign a team attachment") {
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, team).toAttachmentId
      oid <- createObservationAs(pi, pid)
      _   <- updateObservation(pi, oid, updateFile1(aid), ObsAttachmentAssignmentService.NonObservationAttachmentMessage(NonEmptyList.one(aid)).some)
    } yield ()
  }

  test("custom SED attachments cannot be assigned when cloning an observation"){
    for {
      pid <- createProgramAs(pi)
      aid <- insertAttachment(pi, pid, customSed).toAttachmentId
      oid <- createObservationAs(pi, pid)
      _   <- assertObservation(pi, pid, oid)
      _   <- cloneObservationWithAttachmentsWithError(pi, oid, ObsAttachmentAssignmentService.NonObservationAttachmentMessage(NonEmptyList.one(aid)), aid)
    } yield ()
  }
}
