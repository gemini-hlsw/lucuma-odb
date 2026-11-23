// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.effect.Resource
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.service.AttachmentFileService
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder

class customMaskInUse
    extends OdbSuiteWithS3
    with query.ExecutionTestSupportForGmos
    with CustomMaskOps:

  private val client: Client[IO] = JavaNetClientBuilder[IO].create

  private def deleteAttachment(user: User, aid: Attachment.Id): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap: svr =>
        client.run(
          Request[IO](
            method  = Method.DELETE,
            uri     = svr.baseUri / "attachment" / aid.toString,
            headers = Headers(auth)
          )
        )

  private def expectDelete(user: User, aid: Attachment.Id, status: Status, body: String): IO[Unit] =
    deleteAttachment(user, aid)
      .use(r => r.body.through(utf8.decode).compile.string.map((r.status, _)))
      .assertEquals((status, body))

  private def expectDeleted(user: User, aid: Attachment.Id): IO[Unit] =
    deleteAttachment(user, aid).use(r => IO.pure(r.status)).assertEquals(Status.Ok)

  private def maskedStepObservation(a: Attachment.Id, p: Program.Id): IO[Observation.Id] =
    for
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      m  = s"""{ attachmentId: "$a", slitWidth: $SlitWidth }"""
      _ <- query(pi, mutation(Instrument.GmosNorth, input(o, SequenceType.Science, atomInput("Masked", gmosStep(m)))))
    yield o

  private def daytimeCalibration(a: Attachment.Id, cal: Program.Id): IO[Observation.Id] =
    query(
      user  = staff,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${cal.asJson}
            SET: {
              observingMode: {
                daytimeCalibration: {
                  instrument: GMOS_NORTH
                  calibrationSets: [{
                    kind: DARK
                    explicitCount: 1
                    gmosNorth: {
                      exposure: { seconds: 10 }
                      readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: FAST }
                      dtax: ZERO
                      roi: FULL_FRAME
                      fpu: { customMask: { attachmentId: ${a.asJson}, slitWidth: $SlitWidth } }
                    }
                  }]
                }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  private def daytimeMask(o: Observation.Id): IO[Json] =
    query(
      user  = staff,
      query = s"""
        query {
          observation(observationId: ${o.asJson}) {
            observingMode { daytimeCalibration { calibrationSets { gmosNorth { fpu { customMask { attachmentId slitWidth } } } } } }
          }
        }
      """
    ).map:
      _.hcursor
        .downFields("observation", "observingMode", "daytimeCalibration", "calibrationSets")
        .downArray
        .downFields("gmosNorth", "fpu", "customMask")
        .require[Json]

  private def expectInUse(user: User, aid: Attachment.Id): IO[Unit] =
    expectDelete(user, aid, Status.Conflict, AttachmentFileService.AttachmentInUseMsg)

  test("a MOS mask referenced by a step cannot be deleted"):
    for
      p <- createProgram
      a <- insertMosMaskAttachment(p, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth)
      _ <- maskedStepObservation(a, p)
      _ <- expectInUse(pi, a)
    yield ()

  test("the mask of a MOS observation without stored steps can be deleted"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      a <- insertMosMaskAttachment(p, "GN2025AQ001-04_ODF.fits", Instrument.GmosNorth)
      _ <- createObservationWithModeAs(pi, p, List(t), gmosNorthMosMode(Some(a)))
      _ <- expectDeleted(pi, a)
    yield ()

  test("a daytime calibration set does not block the delete"):
    for
      p <- createProgram
      a <- insertMosMaskAttachment(p, "GN2025AQ001-06_ODF.fits", Instrument.GmosNorth)
      o <- daytimeCalibration(a, p)
      _ <- expectDeleted(pi, a)
      m <- daytimeMask(o)
    yield assertEquals(m, json"""{ "attachmentId": null, "slitWidth": "CUSTOM_WIDTH_0_50" }""")

  test("a daytime calibration with a stored step through the mask blocks the delete"):
    val step = (a: Attachment.Id) => s"""
      {
        instrumentConfig: {
          exposure: { seconds: 10 }
          readout: { xBin: ONE, yBin: ONE, ampCount: TWELVE, ampGain: LOW, ampReadMode: FAST }
          dtax: ZERO
          roi: FULL_FRAME
          fpu: { customMask: { attachmentId: ${a.asJson}, slitWidth: $SlitWidth } }
        }
        stepConfig: { dark: true }
        observeClass: DAY_CAL
      }
    """
    for
      p <- createProgram
      a <- insertMosMaskAttachment(p, "GN2025AQ001-07_ODF.fits", Instrument.GmosNorth)
      o <- daytimeCalibration(a, p)
      _ <- query(staff, mutation(Instrument.GmosNorth, input(o, SequenceType.Science, atomInput("Mask", step(a)))))
      _ <- expectInUse(pi, a)
    yield ()
