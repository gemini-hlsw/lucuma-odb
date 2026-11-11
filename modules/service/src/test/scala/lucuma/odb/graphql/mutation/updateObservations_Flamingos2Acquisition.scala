// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations

/**
 * The explicit acquisition type and its sky offset, which the Flamingos 2 long
 * slit and MOS modes handle alike.
 */
class updateObservations_Flamingos2Acquisition
  extends OdbSuite
     with ObservingModeSetupOperations
     with UpdateObservationsOps:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] = List(pi)

  private case class Mode(field: String, required: String)

  private val Modes: List[Mode] =
    List(
      Mode("flamingos2LongSlit", "disperser: R1200_JH, filter: JH, fpu: LONG_SLIT_1"),
      Mode("flamingos2Mos",      "disperser: R1200_JH, filter: JH, customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }")
    )

  private val AcquisitionSelection: String =
    """
      acquisition {
        explicitAcquisitionType
        skyOffset { p { microarcseconds } q { microarcseconds } }
      }
    """

  private def create(mode: Mode, acquisition: String = ""): IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), s"${mode.field}: { ${mode.required} $acquisition }")
    yield oid

  private def update(mode: Mode, oid: Observation.Id, acquisition: String): String =
    updateObservationsMutation(
      oid,
      s"observingMode: { ${mode.field}: { acquisition: { $acquisition } } }",
      s"observations { observingMode { ${mode.field} { $AcquisitionSelection } } }"
    )

  private def updated(mode: Mode, acquisition: Json): Either[List[String], Json] =
    Json.obj(
      "updateObservations" -> Json.obj(
        "observations" -> Json.arr(
          Json.obj("observingMode" -> Json.obj(mode.field -> Json.obj("acquisition" -> acquisition)))
        )
      )
    ).asRight

  private val Automatic: Json =
    json"""{ "explicitAcquisitionType": null, "skyOffset": null }"""

  private val Bright: Json =
    json"""{ "explicitAcquisitionType": "BRIGHT", "skyOffset": null }"""

  private def faint(pµas: Long, qµas: Long): Json =
    json"""
      {
        "explicitAcquisitionType": "FAINT",
        "skyOffset": { "p": { "microarcseconds": $pµas }, "q": { "microarcseconds": $qµas } }
      }
    """

  Modes.foreach: mode =>

    test(s"${mode.field}: FAINT with a sky offset, then BRIGHT drops the offset, then null reverts to automatic"):
      for
        oid <- create(mode)
        _   <- expect(pi, update(mode, oid, "explicitAcquisitionType: null"), updated(mode, Automatic))
        _   <- expect(
                 pi,
                 update(mode, oid, "explicitAcquisitionType: FAINT, skyOffset: { p: { arcseconds: 5 }, q: { arcseconds: 0 } }"),
                 updated(mode, faint(5_000_000L, 0L))
               )
        _   <- expect(pi, update(mode, oid, "explicitAcquisitionType: BRIGHT"), updated(mode, Bright))
        _   <- expect(pi, update(mode, oid, "explicitAcquisitionType: null"), updated(mode, Automatic))
      yield ()

    test(s"${mode.field}: editing another acquisition field leaves the type and sky offset alone"):
      for
        oid <- create(mode)
        _   <- expect(
                 pi,
                 update(mode, oid, "explicitAcquisitionType: FAINT, skyOffset: { p: { arcseconds: 0 }, q: { arcseconds: -7 } }"),
                 updated(mode, faint(0L, -7_000_000L))
               )
        _   <- expect(pi, update(mode, oid, "explicitFilter: H"), updated(mode, faint(0L, -7_000_000L)))
      yield ()

    test(s"${mode.field}: created with an explicit type, and cloned with it"):
      val acquisition: String =
        "acquisition: { explicitAcquisitionType: FAINT, skyOffset: { p: { arcseconds: 3 }, q: { arcseconds: 4 } } }"
      for
        oid <- create(mode, acquisition)
        cid <- cloneObservationAs(pi, oid)
        _   <- expect(pi, update(mode, cid, "explicitFilter: H"), updated(mode, faint(3_000_000L, 4_000_000L)))
      yield ()

    test(s"${mode.field}: FAINT requires a sky offset"):
      create(mode).flatMap: oid =>
        expect(
          pi,
          update(mode, oid, "explicitAcquisitionType: FAINT"),
          List(s"Argument 'input.SET.observingMode.${mode.field}.acquisition' is invalid: 'explicitAcquisitionType' FAINT requires a 'skyOffset'.").asLeft
        )

    test(s"${mode.field}: a sky offset requires FAINT"):
      create(mode).flatMap: oid =>
        expect(
          pi,
          update(mode, oid, "explicitAcquisitionType: BRIGHT, skyOffset: { p: { arcseconds: 5 }, q: { arcseconds: 0 } }"),
          List(s"Argument 'input.SET.observingMode.${mode.field}.acquisition' is invalid: 'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT.").asLeft
        )
