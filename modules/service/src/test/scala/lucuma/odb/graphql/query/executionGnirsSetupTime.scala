// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

/**
 * GNIRS setup costs per mode, as confirmed by the GNIRS team: long slit 15 minutes and IFU 20
 * minutes as in the OCS, imaging reduced to 10 minutes. Reacquisition is 6 minutes for all.
 */
class executionGnirsSetupTime extends ExecutionTestSupportForGnirs:

  private val Reacquisition: TimeSpan = 6.minTimeSpan

  private def setup(pid: Program.Id, oid: Observation.Id): IO[(TimeSpan, TimeSpan)] =
    runObscalcUpdate(pid, oid) *> query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution {
              digest {
                value {
                  setup {
                    full { microseconds }
                    reacquisition { microseconds }
                  }
                }
              }
            }
          }
        }
      """
    ).map: js =>
      val setup: Json =
        js.hcursor
          .downField("observation")
          .downField("execution")
          .downField("digest")
          .downField("value")
          .downField("setup")
          .focus
          .getOrElse(Json.Null)
      (
        setup.hcursor.downField("full").as[TimeSpan].fold(throw _, identity),
        setup.hcursor.downField("reacquisition").as[TimeSpan].fold(throw _, identity)
      )

  private def setupFor(create: (User, Program.Id, Target.Id) => IO[Observation.Id]): IO[(TimeSpan, TimeSpan)] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- create(pi, p, t)
      s <- setup(p, o)
    yield s

  test("long slit setup is 15 minutes"):
    assertIO(setupFor(createGnirsLongSlitObservationAs(_, _, _)), (15.minTimeSpan, Reacquisition))

  test("IFU setup is 20 minutes"):
    assertIO(setupFor(createGnirsIfuObservationAs(_, _, _)), (20.minTimeSpan, Reacquisition))

  test("imaging setup is 10 minutes"):
    assertIO(setupFor(createGnirsImagingObservationAs(_, _, _)), (10.minTimeSpan, Reacquisition))
