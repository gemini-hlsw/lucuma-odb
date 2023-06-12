// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ObservingModeType

class recordStep extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ): IO[(Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (oid, vid)

  test("recordStep") {

    val gn: String =
    """
      instrument: {
        exposure: {
          seconds: 1200
        },
        readout: {
          xBin: ONE,
          yBin: ONE,
          ampCount: TWELVE,
          ampGain: LOW,
          ampRead: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        gratingConfig: {
          grating: B1200_G5301,
          order: ONE,
          wavelength: {
            nanometers: 600
          }
        },
        fpu: {
          builtin: LONG_SLIT_0_50
        }
      }
    """

    val science: String =
    """
      stepConfig: {
        science: {
          offset: {
             p: {
               arcseconds: 0
             },
             q: {
               arcseconds: 10
             }
          },
          guiding: ENABLED
        }
      }
    """


    for {
      ids <- recordVisit(ObservingModeType.GmosNorthLongSlit, staff)
      (_, vid) = ids
      _ <- recordStepAs(staff, vid, Instrument.GmosNorth, gn, science)
    } yield ()

  }


}
