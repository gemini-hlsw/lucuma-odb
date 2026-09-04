// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.time.decoder.given

class executionDigest_gmosMosSetup extends OdbSuite with ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(11)
    )

  private val MosSetup: TimeSpan      = 18.minTimeSpan
  private val Reacquisition: TimeSpan = 5.minTimeSpan

  private def setupTimes(pid: Program.Id, oid: Observation.Id): IO[(TimeSpan, TimeSpan)] =
    runObscalcUpdate(pid, oid) *>
      query(
        pi,
        s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  value {
                    setup {
                      full {
                        seconds
                      }
                      reacquisition {
                        seconds
                      }
                    }
                  }
                }
              }
            }
          }
        """
      ).map: json =>
        val setup = json.hcursor.downFields("observation", "execution", "digest", "value", "setup")
        (setup.downField("full").require[TimeSpan], setup.downField("reacquisition").require[TimeSpan])

  test("GMOS North MOS setup and reacquisition time"):
    assertIO(
      for
        p             <- createProgramWithNonPartnerPi(pi)
        t             <- createTargetAs(pi, p)
        o             <- createGmosNorthMosObservationAs(pi, p, List(t))
        (full, reacq) <- setupTimes(p, o)
      yield (full, reacq),
      (MosSetup, Reacquisition)
    )

  test("GMOS South MOS setup and reacquisition time"):
    assertIO(
      for
        p             <- createProgramWithNonPartnerPi(pi)
        t             <- createTargetAs(pi, p)
        o             <- createGmosSouthMosObservationAs(pi, p, List(t))
        (full, reacq) <- setupTimes(p, o)
      yield (full, reacq),
      (MosSetup, Reacquisition)
    )
