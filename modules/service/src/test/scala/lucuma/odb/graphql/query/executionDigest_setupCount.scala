// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionDigest_setupCount extends OdbSuite with ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    // 5.5 hours => 3 setups
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(11)
    )

  def digestQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution { digest { value { setupCount } } }
        }
      }
    """

  def setupCount(pid: Program.Id, oid: Observation.Id): IO[Int] =
    runObscalcUpdate(pid, oid) *>
    query(
      pi,
      digestQuery(oid)
    ).map: json =>
      json.hcursor.downFields("observation", "execution", "digest", "value", "setupCount").require[Int]

  test("multiple setups"):
    assertIO(
      for
        p  <- createProgramWithNonPartnerPi(pi)
        t  <- createTargetAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        c0 <- setupCount(p, o)  // 5.8 ~ hours => 3 setups

        _  <- setIsSplittableAs(pi, o, isSplittable = false)
        c1 <- setupCount(p, o) // 1

      yield (c0, c1),
      (3, 1)
    )