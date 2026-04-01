// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.foldable.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

// https://app.shortcut.com/lucuma/story/6344
class ShortCut_6344 extends OdbSuite with query.ExecutionTestSupportForGmos:

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
        v  <- recordVisitAs(serviceUser, o)
        c0 <- setupCount(p, o)  // 5.8 ~ hours => 3 setups
        s  <- scienceStepIds(serviceUser, o)
        _  <- addEndStepEvent(s(0), v) // arc
        _  <- addEndStepEvent(s(1), v) // flat

        _  <- addEndStepEvent(s(2), v)
        c1 <- setupCount(p, o)  // ~5.3 hours => 3 setups

        _  <- addEndStepEvent(s(3), v)
        c2 <- setupCount(p, o)  // ~4.8 hours => 3 setups

        _  <- addEndStepEvent(s(4), v) // arc
        _  <- addEndStepEvent(s(5), v) // flat

        _  <- addEndStepEvent(s(6), v)
        c3 <- setupCount(p, o)  // ~4.3 hours => 2 setups

        -  <- s.drop(6).traverse_(sid => addEndStepEvent(sid, v))
        c4 <- setupCount(p, o)

      yield (c0, c1, c2, c3, c4),
      (3, 3, 3, 2, 0)
    )

