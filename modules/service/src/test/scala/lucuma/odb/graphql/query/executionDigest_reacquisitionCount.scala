// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionDigest_reacquisitionCount extends OdbSuite with ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    // ~5.8 hours of science
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(11)
    )

  def counts(pid: Program.Id, oid: Observation.Id): IO[(Int, Int)] =
    runObscalcUpdate(pid, oid) *>
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution { digest { value { estimate { setupCount reacquisitionCount } } } }
          }
        }
      """
    ).map: json =>
      val c = json.hcursor.downFields("observation", "execution", "digest", "value", "estimate")
      (c.downField("setupCount").require[Int], c.downField("reacquisitionCount").require[Int])

  def setGuideProbe(oid: Observation.Id, probe: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { targetEnvironment: { explicitGuideProbe: $probe } }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  test("OIWFS spectroscopy has no reacquisitions"):
    assertIO(
      for
        p <- createProgramWithNonPartnerPi(pi)
        t <- createTargetAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        c <- counts(p, o)
      yield c,
      (3, 0)
    )

  test("PWFS spectroscopy: setup every 2 hours, reacquisition at each hour between"):
    assertIO(
      for
        p <- createProgramWithNonPartnerPi(pi)
        t <- createTargetAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- setGuideProbe(o, "PWFS2")
        c <- counts(p, o)  // floor(5.8 / 2) + 1 = 3, floor(6.8 / 2) = 3
      yield c,
      (3, 3)
    )

  test("non-splittable PWFS spectroscopy still reacquires"):
    assertIO(
      for
        p <- createProgramWithNonPartnerPi(pi)
        t <- createTargetAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- setGuideProbe(o, "PWFS1")
        _ <- setIsSplittableAs(pi, o, isSplittable = false)
        c <- counts(p, o)
      yield c,
      (1, 3)
    )

  test("nonsidereal spectroscopy defaults to PWFS"):
    assertIO(
      for
        p <- createProgramWithNonPartnerPi(pi)
        t <- createNonsiderealTargetAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        c <- counts(p, o)
      yield c,
      (3, 3)
    )

  test("PWFS imaging has no reacquisitions"):
    for
      p <- createProgramWithNonPartnerPi(pi)
      t <- createTargetAs(pi, p)
      o <- createGmosNorthImagingObservationAs(pi, p, t)
      _ <- setGuideProbe(o, "PWFS2")
      c <- counts(p, o)
    yield assertEquals(c._2, 0)
