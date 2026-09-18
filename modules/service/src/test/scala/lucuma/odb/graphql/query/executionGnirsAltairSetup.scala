// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.odb.json.time.decoder.given

/**
 * The setup time an Altair configuration costs: the laser modes pay the longer GNIRS setup. The
 * AOWFS patrol field is only a few tens of arcseconds across, so these share
 * `guideEnvironmentGnirsAltair`'s candidate table.
 */
class executionGnirsAltairSetup extends ExecutionTestSupportForGnirs
                                      with GuideEnvironmentSuite:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  private val NgsSetup: TimeSpan = 15.minTimeSpan
  private val LgsSetup: TimeSpan = 25.minTimeSpan

  private def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            WHERE: { id: { EQ: "$oid" } }
            SET: { targetEnvironment: { altair: $altair } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def observationWithAltair(altair: String, guideStar: Boolean): IO[(Program.Id, Observation.Id)] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltair(o, altair)
      _ <- IO.whenA(guideStar)(setGuideTargetName(pi, o, defaultTargetName.some))
    yield (p, o)

  private def setupTime(pid: Program.Id, oid: Observation.Id): IO[TimeSpan] =
    runObscalcUpdate(pid, oid) *>
      query(
        pi,
        s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  value {
                    setup { full { seconds } }
                  }
                }
              }
            }
          }
        """
      ).map: json =>
        json
          .hcursor
          .downFields("observation", "execution", "digest", "value", "setup", "full")
          .require[TimeSpan]

  test("the laser modes cost the longer setup"):
    for
      (ngsPid, ngsOid)     <- observationWithAltair("{ mode: NGS }", guideStar = true)
      ngs                  <- setupTime(ngsPid, ngsOid)
      (lgsPid, lgsOid)     <- observationWithAltair("{ mode: LGS }", guideStar = true)
      lgs                  <- setupTime(lgsPid, lgsOid)
      (lgsP1Pid, lgsP1Oid) <- observationWithAltair("{ mode: LGS_P1 }", guideStar = false)
      lgsP1                <- setupTime(lgsP1Pid, lgsP1Oid)
    yield
      assertEquals(ngs, NgsSetup)
      assertEquals(lgs, LgsSetup)
      assertEquals(lgsP1, LgsSetup)
