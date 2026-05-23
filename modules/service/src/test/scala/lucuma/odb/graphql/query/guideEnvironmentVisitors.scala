// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class guideEnvironmentVisitors extends ExecutionTestSupport with GuideEnvironmentSuite:

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createVisitorModeObservationAs(user, pid, VisitorObservingModeType.VisitorNorth, tids*)

  private def queryResult(posAngleDegrees: BigDecimal, gaiaId: String): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": ${posAngleDegrees.setScale(6)}
            },
            "guideTargets": [
              {
                "name": $gaiaId,
                "probe": "PWFS2"
              }
            ]
          }
        }
      }
    }
    """.asRight

  private val guideQuery: Observation.Id => String =
    oid => s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            guideEnvironment {
              posAngle { degrees }
              guideTargets {
                name
                probe
              }
            }
          }
        }
      }
    """

  private def setupFor(mode: VisitorObservingModeType): IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createVisitorModeObservationAs(pi, p, mode, t)
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
    yield o

  test("maroon-x ags for sidereal target with pwfs2"):
    setupFor(VisitorObservingModeType.MaroonX).flatMap: oid =>
      expect(
        pi,
        guideQuery(oid),
        expected = queryResult(350.00, "Gaia DR3 3219118090462918016")
      )

  test("generic visitor ags for sidereal target with pwfs2"):
    setupFor(VisitorObservingModeType.VisitorNorth).flatMap: oid =>
      expect(
        pi,
        guideQuery(oid),
        expected = queryResult(240.00, "Gaia DR3 3219118090462918016")
      )

  private def setupForNonsidereal(mode: VisitorObservingModeType): IO[Observation.Id] =
    for
      p   <- createProgramAs(pi)
      eph  = createNonsiderealEphemeris
      t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target")
      o   <- createVisitorModeObservationAs(pi, p, mode, t)
      _   <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
    yield o

  test("maroon-x ags for non sidereal target with pwfs2"):
    setupForNonsidereal(VisitorObservingModeType.MaroonX).flatMap: oid =>
      expect(
        pi,
        guideQuery(oid),
        expected = queryResult(350.00, "Gaia DR3 3219118090462918016")
      )

  test("generic visitor ags for non sidereal target with pwfs2"):
    setupForNonsidereal(VisitorObservingModeType.VisitorNorth).flatMap: oid =>
      expect(
        pi,
        guideQuery(oid),
        expected = queryResult(240.00, "Gaia DR3 3219118090462918016")
      )
