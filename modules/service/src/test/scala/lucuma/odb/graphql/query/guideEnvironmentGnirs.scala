// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class guideEnvironmentGnirs extends ExecutionTestSupportForGnirs
                                  with GuideEnvironmentSuite:

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  private def gnirsPwfs2Result(
    title:           String,
    posAngleDegrees: BigDecimal = BigDecimal("270.000000")
  ): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "title": $title,
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": $posAngleDegrees
            },
            "guideTargets": [
              {
                "name": "Gaia DR3 3219118090462918016",
                "probe": "PWFS2",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
                        {
                          "band": "GAIA"
                        },
                        {
                          "band": "GAIA_RP"
                        }
                      ]
                    }
                  }
                },
                "sidereal": {
                  "catalogInfo": {
                    "name": "GAIA",
                    "id": "3219118090462918016",
                    "objectType": null
                  },
                  "epoch": "J2016.000",
                  "ra": {
                    "microseconds": 20782433789,
                    "hms": "05:46:22.433789",
                    "hours": 5.772898274722222222222222222222222,
                    "degrees": 86.59347412083333333333333333333333
                  },
                  "dec": {
                    "dms": "-00:08:52.645460",
                    "degrees": 359.8520429277778,
                    "microarcseconds": 1295467354540
                  },
                  "radialVelocity": {
                    "metersPerSecond": 10090.042000,
                    "centimetersPerSecond": 1009004,
                    "kilometersPerSecond": 10.090042
                  },
                  "properMotion": {
                    "ra": {
                      "microarcsecondsPerYear": 438,
                      "milliarcsecondsPerYear": 0.438
                    },
                    "dec": {
                      "microarcsecondsPerYear": -741,
                      "milliarcsecondsPerYear": -0.741
                    }
                  },
                  "parallax": {
                    "microarcseconds": 2432,
                    "milliarcseconds": 2.432
                  }
                },
                "nonsidereal": null
              }
            ]
          }
        }
      }
    }
    """.asRight

  test("sidereal target - AGS picks best star with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = gnirsPwfs2Result("V1647 Orionis"))

  // GNIRS imaging guides with PWFS2, like the long slit. The PWFS patrol field and
  // the 20" protected radius are shared, so AGS selects the same star. The imaging
  // field is symmetric under a 180 degree flip, so 90 and 270 are equally valid and
  // the random spiral dither breaks the tie; the test-support layer pins the dither
  // seed (DefaultImagingDitherSeed) so the resulting angle is deterministic.
  test("imaging sidereal target - AGS picks best star with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsImagingObservationAs(pi, p, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = gnirsPwfs2Result("V1647 Orionis", BigDecimal("90.000000")))

  // GNIRS IFU guides with PWFS2, like the long slit, and selects the same star. Its
  // small science area shifts the optimal position angle (240 vs the long slit's 270).
  test("IFU sidereal target - AGS picks best star with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGnirsIfuObservationAs(pi, p, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = gnirsPwfs2Result("V1647 Orionis", BigDecimal("240.000000")))

  test("nonsidereal target with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p   <- createProgramAs(pi)
        eph  = createNonsiderealEphemeris
        t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target")
        o   <- createObservationAs(pi, p, List(t))
        _   <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = gnirsPwfs2Result("Nonsidereal Target"))

  // Daytime pinhole calibrations have no guide stars: AGS is skipped and an
  // empty guide environment is returned, with the position angle taken from the
  // observation's (default, unbounded) position angle constraint.
  val emptyGuideEnvironmentResult: Either[List[String], Json] =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": 0.000000
            },
            "guideTargets": []
          }
        }
      }
    }
    """.asRight

  test("daytime pinhole calibration - empty guide environment, AGS not invoked"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setObservationCalibrationRole(List(o), CalibrationRole.DaytimePinhole)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = emptyGuideEnvironmentResult)
