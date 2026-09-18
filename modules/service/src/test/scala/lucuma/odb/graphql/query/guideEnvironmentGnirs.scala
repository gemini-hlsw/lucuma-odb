// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.service.GuideService
import lucuma.odb.service.Services

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

// Behind Altair the guide probe is fixed by the mode: NGS and LGS guide with the AOWFS, LGS+P1
// with PWFS1.  The AOWFS patrol field is only a few tens of arcseconds across, so these tests
// need their own candidates.
class guideEnvironmentGnirsAltair extends ExecutionTestSupportForGnirs
                                        with GuideEnvironmentSuite:

  override val gaiaResponseString: String = GaiaVoTables.altairCandidates

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  // The only candidate inside the AOWFS patrol field.
  private val aowfsStarName: String = "Gaia DR3 3219118090462917888"

  // The only candidate far enough out for the PWFS1 probe arm to clear the science field.
  private val pwfs1StarName: String = "Gaia DR3 3219118090462900000"

  // Where the default star sits relative to the science target, and the R estimated from its
  // Gaia G, BP and RP.
  private val defaultStarSeparation: Angle = Angle.fromDoubleArcseconds(218.122)

  private val defaultStarRBrightness: BigDecimal = BigDecimal("13.941")

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

  private def observationWithAltair(altair: String): IO[Observation.Id] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationAs(pi, p, List(t))
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltair(o, altair)
    yield o

  private def altairGuideEnvironmentQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            altair { mode }
            guideProbe
            guideEnvironment {
              guideTargets {
                name
                probe
              }
            }
          }
        }
      }
    """

  private def altairGuideEnvironmentResult(mode: String, probe: String, name: String): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "altair": {
            "mode": $mode
          },
          "guideProbe": $probe,
          "guideEnvironment": {
            "guideTargets": [
              {
                "name": $name,
                "probe": $probe
              }
            ]
          }
        }
      }
    }
    """.asRight

  private def resolveStoredGuideStar(oid: Observation.Id): IO[Option[GuideService.ResolvedGuideStar]] =
    withServices(pi): services =>
      Services.asSuperUser(services.guideService.resolveStoredGuideStar(oid)).flatMap(_.get)

  test("NGS guides with the Altair AOWFS"):
    observationWithAltair("{ mode: NGS }").flatMap: oid =>
      expect(
        pi,
        altairGuideEnvironmentQuery(oid),
        expected = altairGuideEnvironmentResult("NGS", "ALTAIR_AOWFS", aowfsStarName)
      )

  test("LGS guides with the Altair AOWFS"):
    observationWithAltair("{ mode: LGS }").flatMap: oid =>
      expect(
        pi,
        altairGuideEnvironmentQuery(oid),
        expected = altairGuideEnvironmentResult("LGS", "ALTAIR_AOWFS", aowfsStarName)
      )

  test("LGS+P1 guides with PWFS1"):
    observationWithAltair("{ mode: LGS_P1 }").flatMap: oid =>
      expect(
        pi,
        altairGuideEnvironmentQuery(oid),
        expected = altairGuideEnvironmentResult("LGS_P1", "PWFS1", pwfs1StarName)
      )

  // The stored star is resolved by id, so the Gaia stub answers with the whole table and the
  // first row stands in for the star that was asked for.
  test("the stored guide star resolves to its separation and R magnitude"):
    for
      oid      <- observationWithAltair("{ mode: NGS }")
      _        <- setGuideTargetName(pi, oid, defaultTargetName.some)
      resolved <- resolveStoredGuideStar(oid)
    yield
      assertEquals(resolved.map(_.name.value.value), defaultTargetName.some)
      val separation: Angle = resolved.map(_.separation).get
      assert(
        (Angle.signedDecimalArcseconds.get(separation) - Angle.signedDecimalArcseconds.get(defaultStarSeparation)).abs < BigDecimal("0.1"),
        s"unexpected separation $separation"
      )
      val rBrightness: BigDecimal = resolved.flatMap(_.rBrightness).map(_.value.value).get
      assert((rBrightness - defaultStarRBrightness).abs < BigDecimal("0.01"), s"unexpected R magnitude $rBrightness")

  test("no stored guide star resolves to nothing"):
    for
      oid      <- observationWithAltair("{ mode: NGS }")
      resolved <- resolveStoredGuideStar(oid)
    yield assertEquals(resolved, none)
