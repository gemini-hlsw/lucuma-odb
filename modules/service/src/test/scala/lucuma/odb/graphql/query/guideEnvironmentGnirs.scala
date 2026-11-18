// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.ags.GuideStarId
import lucuma.ags.GuideStarName
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.service.GuideService
import lucuma.odb.service.Services
import lucuma.odb.util.Codecs.*
import skunk.implicits.*

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

  // Where the AOWFS star sits relative to the science target, and the R estimated from its
  // Gaia G, BP and RP.
  private val aowfsStarSeparation: Angle = Angle.fromDoubleArcseconds(12.0)

  private val aowfsStarRBrightness: BigDecimal = BigDecimal("11.736")

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

  private def resolveGuideStar(oid: Observation.Id): IO[GuideService.GuideStarResolution] =
    withServices(pi): services =>
      Services.asSuperUser(services.guideService.resolveGuideStar(oid)).flatMap(_.get)

  // Behind the mutation's back, which would reject a star the generator rules out.
  private def storeGuideStarName(oid: Observation.Id, name: String): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_observation
             SET c_guide_target_name = $guide_target_name
           WHERE c_observation_id = $observation_id
        """.command
      )(GuideStarName.unsafeFrom(name), oid).void

  private def assertResolvedStar(
    resolution:  GuideService.GuideStarResolution,
    name:        String,
    separation:  Angle,
    rBrightness: BigDecimal
  ): Unit =
    val resolved: Option[GuideService.ResolvedGuideStar] = resolution.star
    assertEquals(resolved.flatMap(_.guideStarId.catalogName).map(_.value.value), name.some)
    val actualSeparation: Angle = resolved.map(_.separation).get
    assert(
      (Angle.signedDecimalArcseconds.get(actualSeparation) - Angle.signedDecimalArcseconds.get(separation)).abs < BigDecimal("0.1"),
      s"unexpected separation $actualSeparation"
    )
    val actualRBrightness: BigDecimal = resolved.flatMap(_.rBrightness).map(_.value.value).get
    assert((actualRBrightness - rBrightness).abs < BigDecimal("0.01"), s"unexpected R magnitude $actualRBrightness")

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

  test("the stored guide star resolves to its separation and R magnitude"):
    for
      oid        <- observationWithAltair("{ mode: NGS }")
      _          <- setCatalogGuideTarget(pi, oid, aowfsStarName.some)
      resolution <- resolveGuideStar(oid)
    yield assertResolvedStar(resolution, aowfsStarName, aowfsStarSeparation, aowfsStarRBrightness)

  test("without a stored guide star the AGS pick resolves"):
    for
      oid        <- observationWithAltair("{ mode: NGS }")
      resolution <- resolveGuideStar(oid)
    yield assertResolvedStar(resolution, aowfsStarName, aowfsStarSeparation, aowfsStarRBrightness)

  // The fixture stars have no proper motion, so the pick at "now" matches the one at the fixed time.
  test("without an observation time the AGS pick resolves at the current time"):
    for
      p          <- createProgramAs(pi)
      t          <- createTargetWithProfileAs(pi, p)
      o          <- createObservationAs(pi, p, List(t))
      _          <- setAltair(o, "{ mode: NGS }")
      resolution <- resolveGuideStar(o)
    yield assertResolvedStar(resolution, aowfsStarName, aowfsStarSeparation, aowfsStarRBrightness)

  // LGS+P1 guides with PWFS1, whose probe arm would vignette a star as close in as the AOWFS one,
  // so the stored star is not usable.
  test("a stored star the current parameters rule out resolves as unusable"):
    for
      oid        <- observationWithAltair("{ mode: LGS_P1 }")
      _          <- setCatalogGuideTarget(pi, oid, pwfs1StarName.some)
      _          <- storeGuideStarName(oid, aowfsStarName)
      resolution <- resolveGuideStar(oid)
    yield assertEquals(
      resolution,
      GuideService.GuideStarResolution.SelectedStarUnusable(
        GuideStarId.GaiaDr3(GuideStarName.gaiaSourceId.getOption(GuideStarName.unsafeFrom(aowfsStarName)).get)
      )
    )

  test("a calibration that does not guide resolves to NotGuided"):
    for
      p          <- createProgramAs(pi)
      t          <- createTargetWithProfileAs(pi, p)
      o          <- createObservationAs(pi, p, List(t))
      _          <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _          <- setObservationCalibrationRole(List(o), CalibrationRole.DaytimePinhole)
      resolution <- resolveGuideStar(o)
    yield assertEquals(resolution, GuideService.GuideStarResolution.NotGuided)


// Behind Altair the AOWFS guides on axis, so the science target is a guide star candidate even
// where Gaia has no star at all.
class guideEnvironmentGnirsAltairObservationTarget extends ExecutionTestSupportForGnirs
                                                       with GuideEnvironmentSuite:

  override val gaiaResponseString: String = GaiaVoTables.noCandidates

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGnirsLongSlitObservationAs(user, pid, tids*)

  // Bright enough in R for the Altair NGS limits.
  private val RBrightProfile: String =
    """
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: { stellarLibrary: O5_V },
            brightnesses: [ { band: R, value: 11, units: VEGA_MAGNITUDE } ]
          }
        }
      }
    """

  // Too faint in R for the Altair NGS limits.
  private val RFaintProfile: String =
    """
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: { stellarLibrary: O5_V },
            brightnesses: [ { band: R, value: 20, units: VEGA_MAGNITUDE } ]
          }
        }
      }
    """

  private def availabilityQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            guideAvailability(start: "2023-08-30T00:00:00Z", end: "2023-09-06T00:00:00Z") {
              posAngles { degrees }
            }
          }
        }
      }
    """

  // The position angles of each availability period.
  private def availablePosAngles(oid: Observation.Id): IO[List[List[Json]]] =
    query(user = pi, query = availabilityQuery(oid)).map:
      _.hcursor
        .downField("observation")
        .downField("targetEnvironment")
        .downField("guideAvailability")
        .as[List[Json]]
        .toOption
        .get
        .map(_.hcursor.downField("posAngles").as[List[Json]].toOption.get)

  private def assertAvailability(oid: Observation.Id): IO[Unit] =
    availablePosAngles(oid).map: angles =>
      assert(angles.nonEmpty)
      assert(angles.forall(_.nonEmpty), s"periods without position angles: $angles")

  private def setAltairNgs(oid: Observation.Id): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            WHERE: { id: { EQ: "$oid" } }
            SET: { targetEnvironment: { altair: { mode: NGS } } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def altairNgsObservation(tids: List[Target.Id], pid: Program.Id): IO[Observation.Id] =
    for
      o <- createObservationAs(pi, pid, tids)
      _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      _ <- setAltairNgs(o)
    yield o

  // The science target and the observation that guides on it.
  private def altairNgsObservation(sourceProfile: String): IO[(Target.Id, Observation.Id)] =
    for
      p <- createProgramAs(pi)
      t <- createTargetWithProfileAs(pi, p, sourceProfile)
      o <- altairNgsObservation(List(t), p)
    yield (t, o)

  private def observationWithAltairNgs: IO[Observation.Id] =
    altairNgsObservation(RBrightProfile).map(_._2)

  // A bright sidereal target near V1647 Orionis.
  private def createBrightTargetAs(pid: Program.Id, name: String, ra: String): IO[Target.Id] =
    query(
      user  = pi,
      query = s"""
        mutation {
          createTarget(input: {
            programId: "$pid"
            SET: {
              name: "$name"
              sidereal: {
                ra: { hms: "$ra" }
                dec: { dms: "-00:06:04.89" }
                epoch: "J2000.0"
              }
              $RBrightProfile
            }
          }) {
            target { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createTarget", "target", "id").require[Target.Id])

  private def observationTargetQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
            guideEnvironment {
              guideTargets {
                name
                probe
                sidereal {
                  ra { hms }
                  dec { dms }
                }
              }
            }
          }
        }
      }
    """

  private val observationTargetResult: Either[List[String], Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "guideEnvironment": {
            "guideTargets": [
              {
                "name": "V1647 Orionis",
                "probe": "ALTAIR_AOWFS",
                "sidereal": {
                  "ra": { "hms": "05:46:13.137000" },
                  "dec": { "dms": "-00:06:04.890000" }
                }
              }
            ]
          }
        }
      }
    }
    """.asRight

  private def guideTargetNamesQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          targetEnvironment {
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

  private def guideTargetNamesResult(name: String): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "targetEnvironment": {
          "guideEnvironment": {
            "guideTargets": [
              {
                "name": $name,
                "probe": "ALTAIR_AOWFS"
              }
            ]
          }
        }
      }
    }
    """.asRight

  private def setGuideTargetMutation(oid: Observation.Id, guideTarget: String): String =
    s"""
      mutation {
        setGuideTarget(input: { observationId: "$oid", guideTarget: $guideTarget }) {
          observation { id }
        }
      }
    """

  private def resolveGuideStar(oid: Observation.Id): IO[GuideService.GuideStarResolution] =
    withServices(pi): services =>
      Services.asSuperUser(services.guideService.resolveGuideStar(oid)).flatMap(_.get)

  test("Altair NGS guides on the science target when Gaia has no star"):
    observationWithAltairNgs.flatMap: oid =>
      expect(pi, observationTargetQuery(oid), expected = observationTargetResult)

  test("the science target gives guide availability when Gaia has no star"):
    observationWithAltairNgs.flatMap(assertAvailability)

  // Behind Altair the sequence is sized from the star, so without one there is no availability.
  test("a science target too faint to guide on gives no guide availability"):
    altairNgsObservation(RFaintProfile).flatMap: (_, oid) =>
      expect(
        pi,
        availabilityQuery(oid),
        expected = List(
          s"Could not generate a sequence for $oid: Altair needs a guide star but none is usable."
        ).asLeft
      )

  test("the selected observation target resolves to the science target"):
    for
      (tid, oid) <- altairNgsObservation(RBrightProfile)
      _          <- setGuideTarget(pi, oid, "{ observationTarget: {} }".some)
      _          <- expect(pi, observationTargetQuery(oid), expected = observationTargetResult)
      resolution <- resolveGuideStar(oid)
    yield
      val resolved: Option[GuideService.ResolvedGuideStar] = resolution.star
      assertEquals(resolved.map(_.guideStarId), GuideStarId.ObservationTarget(tid.some).some)
      val separation: Angle = resolved.map(_.separation).get
      assert(Angle.signedMicroarcseconds.get(separation).abs < 1000L, s"unexpected separation $separation")

  test("with two science targets an id-less observation target selection is ambiguous, an id'd one is followed"):
    for
      p      <- createProgramAs(pi)
      first  <- createBrightTargetAs(p, "First", "05:46:13.137")
      second <- createBrightTargetAs(p, "Second", "05:46:13.470")
      oid    <- altairNgsObservation(List(first, second), p)
      // Automatic selection picks one of them.
      _      <- query(pi, guideTargetNamesQuery(oid)).map: json =>
                  val probes: Option[List[String]] =
                    json.hcursor
                      .downFields("observation", "targetEnvironment", "guideEnvironment", "guideTargets")
                      .as[List[Json]]
                      .toOption
                      .map(_.flatMap(_.hcursor.downField("probe").as[String].toOption))
                  assertEquals(probes, List("ALTAIR_AOWFS").some)
      _      <- expect(
                  pi,
                  setGuideTargetMutation(oid, "{ observationTarget: {} }"),
                  expected = List(
                    s"Could not generate a sequence for $oid: ${GuideService.ambiguousObservationTargetMessage(oid)}"
                  ).asLeft
                )
      _      <- setGuideTarget(pi, oid, s"""{ observationTarget: { targetId: "$second" } }""".some)
      _      <- expect(pi, guideTargetNamesQuery(oid), expected = guideTargetNamesResult("Second"))
    yield ()

  test("an observation target selection naming a target not in the observation is unusable"):
    for
      (tid, oid) <- altairNgsObservation(RBrightProfile)
      p          <- query(pi, s"""query { observation(observationId: "$oid") { program { id } } }""")
                      .map(_.hcursor.downFields("observation", "program", "id").require[Program.Id])
      other      <- createTargetWithProfileAs(pi, p, RBrightProfile)
      _          <- expect(
                      pi,
                      setGuideTargetMutation(oid, s"""{ observationTarget: { targetId: "$other" } }"""),
                      expected = List(
                        s"Could not generate a sequence for $oid: ${GuideService.selectedStarUnusableMessage(GuideStarId.ObservationTarget(other.some))}"
                      ).asLeft
                    )
      resolution <- resolveGuideStar(oid)
    yield assertEquals(resolution.star.map(_.guideStarId), GuideStarId.ObservationTarget(tid.some).some)

  test("Altair NGS guides on a non-sidereal science target, which it follows"):
    val setup: IO[Observation.Id] =
      for
        p   <- createProgramAs(pi)
        eph  = createNonsiderealEphemeris
        t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target", sourceProfile = RBrightProfile)
        o   <- altairNgsObservation(List(t), p)
      yield o

    val nonsiderealQuery: Observation.Id => String =
      oid => s"""
        query {
          observation(observationId: "$oid") {
            targetEnvironment {
              guideEnvironment {
                guideTargets {
                  name
                  probe
                  sidereal { epoch }
                  nonsidereal { keyType }
                }
              }
            }
          }
        }
      """

    for
      oid <- setup
      _   <- expect(
               pi,
               nonsiderealQuery(oid),
               expected = json"""
                 {
                   "observation": {
                     "targetEnvironment": {
                       "guideEnvironment": {
                         "guideTargets": [
                           {
                             "name": "Nonsidereal Target",
                             "probe": "ALTAIR_AOWFS",
                             "sidereal": null,
                             "nonsidereal": { "keyType": "USER_SUPPLIED" }
                           }
                         ]
                       }
                     }
                   }
                 }
               """.asRight
             )
      _   <- assertAvailability(oid)
    yield ()
