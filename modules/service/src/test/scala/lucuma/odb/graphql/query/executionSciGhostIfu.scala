// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfuMappingType
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.odb.util.Codecs.coordinates
import lucuma.odb.util.Codecs.epoch
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.parallax
import lucuma.odb.util.Codecs.proper_motion
import lucuma.odb.util.Codecs.radial_velocity
import lucuma.odb.util.GhostCodecs.ghost_ifu_mapping_type
import skunk.*
import skunk.implicits.*

class executionSciGhostIfu extends ExecutionTestSupportForGhost:

  val StepCount: Int = 3

  def mode(
    resolutionMode: GhostResolutionMode = GhostResolutionMode.Standard,
    skyPosition: Option[Coordinates]    = None
  ): String =
    s"""
      ghostIfu: {
        stepCount: $StepCount
        resolutionMode: ${resolutionMode.tag.toScreamingSnakeCase}
        red: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 10.0 }
              count: 2
              at: { nanometers: 500 }
            }
          }
          explicitBinning: ONE_BY_TWO
        }
        blue: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 30.0 }
              count: 4
              at: { nanometers: 500 }
            }
          }
          explicitReadMode: FAST
        }
        slitViewingCameraExposureTime: { seconds: 5.0 }
        explicitIfu1Agitator: ENABLED
        ${
          skyPosition.fold(""): s =>
            s"""
              skyPosition: {
                ra: { hms: "${RightAscension.fromStringHMS.reverseGet(s.ra)}" }
                dec: { dms: "${Declination.fromStringSignedDMS.reverseGet(s.dec)}" }
              }
            """
        }
      }
    """

  val standardResolutionNoSky: String =
    mode()

  val config = GhostDynamicConfig(
    GhostDetector(
      10.secondTimeSpan,
      PosInt.unsafeFrom(2),
      GhostBinning.OneByTwo,
      GhostReadMode.DefaultRed
    ).asRed,
    GhostDetector(
      30.secondTimeSpan,
      PosInt.unsafeFrom(4),
      GhostBinning.OneByOne,
      GhostReadMode.Fast
    ).asBlue,
    GhostIfu1FiberAgitator.Enabled,
    GhostIfu2FiberAgitator.Disabled
  )

  val expected: Json =
    Json.obj(
      "executionConfig" -> Json.obj(
        "ghost" -> Json.obj(
          "static" -> Json.obj(
            "resolutionMode" -> GhostResolutionMode.Standard.asJson,
            "slitViewingCameraExposureTime" -> Json.obj(
              "seconds" -> BigDecimal("5.000000").asJson
            )
          ),
          "science" -> Json.obj(
            "nextAtom" -> expectedAtom(config),
            "possibleFuture" -> List.fill(StepCount-1)(expectedAtom(config)).asJson,
            "hasMore" -> Json.fromBoolean(false)
          )
        )
      )
    )

  test("generation - pre visit"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = scienceQuery(oid),
        expected = expected.asRight
      )

  test("generation - post visit"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
        _ <- recordVisitAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = scienceQuery(oid),
        expected = expected.asRight
      )

  test("execution"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      v <- recordVisitAs(serviceUser, o)
      s <- firstScienceStepId(serviceUser, o)
      _ <- addEndStepEvent(s, v)
      _ <- expect(
            pi,
            s"""
              query {
                observation(observationId: "$o") {
                  execution {
                    atomRecords {
                      matches {
                        steps {
                          matches {
                            index
                            ghost {
                              red {
                                exposureTime { seconds }
                                exposureCount
                                binning
                                readMode
                              }
                              blue {
                                exposureTime { seconds }
                                exposureCount
                                binning
                                readMode
                              }
                              ifu1FiberAgitator
                              ifu2FiberAgitator
                              centralWavelength { nanometers }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            """,
            json"""
              {
                "observation": {
                  "execution": {
                    "atomRecords": {
                      "matches": [
                        {
                          "steps": {
                            "matches": [
                              {
                                "index": 1,
                                "ghost": ${expectedInstrumentConfig(config)}
                              }
                            ]
                          }
                        }
                      ]
                    }
                  }
                }
              }
            """.asRight
          )
    yield ()

  private def readIfuMappingRow(
    oid: Observation.Id
  ): IO[(
    GhostIfuMappingType,
    Option[Coordinates],
    Option[Epoch],
    Option[ProperMotion],
    Option[RadialVelocity],
    Option[Parallax],
    Option[Coordinates],
    Option[Epoch],
    Option[ProperMotion],
    Option[RadialVelocity],
    Option[Parallax]
  )] =
    withSession: session =>
      session.unique(
        sql"""
          SELECT
            c_ifu_mapping,
            c_ifu1_ra,
            c_ifu1_dec,
            c_ifu1_epoch,
            c_ifu1_pm_ra,
            c_ifu1_pm_dec,
            c_ifu1_rv,
            c_ifu1_parallax,
            c_ifu2_ra,
            c_ifu2_dec,
            c_ifu2_epoch,
            c_ifu2_pm_ra,
            c_ifu2_pm_dec,
            c_ifu2_rv,
            c_ifu2_parallax
          FROM t_ghost_static
          WHERE c_observation_id = $observation_id
        """.query(
          ghost_ifu_mapping_type *:
          coordinates.opt        *:
          epoch.opt              *:
          proper_motion.opt      *:
          radial_velocity.opt    *:
          parallax.opt           *:
          coordinates.opt        *:
          epoch.opt              *:
          proper_motion.opt      *:
          radial_velocity.opt    *:
          parallax.opt
        )
      )(oid)

  def assertIfuMappingRow(
    oid: Observation.Id,
    row: (
      GhostIfuMappingType,
      Option[Coordinates],
      Option[Epoch],
      Option[ProperMotion],
      Option[RadialVelocity],
      Option[Parallax],
      Option[Coordinates],
      Option[Epoch],
      Option[ProperMotion],
      Option[RadialVelocity],
      Option[Parallax]
    )
  ): IO[Unit] =
    assertIO(readIfuMappingRow(oid), row)

  val siderealQuery: String =
    """
                    ra { hms }
                    dec { dms }
                    epoch
                    properMotion {
                      ra { microarcsecondsPerYear }
                      dec { microarcsecondsPerYear }
                    }
                    radialVelocity { centimetersPerSecond }
                    parallax { microarcseconds }
    """

  val skyQuery: String =
    """
                    ra { hms }
                    dec { dms }
    """

  def staticConfigQuery(o: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: "$o") {
          ghost {
            static {
              resolutionMode
              ifuMapping {
                mappingType
                singleTarget {
                  ifu1 { $siderealQuery }
                }
                targetPlusSky {
                  ifu1 { $siderealQuery }
                  ifu2 { $skyQuery }
                }
                skyPlusTarget {
                  ifu1 { $skyQuery }
                  ifu2 { $siderealQuery }
                }
                dualTarget {
                  ifu1 { $siderealQuery }
                  ifu2 { $siderealQuery }
                }
              }
              slitViewingCameraExposureTime { seconds }
            }
          }
        }
      }
    """

  val NonsiderealResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "NONSIDEREAL",
                "singleTarget": null,
                "targetPlusSky": null,
                "skyPlusTarget": null,
                "dualTarget": null
              },
              "slitViewingCameraExposureTime": {
                "seconds": 5.000000
              }
            }
          }
        }
      }
    """

  test("IFU Mapping Nonsidereal"):
    for
      p <- createProgram
      t <- createNonsiderealTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            NonsiderealResult.asRight
          )
    yield ()

  test("IFU Mapping Nonsidereal - stored"):
    for
      p <- createProgram
      t <- createNonsiderealTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.Nonsidereal,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            NonsiderealResult.asRight
          )
    yield ()

  val TargetJson: Json =
    json"""
      {
        "ra" : { "hms" : "05:46:13.137000" },
        "dec" : { "dms" : "-00:06:04.890000" },
        "epoch" : "J2000.000",
        "properMotion" : {
          "ra" : { "microarcsecondsPerYear" : 918 },
          "dec" : { "microarcsecondsPerYear" : -1057 }
        },
        "radialVelocity" : { "centimetersPerSecond" : 2758000 },
        "parallax" : { "microarcseconds" : 2422 }
      }
    """

  val SingleTargetResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "SINGLE_TARGET",
                "singleTarget": {
                  "ifu1": $TargetJson
                },
                "targetPlusSky": null,
                "skyPlusTarget": null,
                "dualTarget": null

              },
              "slitViewingCameraExposureTime": {
                "seconds": 5.000000
              }
            }
          }
        }
      }
    """

  test("IFU Mapping SingleTarget"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            SingleTargetResult.asRight
          )
    yield ()

  val TargetCoordinates: Option[Coordinates] =
    Coordinates(
      RightAscension.lenientFromStringHMS.unsafeGet("05:46:13.137"),
      Declination.lenientFromStringDMS.unsafeGet("-00:06:04.89")
    ).some

  val TargetEpoch: Option[Epoch] =
    Epoch.fromString.getOption("J2000.0")

  val TargetProperMotion: Option[ProperMotion] =
    ProperMotion(
      ProperMotion.RA.microarcsecondsPerYear.get(918L),
      ProperMotion.Dec.microarcsecondsPerYear.get(-1057L)
    ).some

  val TargetRadialVelocity: Option[RadialVelocity] =
    RadialVelocity.fromMetersPerSecond.getOption(27580)

  val TargetParallax: Option[Parallax] =
    Parallax.fromMicroarcseconds(2422).some

  test("IFU Mapping SingleTarget - stored"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.SingleTarget,
          TargetCoordinates,
          TargetEpoch,
          TargetProperMotion,
          TargetRadialVelocity,
          TargetParallax,
          None,
          None,
          None,
          None,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            SingleTargetResult.asRight
          )
    yield ()

  val TargetPlusSkyResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "TARGET_PLUS_SKY",
                "singleTarget": null,
                "targetPlusSky": {
                  "ifu1": $TargetJson,
                  "ifu2": {
                    "ra": { "hms":  "05:46:03.137000" },
                    "dec": { "dms":  "-00:06:04.890000" }
                  }
                },
                "skyPlusTarget": null,
                "dualTarget": null
              },
              "slitViewingCameraExposureTime": {
                "seconds": 5.000000
              }
            }
          }
        }
      }
    """

  test("IFU Mapping TargetPlusSky"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            TargetPlusSkyResult.asRight
          )
    yield ()

  test("IFU Mapping TargetPlusSky - stored"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.TargetPlusSky,
          TargetCoordinates,
          TargetEpoch,
          TargetProperMotion,
          TargetRadialVelocity,
          TargetParallax,
          Coordinates(
            RightAscension.lenientFromStringHMS.unsafeGet("05:46:03.137"),
            Declination.lenientFromStringDMS.unsafeGet("-00:06:04.89")
          ).some,
          None,
          None,
          None,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            TargetPlusSkyResult.asRight
          )
    yield ()

  val SkyPlusTargetResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "SKY_PLUS_TARGET",
                "singleTarget": null,
                "targetPlusSky": null,
                "skyPlusTarget": {
                  "ifu1": {
                    "ra": { "hms":  "05:46:23.137000" },
                    "dec": { "dms":  "-00:06:04.890000" }
                  },
                  "ifu2": $TargetJson
                },
                "dualTarget": null
              },
              "slitViewingCameraExposureTime": {
                "seconds": 5.000000
              }
            }
          }
        }
      }
    """

  test("IFU Mapping SkyPlusTarget"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            SkyPlusTargetResult.asRight
          )
    yield ()

  test("IFU Mapping SkyPlusTarget - stored"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.SkyPlusTarget,
          Coordinates(
            RightAscension.lenientFromStringHMS.unsafeGet("05:46:23.137"),
            Declination.lenientFromStringDMS.unsafeGet("-00:06:04.89")
          ).some,
          None,
          None,
          None,
          None,
          TargetCoordinates,
          TargetEpoch,
          TargetProperMotion,
          TargetRadialVelocity,
          TargetParallax,
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            SkyPlusTargetResult.asRight
          )
    yield ()

  val TargetJson2: Json =
    json"""
      {
        "ra" : { "hms" : "05:46:03.137000" },
        "dec" : { "dms" : "-00:06:04.890000" },
        "epoch" : "J2000.000",
        "properMotion" : {
          "ra" : { "microarcsecondsPerYear" : 918 },
          "dec" : { "microarcsecondsPerYear" : -1057 }
        },
        "radialVelocity" : { "centimetersPerSecond" : 2758000 },
        "parallax" : { "microarcseconds" : 2422 }
      }
    """

  val DualTargetResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "DUAL_TARGET",
                "singleTarget": null,
                "targetPlusSky": null,
                "skyPlusTarget": null,
                "dualTarget": {
                  "ifu1": $TargetJson,
                  "ifu2": $TargetJson2
                }
              },
              "slitViewingCameraExposureTime": {
                "seconds": 5.000000
              }
            }
          }
        }
      }
    """

  test("IFU Mapping DualTarget"):
    for
      p  <- createProgram
      t1 <- createTargetWithProfileAs(pi, p)
      t2 <- query(
        user  = pi,
        query = s"""
          mutation {
            cloneTarget(input: {
              targetId: "$t1"
              SET: {
                sidereal: {
                  ra: { hms: "05:46:03.137000" }
                }
              }
            }) {
              newTarget {
                id
              }
            }
          }
        """
      ).map: json =>
        json.hcursor.downFields("cloneTarget", "newTarget", "id").require[Target.Id]
      o  <- createObservationWithModeAs(pi, p, List(t1, t2), standardResolutionNoSky)
      _  <- expect(
            pi,
            staticConfigQuery(o),
            DualTargetResult.asRight
          )
    yield ()

  // Rotate position angle by 180º -- target 1 goes to IFU2, target 2 to IFU1
  test("IFU Mapping DualTarget - flip position angle"):
    for
      p  <- createProgram
      t1 <- createTargetWithProfileAs(pi, p)
      t2 <- query(
        user  = pi,
        query = s"""
          mutation {
            cloneTarget(input: {
              targetId: "$t1"
              SET: {
                sidereal: {
                  ra: { hms: "05:46:03.137000" }
                }
              }
            }) {
              newTarget {
                id
              }
            }
          }
        """
      ).map: json =>
        json.hcursor.downFields("cloneTarget", "newTarget", "id").require[Target.Id]
      o  <- createObservationWithModeAs(pi, p, List(t1, t2), standardResolutionNoSky)
      _  <- query(
        user  = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                posAngleConstraint: {
                  mode: FIXED
                  angle: { degrees: 180.0 }
                }
              }
              WHERE: {
                id: { EQ: "$o" }
              }
            }) {
              observations {
                id
              }
            }
          }
        """
      )
      _  <- expect(
            pi,
            staticConfigQuery(o),
            json"""
              {
                "executionConfig": {
                  "ghost": {
                    "static": {
                      "resolutionMode": "STANDARD",
                      "ifuMapping": {
                        "mappingType": "DUAL_TARGET",
                        "singleTarget": null,
                        "targetPlusSky": null,
                        "skyPlusTarget": null,
                        "dualTarget": {
                          "ifu1": $TargetJson2,
                          "ifu2": $TargetJson
                        }
                      },
                      "slitViewingCameraExposureTime": {
                        "seconds": 5.000000
                      }
                    }
                  }
                }
              }
            """.asRight
          )
    yield ()

  test("IFU Mapping FAIL - No Targets"):
    for
      p <- createProgram
      o <- createObservationWithModeAs(pi, p, Nil, standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            List(s"Could not generate a sequence for $o: observation is missing target").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - Too many targets"):
    for
      p  <- createProgram
      t0 <- createTargetWithProfileAs(pi, p)
      t1 <- createTargetWithProfileAs(pi, p)
      t2 <- createTargetWithProfileAs(pi, p)
      o  <- createObservationWithModeAs(pi, p, List(t0, t1, t2), standardResolutionNoSky)
      _  <- expect(
             pi,
             staticConfigQuery(o),
             List("Could not compute GHOST IFU mapping: Cannot derive a GHOST IFU mapping with more than two targets.").asLeft
           )
    yield ()

  test("IFU Mapping FAIL - Explicit base but target on ifu2"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- query(
        user  = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  explicitBase: {
                    ra: { hms: "05:46:23.137" }
                    dec: { dms: "-00:06:04.89" }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$o" }
              }
            }) {
              observations {
                id
              }
            }
          }
        """
      )

      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: The target does not fall in range of GHOST IFU1 probe.").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - Target and sky too far apart"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-20000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- query(
        user  = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                targetEnvironment: {
                  explicitBase: {
                    ra: { hms: "05:46:23.137" }
                    dec: { dms: "-00:06:04.89" }
                  }
                }
              }
              WHERE: {
                id: { EQ: "$o" }
              }
            }) {
              observations {
                id
              }
            }
          }
        """
      )

      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: The target and sky positions are too far apart.").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - nonsidereal + sky"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createNonsiderealTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: GHOST does not support sky positions for nonsidereal targets.").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - opportunity"):
    for
      p <- createProgram
      t <- createOpportunityTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: A GHOST IFU mapping can only be determined after the science target is identified.").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - high resolution, no sky"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(resolutionMode = GhostResolutionMode.High))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: GHOST High Resolution mode requires a sky position.").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - mixed targets"):
    for
      p  <- createProgram
      t0 <- createTargetWithProfileAs(pi, p)
      t1 <- createNonsiderealTargetAs(pi, p)
      o  <- createObservationWithModeAs(pi, p, List(t0, t1), standardResolutionNoSky)
      _  <- expect(
              pi,
              staticConfigQuery(o),
              List("Could not compute GHOST IFU mapping: GHOST Dual Target mode is available for sidereal targets only.").asLeft
            )
    yield ()

  test("IFU Mapping FAIL - dual + sky"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p  <- createProgram
      t0 <- createTargetWithProfileAs(pi, p)
      t1 <- createTargetWithProfileAs(pi, p)
      o  <- createObservationWithModeAs(pi, p, List(t0, t1), mode(skyPosition = sky.some))
      _  <- expect(
              pi,
              staticConfigQuery(o),
              List("Could not compute GHOST IFU mapping: A sky position should not be defined for Dual Target mode.").asLeft
            )
    yield ()

  test("IFU Mapping FAIL - dual + high res"):
    for
      p  <- createProgram
      t0 <- createTargetWithProfileAs(pi, p)
      t1 <- createTargetWithProfileAs(pi, p)
      o  <- createObservationWithModeAs(pi, p, List(t0, t1), mode(resolutionMode = GhostResolutionMode.High))
      _  <- expect(
              pi,
              staticConfigQuery(o),
              List("Could not compute GHOST IFU mapping: Dual Target mode is only available in Standard Resolution.").asLeft
            )
    yield ()