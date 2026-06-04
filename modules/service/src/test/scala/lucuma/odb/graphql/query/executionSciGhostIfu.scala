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
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostIfuMappingType
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.odb.util.Codecs.coordinates
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.target_id
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
    Option[Target.Id],
    Option[Coordinates],
    Option[Target.Id]
  )] =
    withSession: session =>
      session.unique(
        sql"""
          SELECT
            c_ifu_mapping,
            c_ifu1_ra,
            c_ifu1_dec,
            c_ifu1_target_id,
            c_ifu2_ra,
            c_ifu2_dec,
            c_ifu2_target_id
          FROM t_ghost_static
          WHERE c_observation_id = $observation_id
        """.query(
          ghost_ifu_mapping_type *:
          coordinates.opt        *:
          target_id.opt          *:
          coordinates.opt        *:
          target_id.opt
        )
      )(oid)

  def assertIfuMappingRow(
    oid: Observation.Id,
    row: (
      GhostIfuMappingType,
      Option[Coordinates],
      Option[Target.Id],
      Option[Coordinates],
      Option[Target.Id]
    )
  ): IO[Unit] =
    assertIO(readIfuMappingRow(oid), row)

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
                  ifu1
                }
                targetPlusSky {
                  ifu1
                  ifu2 { $skyQuery }
                }
                skyPlusTarget {
                  ifu1 { $skyQuery }
                  ifu2
                }
                dualTarget {
                  ifu1
                  ifu2
                }
              }
              slitViewingCameraExposureTime { seconds }
            }
          }
        }
      }
    """

  def singleTargetResult(t: Target.Id): Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "SINGLE_TARGET",
                "singleTarget": {
                  "ifu1": ${t.asJson}
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

  test("IFU Mapping SingleTarget (Nonsidereal)"):
    for
      p <- createProgram
      t <- createNonsiderealTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            singleTargetResult(t).asRight
          )
    yield ()

  test("IFU Mapping SingleTarget (Nonsidereal) - stored"):
    for
      p <- createProgram
      t <- createNonsiderealTargetAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.SingleTarget,
          None,
          Some(t),
          None,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            singleTargetResult(t).asRight
          )
    yield ()

  test("IFU Mapping SingleTarget (Sidereal)"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- expect(
            pi,
            staticConfigQuery(o),
            singleTargetResult(t).asRight
          )
    yield ()

  val TargetCoordinates: Option[Coordinates] =
    Coordinates(
      RightAscension.lenientFromStringHMS.unsafeGet("05:46:13.137"),
      Declination.lenientFromStringDMS.unsafeGet("-00:06:04.89")
    ).some

  test("IFU Mapping SingleTarget (Sidereal) - stored"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), standardResolutionNoSky)
      _ <- recordVisitAs(serviceUser, o)
      r <- assertIfuMappingRow(
        o,
        (
          GhostIfuMappingType.SingleTarget,
          None,
          Some(t),
          None,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            singleTargetResult(t).asRight
          )
    yield ()

  def targetPlusSkyResult(t: Target.Id): Json =
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
                  "ifu1": ${t.asJson},
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
            targetPlusSkyResult(t).asRight
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
          None,
          Some(t),
          Coordinates(
            RightAscension.lenientFromStringHMS.unsafeGet("05:46:03.137"),
            Declination.lenientFromStringDMS.unsafeGet("-00:06:04.89")
          ).some,
          None
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            targetPlusSkyResult(t).asRight
          )
    yield ()

  def skyPlusTargetResult(t: Target.Id): Json =
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
                  "ifu2": ${t.asJson}
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
            skyPlusTargetResult(t).asRight
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
          Some(t)
        )
      )
      _ <- expect(
            pi,
            staticConfigQuery(o),
            skyPlusTargetResult(t).asRight
          )
    yield ()

  def dualTargetResult(
    target1: Target.Id,
    target2: Target.Id
  ): Json =
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
                  "ifu1": ${target1.asJson},
                  "ifu2": ${target2.asJson}
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
            dualTargetResult(t1, t2).asRight
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
            dualTargetResult(t2, t1).asRight
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

  test("IFU Mapping FAIL - target an sky are too close"):
    val sky = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-6790000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = sky.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            List("Could not compute GHOST IFU mapping: The target and sky positions are too close (minimum separation is 102 arcseconds).").asLeft
          )
    yield ()

  test("IFU Mapping FAIL - dual targets are too close"):
    for
      p  <- createProgram
      t1 <- createTargetWithProfileAs(pi, p)
      t2 <- query(
              user  = pi,
              query =
              s"""
                mutation {
                  createTarget(input: {
                    programId: "$p",
                    SET: {
                      name: "V1647 Orionis 2"
                      sidereal: {
                        ra: { hms: "05:46:10.137" },
                        dec: { dms: "-00:06:04.89" },
                        epoch: "J2000.0",
                        properMotion: {
                          ra: {
                            milliarcsecondsPerYear: 0.918
                          },
                          dec: {
                            milliarcsecondsPerYear: -1.057
                          },
                        },
                        radialVelocity: {
                          kilometersPerSecond: 27.58
                        },
                        parallax: {
                          milliarcseconds: 2.422
                        }
                      },
                      $PointBandNormalizedProfile
                    }
                  }) {
                    target {
                      id
                    }
                  }
                }
              """
            ).map(
              _.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
            )
      o  <- createObservationWithModeAs(pi, p, List(t1, t2), standardResolutionNoSky)
      _  <- expect(
              pi,
              staticConfigQuery(o),
              List("Could not compute GHOST IFU mapping: The dual targets are too close (minimum separation is 102 arcseconds).").asLeft
            )
    yield ()

  test("IFU Mapping FAIL - validation"):
    val workflow = for
      p  <- createProgram
      t0 <- createTargetWithProfileAs(pi, p)
      t1 <- createTargetWithProfileAs(pi, p)
      o  <- createObservationWithModeAs(pi, p, List(t0, t1), mode(resolutionMode = GhostResolutionMode.High))
      _  <- runObscalcUpdate(p, o)
      w  <- queryObservationWorkflowState(pi, o)
      _  <- expect(
        user  = pi,
        query = s"""
          query {
            observation(observationId: "$o") {
              workflow {
                value {
                  state
                  validationErrors {
                    code
                    messages
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "workflow": {
                "value": {
                  "state": "UNDEFINED",
                  "validationErrors": [
                    {
                      "code": "CONFIGURATION_ERROR",
                      "messages": [
                        "Dual Target mode is only available in Standard Resolution."
                      ]
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    yield w

    assertIO(workflow, ObservationWorkflowState.Undefined)