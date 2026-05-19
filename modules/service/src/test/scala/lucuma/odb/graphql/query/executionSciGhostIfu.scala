// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

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
                ... on GhostNonsidereal {
                  mappingType
                }
                ... on GhostSingleTarget {
                  mappingType
                  ifu1 {
                    $siderealQuery
                  }
                }
                ... on GhostTargetPlusSky {
                  mappingType
                  ifu1 {
                    $siderealQuery
                  }
                  ifu2 {
                    $skyQuery
                  }
                }
              }
              slitViewingCameraExposureTime { seconds }
            }
          }
        }
      }
    """
  /*
  val NonsiderealResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "NONSIDEREAL"
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
  */
  val Target: Json =
    json"""
      {
        "ra" : {
          "hms" : "05:46:13.137000"
        },
        "dec" : {
          "dms" : "-00:06:04.890000"
        },
        "epoch" : "J2000.000",
        "properMotion" : {
          "ra" : {
            "microarcsecondsPerYear" : 918
          },
          "dec" : {
            "microarcsecondsPerYear" : -1057
          }
        },
        "radialVelocity" : {
          "centimetersPerSecond" : 2758000
        },
        "parallax" : {
          "microarcseconds" : 2422
        }
      }
    """
  /*
  val SingleTargetResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "SINGLE_TARGET",
                "ifu1": $Target
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
  */
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
  /*
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
                "ifu1": $Target,
                "ifu2": {
                  "ra": { "hms":  "05:46:03.137000" },
                  "dec": { "dms":  "-00:06:04.890000" }
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

  test("IFU Mapping TargetPlusSky"):
    val offset = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = offset.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            TargetPlusSkyResult.asRight
          )
    yield ()

  test("IFU Mapping TargetPlusSky - stored"):
    val offset = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(-10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = offset.some))
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

  */

  val SkyPlusTargetResult: Json =
    json"""
      {
        "executionConfig": {
          "ghost": {
            "static": {
              "resolutionMode": "STANDARD",
              "ifuMapping": {
                "mappingType": "SKY_PLUS_TARGET",
                "ifu1": {
                  "ra": { "hms":  "05:46:23.137000" },
                  "dec": { "dms":  "-00:06:04.890000" }
                },
                "ifu2": $Target
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
    val offset = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = offset.some))
      _ <- expect(
            pi,
            staticConfigQuery(o),
            SkyPlusTargetResult.asRight
          )
    yield ()

  test("IFU Mapping SkyPlusTarget - stored"):
    val offset = TargetCoordinates.get.shift(HourAngle.fromMicroseconds(10000000L), Angle.Angle0)
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode(skyPosition = offset.some))
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
            TargetPlusSkyResult.asRight
          )
    yield ()