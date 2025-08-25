// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import munit.TestOptions

import ObservingModeSetupOperations.*

trait ObservingModeSetupOperations extends DatabaseOperations { this: OdbSuite =>

  private def formatExplicitSpatialOffsetsInput(arcsecs: List[Int]): String =
    arcsecs.map(a => s"{ arcseconds: $a }").mkString("explicitSpatialOffsets: [", ", ", "]")

  def createFlamingos2LongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: JH
          fpu: LONG_SLIT_1
        }
      """
    )

  def createGmosNorthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    offsetArcsec: Option[List[Int]] = None
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        gmosNorthLongSlit: {
          grating: R831_G5302
          filter: R_PRIME
          fpu: LONG_SLIT_0_50
          centralWavelength: {
            nanometers: 500
          }
          explicitYBin: TWO
          ${offsetArcsec.fold("")(formatExplicitSpatialOffsetsInput)}
        }
      """
    )

  def createGmosSouthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      """
        gmosSouthLongSlit: {
          grating: R600_G5324,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          centralWavelength: {
            nanometers: 500
          },
          explicitYBin: TWO
        }
      """
    )

  def createObservationWithModeAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    mode:         String,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson},
            SET: {
              $ConstraintSet,
              targetEnvironment: {
                asterism: ${tids.asJson}
              },
              $SpectroscopyScienceRequirements,
              observingMode: {
                $mode
              }
            }
          }) {
            observation {
              id
            }
          }
        }
      """
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createObservationWithNoModeAs(
    user:         User,
    pid:          Program.Id,
    tid:          Target.Id,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson},
            SET: {
              $ConstraintSet,
              targetEnvironment: {
                asterism: ${List(tid).asJson}
              },
              $SpectroscopyScienceRequirements,
            }
          }) {
            observation {
              id
            }
          }
        }
      """
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  val PointBandNormalizedProfile: String = """
    sourceProfile: {
      point: {
        bandNormalized: {
          sed: {
            stellarLibrary: O5_V
          },
          brightnesses: [
            {
              band: J,
              value: 14.74,
              units: VEGA_MAGNITUDE
            },
            {
              band: V,
              value: 18.1,
              units: VEGA_MAGNITUDE
            }
          ]
        }
      }
    }
  """

  val PointEmissionLinesProfile: String = """
    sourceProfile: {
      point: {
        emissionLines: {
          lines: [
            {
              wavelength: {micrometers: 0.5 },
              lineWidth: 801,
              lineFlux: {
                value: 1e-12,
                units: ERG_PER_S_PER_CM_SQUARED
              }
            },
            {
              wavelength: {micrometers: 0.65 },
              lineWidth: 850,
              lineFlux: {
                value: 1e-13,
                units: ERG_PER_S_PER_CM_SQUARED
              }
            }
          ],
          fluxDensityContinuum: {
            value: 1e-15,
            units: ERG_PER_S_PER_CM_SQUARED_PER_A
          }
        }
      }
    }
  """

  val PointEmissionLinesProfileNoLines: String = """
    sourceProfile: {
      point: {
        emissionLines: {
          lines: [
          ],
          fluxDensityContinuum: {
            value: 1e-15,
            units: ERG_PER_S_PER_CM_SQUARED_PER_A
          }
        }
      }
    }
  """

  val UniformEmissionLinesProfile: String = """
    sourceProfile: {
      uniform: {
        emissionLines: {
          lines: [
            {
              wavelength: {micrometers: 0.5 },
              lineWidth: 801,
              lineFlux: {
                value: 1e-12,
                units: ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED
              }
            },
            {
              wavelength: {micrometers: 0.65 },
              lineWidth: 850,
              lineFlux: {
                value: 1e-13,
                units:ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED
              }
            }
          ],
          fluxDensityContinuum: {
            value: 1e-15,
            units: ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED
          }
        }
      }
    }
  """

  val UniformEmissionLinesProfileNoLines: String = """
    sourceProfile: {
      uniform: {
        emissionLines: {
          lines: [
          ],
          fluxDensityContinuum: {
            value: 1e-15,
            units: ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED
          }
        }
      }
    }
  """

  def gaussianBandNormalizedProfile(fwhm: Angle): String = s"""
    sourceProfile: {
      gaussian: {
        fwhm: {
          microarcseconds: ${fwhm.toMicroarcseconds}
        },
        spectralDefinition: {
          bandNormalized: {
            sed: {
              stellarLibrary: O5_V
            },
            brightnesses: [
              {
                band: J,
                value: 14.74,
                units: VEGA_MAGNITUDE
              },
              {
                band: V,
                value: 18.1,
                units: VEGA_MAGNITUDE
              }
            ]
          }
        }
      }
    }
  """

  enum TargetType:
    case Sidereal, Nonsidereal, Opportunity

  /** Create multiple tests that take an injected Target constructor. */
  def testWithTargetTypes(
    name: String | TestOptions,
    ctors: Map[TargetType, (User, Program.Id) => IO[Target.Id]] =
      Map(
        TargetType.Sidereal    -> ((u, p) => createTargetWithProfileAs(u, p)),
        TargetType.Opportunity -> ((u, p) => createOpportunityTargetAs(u, p)),
      )
  )(body: (TargetType, (User, Program.Id) => IO[Target.Id]) => Any) =
    ctors.foreach: (tt, fun) =>
      val prefix = s"[$tt]".padTo(13, ' ')
      val ops = name match
        case s: String => TestOptions(s"$prefix $name")
        case o: TestOptions => o.withName(s"$prefix ${o.name}")          
      test(ops)(body(tt, fun))

  def createTargetWithProfileAs(
    user:     User,
    pid:      Program.Id,
    profile:  String = PointBandNormalizedProfile
  ): IO[Target.Id] =
    query(
      user  = user,
      query =
      s"""
        mutation {
          createTarget(input: {
            programId: ${pid.asJson},
            SET: {
              name: "V1647 Orionis"
              sidereal: {
                ra: { hms: "05:46:13.137" },
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
              $profile
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

}

object ObservingModeSetupOperations {

  val ConstraintSet: String =
    """
      constraintSet: {
        cloudExtinction: POINT_ONE,
        imageQuality: POINT_ONE,
        skyBackground: DARKEST
      }
    """

  val SpectroscopyScienceRequirements: String =
    """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0,
            at: { nanometers: 500 }
          }
        },
        spectroscopy: {
          wavelength: {
            nanometers: 500
          },
          resolution: 100,
          wavelengthCoverage: {
            nanometers: 20
          },
          focalPlane: SINGLE_SLIT,
          focalPlaneAngle: {
            microarcseconds: 0
          }
        }
      }
    """

}
