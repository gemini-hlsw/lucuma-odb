// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
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

  def createObservationWithModeQuery(
    pid:  Program.Id,
    tids: List[Target.Id],
    mode: String
  ): String =
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

  def createObservationWithModeAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    mode:         String,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query = createObservationWithModeQuery(pid, tids, mode),
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

  enum TargetType:
    case Sidereal, Nonsidereal, Opportunity

  /** Create multiple tests that take an injected Target constructor. */
  def testWithTargetTypes(
    name: String | TestOptions,
    ctors: Map[TargetType, (User, Program.Id) => IO[Target.Id]] =
      Map(
        TargetType.Sidereal    -> ((u, p) => createTargetWithProfileAs(u, p)),
        TargetType.Opportunity -> ((u, p) => createOpportunityTargetAs(u, p)),
        TargetType.Nonsidereal -> ((u, p) => createNonsiderealTargetAs(u, p)),
      )
  )(body: (TargetType, (User, Program.Id) => IO[Target.Id]) => Any) =
    ctors.foreach: (tt, fun) =>
      val prefix = s"[$tt]".padTo(13, ' ')
      val ops = name match
        case s: String => TestOptions(s"$prefix $name")
        case o: TestOptions => o.withName(s"$prefix ${o.name}")
      test(ops)(body(tt, fun))

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
