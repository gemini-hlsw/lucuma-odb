// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*

import ObservingModeSetupOperations.*

trait ObservingModeSetupOperations extends DatabaseOperations { this: OdbSuite =>

  def createGmosNorthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    status:       ObsStatus       = ObsStatus.Approved,
    activeStatus: ObsActiveStatus = ObsActiveStatus.Active
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids.toList,
      """
        gmosNorthLongSlit: {
          grating: R831_G5302,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          centralWavelength: {
            nanometers: 500
          }
        }
      """,
      status,
      activeStatus
    )

  def createGmosSouthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    status:       ObsStatus       = ObsStatus.Approved,
    activeStatus: ObsActiveStatus = ObsActiveStatus.Active
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids.toList,
      """
        gmosSouthLongSlit: {
          grating: R600_G5324,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          centralWavelength: {
            nanometers: 500
          }
        }
      """,
      status,
      activeStatus
    )

  def createObservationWithModeAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    mode:         String,
    status:       ObsStatus       = ObsStatus.Approved,
    activeStatus: ObsActiveStatus = ObsActiveStatus.Active
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
               $ScienceRequirements,
               observingMode: {
                 $mode
               },
               status: ${status.tag.toScreamingSnakeCase},
               activeStatus: ${activeStatus.tag.toScreamingSnakeCase}
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
    status:       ObsStatus       = ObsStatus.Approved,
    activeStatus: ObsActiveStatus = ObsActiveStatus.Active
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
               $ScienceRequirements,
               status: ${status.tag.toScreamingSnakeCase},
               activeStatus: ${activeStatus.tag.toScreamingSnakeCase}
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

  def createTargetWithProfileAs(
    user: User,
    pid:  Program.Id
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

  val ScienceRequirements: String =
    """
      scienceRequirements: {
        mode: SPECTROSCOPY,
        spectroscopy: {
          wavelength: {
            nanometers: 500
          },
          resolution: 100,
          signalToNoise: 100.0,
          signalToNoiseAt: { nanometers: 500 },
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
