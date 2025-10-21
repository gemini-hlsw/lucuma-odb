// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

import java.time.Instant

/**
 * Simulates bug report sequence for "untargeted calibrations" behavior:
 *
 * 1. Create a complete GMOS observation -> automatic calibrations (SpectroPhotometric + Twilight)
 * 2. Duplicate the science observation -> Same calibs
 * 3. Change read mode of one observation -> generates additional calibrations for different configurations
 * 4. Delete observations -> calibrations deleted
 * 5. Undelete observations -> calibrations restoreb
 * 6. Change observation status to Inactive -> Extra untargeted calibratios appear
 */
class ShortCut_6598 extends ExecutionTestSupportForGmos:

  // Need a timestamp to call the calibrations service
  val when: Instant = Instant.ofEpochMilli(1729596890131L)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  case class CalibrationCounts(
    specPhotoCount: Int,
    twilightCount: Int,
    untargetedCount: Int,
    untargetedByTitleCount: Int
  )

  def countCalibrations(pid: Program.Id): IO[CalibrationCounts] =
    query(
      pi,
      s"""
        query {
          observations(WHERE: { program: { id: { EQ: "$pid" } } }) {
            matches {
              id
              title
              calibrationRole
              workflow {
                state
                value {
                  state
                }
              }
              existence
              targetEnvironment {
                asterism {
                  id
                }
              }
            }
          }
        }
      """
    ).map: json =>
      val observations = json.hcursor.downFields("observations", "matches").values.toList.flatten
      val calibrationsWithRoleAndTargets = observations.map: obsJson =>
        val role = obsJson
                     .hcursor
                     .downField("calibrationRole")
                     .as[Option[CalibrationRole]]
                     .getOrElse(None)
        val title = obsJson
                     .hcursor
                     .downField("title")
                     .as[String]
                     .getOrElse("")
        val hasTargets = obsJson
                          .hcursor
                          .downFields("targetEnvironment", "asterism")
                          .values
                          .exists(_.nonEmpty)
        (role, title, hasTargets)

      val specPhotoCount = calibrationsWithRoleAndTargets.count(_._1.contains(CalibrationRole.SpectroPhotometric))
      val twilightCount = calibrationsWithRoleAndTargets.count(_._1.contains(CalibrationRole.Twilight))
      val untargetedCount = calibrationsWithRoleAndTargets.count:
        case (Some(role), _, false) => true  // no target
        case _                      => false
      val untargetedByTitleCount = calibrationsWithRoleAndTargets.count:
        case (Some(role), "Untargeted", _) => true
        case _                             => false

      CalibrationCounts(specPhotoCount, twilightCount, untargetedCount, untargetedByTitleCount)

  def updateGmosReadMode(oid: Observation.Id, readMode: GmosAmpReadMode, gain: GmosAmpGain): IO[Unit] =
    query(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gmosNorthLongSlit: {
                  explicitAmpReadMode: ${readMode.tag.toScreamingSnakeCase}
                  explicitAmpGain: ${gain.tag.toScreamingSnakeCase}
                }
              }
            },
            WHERE: {
              id: { EQ: "$oid" }
            }
          }) {
            observations {
              id
            }
          }
        }
      """
    ).void

  def deleteMultipleObservations(oids: List[Observation.Id]): IO[Unit] =
    oids.traverse_ : oid =>
      query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                existence: DELETED
              },
              WHERE: {
                id: { IN: ["$oid"] }
              }
            }) {
              observations {
                id
              }
            }
          }
        """
      )

  def undeleteMultipleObservations(oids: List[Observation.Id]): IO[Unit] =
    oids.traverse_ : oid =>
      query(
        user = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                existence: PRESENT
              },
              WHERE: {
                id: { IN: ["$oid"] }
              },
              includeDeleted: true
            }) {
              observations {
                id
              }
            }
          }
        """
      )

  test("issue #6598: calibration operations produce untargeted calibrations"):
    for {
      // Create a complete GMOS observation
      pid <- createProgram
      tid <- createTargetAs(pi, pid, "Science Target")
      o1  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))

      // Step 1: Create a single obs and calculate calibrations
      _   <- IO.println("Step 1: Setup observation...")
      // Run workflow
      _   <- runObscalcUpdate(pid, o1)
      // Recalculate calibrations
      _   <- recalculateCalibrations(pid, when).flatMap(_._1.traverse(runObscalcUpdate(pid, _)))
      // First pass we get 1 specphoto + 1 telluric
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(1, 1, 0, 0))

      // Step 2: Duplicate the science observation (clone it)
      _   <- IO.println("Step 2: Cloning observation...")
      o2  <- cloneObservationAs(pi, o1)
      // Run workflow
      _   <- runObscalcUpdate(pid, o2)
      _   <- recalculateCalibrations(pid, when).flatMap(_._1.traverse(runObscalcUpdate(pid, _)))
      // Here we have the same 1 specphoto + 1 telluric, two obs one config
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(1, 1, 0, 0))

      // Step 3: Change the read mode of one observation to "Fast, High"
      _   <- IO.println("Step 3: Updating read mode...")
      _   <- updateGmosReadMode(o2, GmosAmpReadMode.Fast, GmosAmpGain.High)
      // Recalculate calibrations - we should now have more sets due to different read modes
      _   <- recalculateCalibrations(pid, when).flatMap(_._1.traverse(runObscalcUpdate(pid, _)))
      // Verify calibrations increased (should have 2 calibrations for each readmode configurations)
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(2, 2, 0, 0))

      // Step 4: Delete science observations
      _   <- IO.println("Step 4: Deleting science observations...")
      _   <- deleteMultipleObservations(List(o1, o2))
      // we deleted science but before running calibrations they are stil present
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(2, 2, 0, 0))
      _   <- recalculateCalibrations(pid, when)
      // No calibrations now
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(0, 0, 0, 0))

      // Step 5: Undo (undelete the observations)
      _   <- IO.println("Step 5: Undeleting observations...")
      _   <- undeleteMultipleObservations(List(o1, o2))
      // Stil at 0
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(0, 0, 0, 0))
      _   <- recalculateCalibrations(pid, when).flatMap(_._1.traverse(runObscalcUpdate(pid, _)))
      // Calibs restored
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(2, 2, 0, 0))

      // Step 6: Change the status of one science observation to "Inactive"
      _   <- IO.println("Step 6: Setting first observation inactive...")
      _   <- setObservationWorkflowState(pi, o1, ObservationWorkflowState.Inactive)
      _   <- recalculateCalibrations(pid, when)
      // Calibs restored, given only one science config it should have 1 specphoto + 1 twilight
      // With the bug we would unteragetd calibratoins observations as a shared target was deleted 
      // on the inactivated observation
      _   <- countCalibrations(pid).assertEquals(CalibrationCounts(1, 1, 0, 0))
    } yield ()
