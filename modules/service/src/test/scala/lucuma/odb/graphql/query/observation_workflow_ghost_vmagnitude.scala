// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target

// A GHOST target needs V magnitudes, lack of them shows up as a configuration error.
class observation_workflow_ghost_vmagnitude extends ExecutionTestSupportForGhost:

  private val ghostObs: String =
    """
      ghostIfu: {
        stepCount: 1
        resolutionMode: STANDARD
        red: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 1 }
              count: 1
              at: { nanometers: 500 }
            }
          }
        }
        blue: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 1 }
              count: 1
              at: { nanometers: 500 }
            }
          }
        }
      }
    """

  // A source profile with no V magnitude (R only).
  private val SourceProfileNoV: String =
    """
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: { stellarLibrary: B5_III },
            brightnesses: [
              { band: R, value: 15.0, units: VEGA_MAGNITUDE }
            ]
          }
        }
      }
    """

  private def workflowValidationErrors(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow {
              value {
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("observation", "workflow", "value", "validationErrors")
        .values
        .toList
        .flatten
        .flatMap(_.hcursor.downField("messages").values.toList.flatten)
        .map(_.asString.getOrElse(""))

  private def expectMissingVMagnitude(pid: Program.Id, oid: Observation.Id, expected: Boolean): IO[Unit] =
    runObscalcUpdate(pid, oid) >>
      workflowValidationErrors(oid).map: messages =>
        assertEquals(messages.contains("Please add a V magnitude."), expected)

  test("GHOST observation with a target lacking a V magnitude reports a validation error"):
    for
      pid <- createProgram
      tid <- createTargetAs(pi, pid, sourceProfile = SourceProfileNoV)
      oid <- createObservationWithModeAs(pi, pid, List(tid), ghostObs)
      _   <- expectMissingVMagnitude(pid, oid, expected = true)
    yield ()

  test("GHOST observation with a target that has a V magnitude reports no such error"):
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), ghostObs)
      _   <- expectMissingVMagnitude(pid, oid, expected = false)
    yield ()

  test("adding a V-less target to a GHOST observation via updateAsterisms reports a validation error"):
    for
      pid <- createProgram
      tV  <- createTargetWithProfileAs(pi, pid)        // has V
      tNo <- createTargetAs(pi, pid, sourceProfile = SourceProfileNoV)
      oid <- createObservationWithModeAs(pi, pid, List(tV), ghostObs)
      _   <- query(
               pi,
               s"""
                 mutation {
                   updateAsterisms(input: {
                     SET: { ADD: [ ${tNo.asJson} ] }
                     WHERE: { id: { IN: [ ${oid.asJson} ] } }
                   }) {
                     observations { id }
                   }
                 }
               """
             )
      _   <- expectMissingVMagnitude(pid, oid, expected = true)
    yield ()

  test("switching an observation to GHOST with a V-less target reports a validation error"):
    for
      pid <- createProgram
      tNo <- createTargetAs(pi, pid, sourceProfile = SourceProfileNoV)
      oid <- createObservationWithNoModeAs(pi, pid, tNo)
      _   <- query(
               pi,
               s"""
                 mutation {
                   updateObservations(input: {
                     SET: {
                       observingMode: { ${ghostObs} }
                     }
                     WHERE: { id: { IN: [ ${oid.asJson} ] } }
                   }) {
                     observations { id }
                   }
                 }
               """
             )
      _   <- expectMissingVMagnitude(pid, oid, expected = true)
    yield ()

  test("removing the V magnitude from a GHOST target via updateTargets reports a validation error"):
    for
      pid <- createProgram
      tV  <- createTargetWithProfileAs(pi, pid)        // has V
      oid <- createObservationWithModeAs(pi, pid, List(tV), ghostObs)
      _   <- query(
               pi,
               s"""
                 mutation {
                   updateTargets(input: {
                     SET: {
                       sourceProfile: {
                         point: {
                           bandNormalized: {
                             sed: { stellarLibrary: B5_III },
                             brightnesses: [
                               { band: R, value: 15.0, units: VEGA_MAGNITUDE }
                             ]
                           }
                         }
                       }
                     }
                     WHERE: { id: { EQ: ${tV.asJson} } }
                   }) {
                     targets { id }
                   }
                 }
               """
             )
      _   <- expectMissingVMagnitude(pid, oid, expected = true)
    yield ()
