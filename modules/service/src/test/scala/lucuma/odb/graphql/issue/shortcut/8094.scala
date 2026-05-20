// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.graphql.mutation.ReplaceSequenceOps
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

// sc-8094: Error reverting configuration with edited sequence
// The user should be able to revert a configuration or switch to a different observing mode
// even if the sequence has been edited.
class ShortCut_8094 extends ExecutionTestSupportForGmos with ReplaceSequenceOps: 

  def removeMutation(oid: Observation.Id): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: {
            observingMode: null
          }
          WHERE: {
            id: {
              EQ: ${oid.asJson}
            }
          }
        }) {
          observations {
            id
            observingMode {
              instrument
            }
          }
        }
      }
    """

  def removeObservingMode(user: User, oid: Observation.Id): IO[Unit] =
    expect(
      user = user,
      query = removeMutation(oid),
      expected = Right(
        json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $oid,
                  "observingMode": null
                }
              ]
            }
          }
        """
      )
    )

  def updateToF2Mutation(oid: Observation.Id): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: {
            observingMode: {
              flamingos2LongSlit: {
                disperser: R1200_JH
                filter: Y
                fpu: LONG_SLIT_2
                exposureTimeMode: {
                  signalToNoise: {
                    value: 30.0
                    at: { nanometers: 345.67 }
                  }
                }
              }
            }
          }
          WHERE: {
            id: {
              EQ: ${oid.asJson}
            }
          }
        }
      ){
        observations {
          id
          observingMode {
            instrument
          }
        }
      }
    }
    """

  def updateToF2ObservingMode(user: User, oid: Observation.Id): IO[Unit] =
    expect(
      user = user,
      query = updateToF2Mutation(oid),
      expected = Right(
        json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $oid,
                  "observingMode": {
                    "instrument": "FLAMINGOS2"
                  }
                }
              ]
            }
          }
        """
      )
    )

  test("Remove the observing mode for an observation with an edited sequence"):
    for 
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      in   = input(oid, SequenceType.Science, atomInput("a", stepInput(GmosNorthFilter.GPrime)))
      _   <- query(user = pi, query = mutation(Instrument.GmosNorth, in))
      _   <- removeObservingMode(pi, oid)
    yield ()

  test("Update to a different observing mode for an observation with an edited sequence"):
    for 
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      in   = input(oid, SequenceType.Science, atomInput("a", stepInput(GmosNorthFilter.GPrime)))
      _   <- query(user = pi, query = mutation(Instrument.GmosNorth, in))
      _   <- updateToF2ObservingMode(pi, oid)
    yield ()
  
