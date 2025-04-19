// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ExecutionTestSupport

class observationEditOnCachedResultUpdate extends ExecutionTestSupport with SubscriptionUtils:

  def observationUpdateSubscription(oid: Observation.Id): String =
    s"""
      subscription {
        observationEdit(input: { observationId: "$oid" }) {
          editType
        }
      }
    """

  def programUpdateSubscription(pid: Program.Id): String =
    s"""
      subscription {
        observationEdit(input: { programId: "$pid" }) {
          editType
        }
      }
    """

  val updateResponse: Json =
    json"""
      {
        "observationEdit": {
          "editType": "UPDATED"
        }
      }
    """

  def requestItcResult(user: User, oid: Observation.Id) =
    sleep >>
      query(
        user = user,
        query = s"""
          query {
            observation(observationId: "$oid") {
              itc {
                science {
                  selected {
                    exposureCount
                  }
                }
              }
            }
          }
        """
      ).void

  def requestSequenceDigest(user: User, oid: Observation.Id) =
    sleep >>
      query(
        user = user,
        query = s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  science {
                    atomCount
                  }
                }
              }
            }
          }
        """
      )

  test("triggers when caching an ITC result"):
    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))

      _   <- subscriptionExpect(
        user      = pi,
        query     = observationUpdateSubscription(oid),
        mutations = Right(requestItcResult(pi, oid)),
        expected  = List(updateResponse)
      )
    yield ()

  test("triggers when caching sequence digest"):
    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))

      _   <- subscriptionExpect(
        user      = pi,
        query     = observationUpdateSubscription(oid),
        mutations = Right(requestSequenceDigest(pi, oid)),
        expected  = List(updateResponse, updateResponse)  // caches ITC and then sequence digest
      )
    yield ()

  test("doesn't trigger when querying after cached"):
    def timeQuery(pid: Program.Id) =
     sleep >>
       query(
         user  = pi,
         query = s"""
           query {
             program(programId: "$pid") {
               timeEstimateRange {
                 minimum { program { microseconds } }
               }
             }
           }
         """
       )

    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- subscriptionExpect(
        user      = pi,
        query     = programUpdateSubscription(pid),
        mutations = Right(timeQuery(pid)),
        expected  = List(updateResponse, updateResponse)
      )
      _   <- subscriptionExpect(
        user      = pi,
        query     = programUpdateSubscription(pid),
        mutations = Right(timeQuery(pid)),
        expected  = List()
      )
    yield ()