// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.syntax.either.*
import io.circe.Json
import io.circe.JsonObject
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

// OBSCALC TODO: At least some of these tests will be irrelevant and should be
// removed.  They are about triggering obs edit events on cached execution
// digest update but the cache itself will be deleted and replaced with the
// obscalc backgroup update version.

class observationEditOnCachedResultUpdate extends ExecutionTestSupportForGmos with SubscriptionUtils:

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
                  value {
                    science {
                      atomCount
                    }
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
        mutations = runObscalcUpdate(pid, oid).asRight[List[(String, Option[JsonObject])]],
        expected  = List(updateResponse, updateResponse)  // caches ITC and then sequence digest
      )
    yield ()