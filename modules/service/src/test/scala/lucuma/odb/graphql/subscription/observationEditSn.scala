// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.subscription

import cats.effect.IO
import cats.syntax.show.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.EditType
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.graphql.query.ObservingModeSetupOperations

class observationEditSn extends ExecutionTestSupport with ObservingModeSetupOperations with SubscriptionUtils:

  def subscriptionQuery(pid: Program.Id) =
    s"""
      subscription {
        observationEdit(input: { programId: "${pid.show}" }) {
          observationId
          editType
          value {
            id
            execution {
              config {
                gmosNorth {
                  science {
                    nextAtom {
                      observeClass
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

  def subscriptionResponse(oid: Observation.Id): Json =
      Json.obj(
        "observationEdit" -> Json.obj(
          "observationId" -> Json.fromString(oid.show),
          "editType"      -> Json.fromString(EditType.Updated.tag.toUpperCase),
          "value" -> Json.obj(
            "id"          -> oid.asJson,
            "execution" -> Json.obj(
              "config" -> Json.obj(
                "gmosNorth" -> Json.obj(
                  "science" -> Json.obj(
                    "nextAtom" -> Json.obj(
                      "observeClass" -> "SCIENCE".asJson
                    )
                  )
                )
              )
            )
          )
        )
      )

  def updateSn(
    oid: Observation.Id
  ): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              scienceRequirements: {
                spectroscopy: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 99
                      at: { nanometers: 500 }
                    }
                  }
                }
              }
            },
            WHERE: {
              id: { EQ: "$oid" }
            }
          }) {
            observations {
              scienceRequirements {
                spectroscopy {
                  exposureTimeMode {
                    signalToNoise {
                      value
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).void

  test("triggers for editing s/n"):
    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- generateOrFail(pid, oid)
      // expect two responses, one from editing the S/N, one because our query
      // requests the sequence which requires a cache update
      _   <- subscriptionExpect(
        user      = pi,
        query     = subscriptionQuery(pid),
        mutations = Right(sleep >> updateSn(oid)),
        expected  = List(subscriptionResponse(oid), subscriptionResponse(oid))
      )
    yield ()

  test("does not trigger for subsequent generation"):
    for
      pid <- createProgram(pi, "foo")
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- generateOrFail(pid, oid)
      _   <- subscriptionExpect(
        user      = pi,
        query     = subscriptionQuery(pid),
        mutations = Right(
                      sleep                         >>
                      updateSn(oid)                 >>
                      generateOrFail(pid, oid).void >>
                      generateOrFail(pid, oid).void >>
                      sleep
                    ),
        expected  = List(subscriptionResponse(oid), subscriptionResponse(oid))
      )
    yield ()