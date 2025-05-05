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
  val AcquisitionTotal: Long = 219362500l
  val ScienceTotal: Long     = 784200000l

  def subscriptionQuery(pid: Program.Id) =
    s"""
      subscription {
        observationEdit(input: { programId: "${pid.show}" }) {
          observationId
          editType
          value {
            id
            itc {
              science {
                selected {
                  exposureCount
                }
              }
            }
            execution {
              digest {
                acquisition {
                  timeEstimate {
                    total { microseconds }
                  }
                }
                science {
                  timeEstimate {
                    total { microseconds }
                  }
                }
              }
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
            "itc" -> Json.obj(
              "science" -> Json.obj(
                "selected" -> Json.obj(
                  "exposureCount" -> 6.asJson
                )
              )
            ),
            "execution" -> Json.obj(
              "digest" -> Json.obj(
                "acquisition" -> Json.obj(
                  "timeEstimate" -> Json.obj(
                    "total" -> Json.obj(
                      "microseconds" -> AcquisitionTotal.asJson
                    )
                  )
                ),
                "science" -> Json.obj(
                  "timeEstimate" -> Json.obj(
                    "total" -> Json.obj(
                      "microseconds" -> ScienceTotal.asJson
                    )
                  )
                )
              ),
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
                  wavelength: { micrometers: 0.700000 }
                  resolution: 1000
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 99
                      at: { nanometers: 500 }
                    }
                  }
                  wavelengthCoverage: { micrometers: 0.400000 }
                  focalPlane: null
                  focalPlaneAngle: null
                  capability: null
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
      _   <- subscriptionExpect(
        user      = pi,
        query     = subscriptionQuery(pid),
        mutations = Right(sleep >> updateSn(oid)),
        expected  = List(subscriptionResponse(oid), subscriptionResponse(oid))
      )
    yield ()