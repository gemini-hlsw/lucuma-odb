// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.Semester
import lucuma.core.util.CalculationState
import lucuma.odb.data.EditType
import lucuma.odb.service.ObscalcServiceSuiteSupport
import lucuma.odb.service.Services

import scala.concurrent.duration.*

class obscalcUpdate extends ObscalcServiceSuiteSupport:
  protected val sleep: IO[Unit] =
    IO.sleep(5000.millis)

  test("trigger for new observations"):
    val setup = for
      p <- createProgramAs(pi, "foo")
      t <- createTargetWithProfileAs(pi, p)
    yield (p, t)

    setup.flatMap: (pid, tid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value {
                title
              }
            }
          }
        """,
        mutations = createGmosNorthLongSlitObservationAs(pi, pid, List(tid)).asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> Json.Null,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Created.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "title" -> "V1647 Orionis".asJson  // Target title
              )
            )
          )
        )
      )

  val setup = for
    _ <- cleanup
    p <- createProgramAs(pi, "foo")
    t <- createTargetWithProfileAs(pi, p)
    o <- createGmosSouthLongSlitObservationAs(pi, p, List(t))
  yield (p, o)

  test("trigger for transition to calculating"):
    setup.flatMap: (_, oid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = load.asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Calculating.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )

  test("trigger for CfP assignment"):
    val s = for {
      (pid, oid) <- setup
      cid        <- createCallForProposalsAs(staff)
      _          <- addProposal(pi, pid)
    } yield (pid, oid, cid)
    s.flatMap: (pid, oid, cid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = setCallId(pi, pid, cid).asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )

  test("trigger for CfP assignment change"):
    val s = for {
      (pid, oid) <- setup
      cid1       <- createCallForProposalsAs(staff)
      _          <- addProposal(pi, pid, cid1.some)
      cid2       <- createCallForProposalsAs(staff, semester = Semester.unsafeFromString("2025B"))
    } yield (pid, oid, cid2)
    s.flatMap: (pid, oid, cid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = setCallId(pi, pid, cid).asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )

  test("trigger for CfP coordinateLimits change"):
    val set = "{ coordinateLimits: { north: {raStart: {degrees: 17.3}}} }"
    val s = for {
      (pid, oid) <- setup
      cid        <- createCallForProposalsAs(staff)
      _          <- addProposal(pi, pid, cid.some)
    } yield (oid, cid)
    s.flatMap: (oid, cid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = updateCallForProposalsAs(staff, cid, set).asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )
    
  test("trigger for CfP instrument change"):
    val s = for {
      (pid, oid) <- setup
      cid        <- createCallForProposalsAs(staff)
      _          <- addProposal(pi, pid, cid.some)
    } yield (oid, cid)
    s.flatMap: (oid, cid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = updateCallForProposalsAs(staff, cid, "{ instruments: [GMOS_NORTH] }").asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )
    
  def deleteCalibrationObservation(oid: Observation.Id): IO[Unit] =
    withServices(pi): services =>
      services.transactionally {
        Services.asSuperUser:
          services.observationService.deleteCalibrationObservations(NonEmptyList.one(oid))
      }
    .void

  test("trigger for hard delete"):
    setup.flatMap: (_, oid) =>
      setObservationCalibratioRole(oid, Some(CalibrationRole.Telluric)) >>
      subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          obscalcUpdate {
            observationId
            oldState
            newState
            editType
            value { id }
          }
        }
      """,
      mutations = (deleteCalibrationObservation(oid) >> sleep).asRight,
      expected  = List(
        // The first update comes from when the asterism is deleted (the
        // observation is gone by then so value is null).
        Json.obj(
          "obscalcUpdate" -> Json.obj(
            "observationId" -> oid.asJson,
            "oldState" -> CalculationState.Pending.asJson,
            "newState" -> CalculationState.Pending.asJson,
            "editType" -> EditType.Updated.tag.toUpperCase.asJson,
            "value" -> Json.Null
          )
        ),
        // The second update comes when the t_obscalc entry itself is cleaned up
        // via the ON DELETE CASCADE in the FK reference to t_observation.
        Json.obj(
          "obscalcUpdate" -> Json.obj(
            "observationId" -> oid.asJson,
            "oldState" -> CalculationState.Pending.asJson,
            "newState" -> Json.Null,
            "editType" -> EditType.HardDelete.tag.toUpperCase.asJson,
            "value" -> Json.Null
          )
        )
      )
    )

  test("trigger only on transition to ready"):
    subscriptionExpect(
      user      = pi,
      query     = s"""
        subscription {
          obscalcUpdate(input: {
            newState: { EQ: READY }
          }) {
            oldState
            newState
            editType
          }
        }
      """,
      mutations = (setup.flatMap(_ => load.flatMap(lst => calculateAndUpdate(lst.head)))).asRight,
      expected  = List(
        // Just one event -- when moving from calculating to ready
        Json.obj(
          "obscalcUpdate" -> Json.obj(
            "oldState" -> CalculationState.Calculating.asJson,
            "newState" -> CalculationState.Ready.asJson,
            "editType" -> EditType.Updated.tag.toUpperCase.asJson
          )
        )
      )
    )

  test("trigger for exposure time mode update"):
    def updateEtm(o: Observation.Id): IO[Json] =
      query(
        user  = pi,
        query = s"""
          mutation {
            updateObservations(input: {
              SET: {
                observingMode: {
                  gmosSouthLongSlit: {
                    exposureTimeMode: {
                      signalToNoise: {
                        value: 99.99
                        at: { nanometers: 12 }
                      }
                    }
                  }
                }
              }
              WHERE: { id: { EQ: "$o" } }
            }) {
              observations {
                id
              }
            }
          }
        """
      )

    setup.flatMap: (_, oid) =>
      subscriptionExpect(
        user      = pi,
        query     = s"""
          subscription {
            obscalcUpdate {
              oldState
              newState
              editType
              value { id }
            }
          }
        """,
        mutations = updateEtm(oid).asRight,
        expected  = List(
          Json.obj(
            "obscalcUpdate" -> Json.obj(
              "oldState" -> CalculationState.Pending.asJson,
              "newState" -> CalculationState.Pending.asJson,
              "editType" -> EditType.Updated.tag.toUpperCase.asJson,
              "value" -> Json.obj(
                "id" -> oid.asJson
              )
            )
          )
        )
      )

