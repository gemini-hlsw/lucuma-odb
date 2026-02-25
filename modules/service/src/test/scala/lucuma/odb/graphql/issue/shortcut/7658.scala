// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.mutation.UpdateObservationsOps
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

// https://app.shortcut.com/lucuma/story/7658
trait ShortCut_7658_Helpers extends OdbSuite with UpdateObservationsOps:
  extension (i: Instrument)
    private def fieldName: String = i match
      case Instrument.Flamingos2 => "flamingos2LongSlit"
      case Instrument.GmosNorth  => "gmosNorthLongSlit"
      case Instrument.GmosSouth  => "gmosSouthLongSlit"
      case _ => throw new Exception("Unexpected Instrument")
    
    private def expectedObservingMode(inner: Json): Json =
      Json.obj(fieldName -> inner)

  private val ExposureTimeModeUpdate: String =
    """
      exposureTimeMode: {
        signalToNoise: {
          value: 100
          at: { picometers: 10000 }
        }
      }
    """

  private val AcquisitionUpdate: String =
    s"""
      acquisition: {
        $ExposureTimeModeUpdate
      }
    """

  private def acquisitionUpdate(i: Instrument): String =
    s"""
      observingMode: {
        ${i.fieldName}: {
          $AcquisitionUpdate
        }
      }
    """

  private def nonAcquisitionUpdate(i: Instrument): String =
    s"""
      observingMode: {
        ${i.fieldName}: {
          $ExposureTimeModeUpdate
        }
      }
    """
  
  private def updateObservingModeMutation(oid: Observation.Id, observingMode: String): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: {
              $observingMode
          }
          WHERE: { id: { EQ: "${oid}" } }
        }) {
          observations { id }
        }
      }
    """

  private def updateAcquisitionMutation(oid: Observation.Id, i: Instrument): String =
    updateObservingModeMutation(oid, acquisitionUpdate(i))

  private def updateNonAcquisitionMutation(oid: Observation.Id, i: Instrument): String =
    updateObservingModeMutation(oid, nonAcquisitionUpdate(i))

  private val EtmQuery: String =
    """
      exposureTimeMode {
        signalToNoise {
          value
          at { picometers }
        }
      }
    """

  private val AcquisitionQuery: String =
    s"""
      acquisition {
        $EtmQuery
      }
    """

  private def observingModeQuery(i: Instrument, subQuery: String): String =
    s"""
      observations {
        observingMode {
          ${i.fieldName} {
            $subQuery
          }
        }
      }
    """

  private def acquisitionQuery(i: Instrument): String = 
    observingModeQuery(i, AcquisitionQuery)

  private val ExpectedEtm: Json =
    json"""
    {
      "exposureTimeMode": {
        "signalToNoise": {
          "value": 100.000,
          "at": {
            "picometers": 10000
          }
        }
      }
    }
    """

  private val ExpectedAcquisition: Json =
    json"""
      {
        "acquisition": $ExpectedEtm
      }
    """

  private def expectedObservingMode(inner: Json): Json =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": $inner
            }
          ]
        }
      }
    """

  private def expectedAcquisition(i: Instrument): Json =
    expectedObservingMode(i.expectedObservingMode(ExpectedAcquisition))

  protected val ErrorForOngoing: String = "is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed)"
  protected val ErrorForCompleted: String = "is ineligible for this operation due to its workflow state (Completed with allowed transition to Ongoing)"

  def testFailedAcquistion(user: User, oid: Observation.Id, i: Instrument, error: String = ErrorForOngoing): IO[Unit] =
    expectOdbError(
      user = user,
      query = updateAcquisitionMutation(oid, i),
      expected = {
        case OdbError.InvalidObservation(_, Some(msg)) if msg.contains(error) => ()
      }
    )

  def testFailedNonAcquistion(user: User, oid: Observation.Id, i: Instrument, error: String = ErrorForOngoing): IO[Unit] =
    expectOdbError(
      user = user,
      query = updateNonAcquisitionMutation(oid, i),
      expected = {
        case OdbError.InvalidObservation(_, Some(msg)) if msg.contains(error) => ()
      }
    )

  def testSuccessAcquisition(user: User, oid: Observation.Id, i: Instrument): IO[Unit] =
    updateObservation(
      user = user,
      oid,
      update = acquisitionUpdate(i),
      query = acquisitionQuery(i),
      expected = expectedAcquisition(i).asRight
    )

class ShortCut_7658_Flamingos2 extends ExecutionTestSupportForFlamingos2 with ShortCut_7658_Helpers:
  test("flamingos2: pi cannot update acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedAcquistion(pi, oid, Instrument.Flamingos2)

  test("flamingos2: pi cannot update non-acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, Instrument.Flamingos2)

  test("flamingos2: staff can update acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, Instrument.Flamingos2)

  test("flamingos2: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingFlamingos2Observation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, Instrument.Flamingos2, ErrorForCompleted)
    yield ()

  test("flamingos2: staff cannot update non-acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, Instrument.Flamingos2)

class ShortCut_7658_Gmos extends ExecutionTestSupportForGmos with ShortCut_7658_Helpers:
  test("gmosNorth: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, Instrument.GmosNorth)

  test("gmosNorth: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, Instrument.GmosNorth)

  test("gmosNorth: staff can update acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, Instrument.GmosNorth)

  test("gmosNorth: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGmosNorthObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, Instrument.GmosNorth, ErrorForCompleted)
    yield ()

  test("gmosNorth: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, Instrument.GmosNorth)

  test("gmosSouth: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, Instrument.GmosSouth)

  test("gmosSouth: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, Instrument.GmosSouth)

  test("gmosSouth: staff can update acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, Instrument.GmosSouth)

  test("gmosSouth: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGmosSouthObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, Instrument.GmosSouth, ErrorForCompleted)
    yield ()

  test("gmosSouth: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, Instrument.GmosSouth)

