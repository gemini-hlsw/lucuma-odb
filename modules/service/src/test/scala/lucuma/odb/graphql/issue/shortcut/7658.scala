// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package issue.shortcut

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.mutation.UpdateObservationsOps
import lucuma.odb.graphql.query.ExecutionTestSupportForFlamingos2
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.graphql.query.ExecutionTestSupportForGnirs

// https://app.shortcut.com/lucuma/story/7658
trait ShortCut_7658_Helpers extends OdbSuite with UpdateObservationsOps:
  // The helpers take the observing mode input field; the acquisition and
  // exposure time mode inputs are shaped alike across these modes.
  extension (modeField: String)
    private def expectedObservingMode(inner: Json): Json =
      Json.obj(modeField -> inner)

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

  private def acquisitionUpdate(modeField: String): String =
    s"""
      observingMode: {
        $modeField: {
          $AcquisitionUpdate
        }
      }
    """

  private def nonAcquisitionUpdate(modeField: String): String =
    s"""
      observingMode: {
        $modeField: {
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

  private def updateAcquisitionMutation(oid: Observation.Id, modeField: String): String =
    updateObservingModeMutation(oid, acquisitionUpdate(modeField))

  private def updateNonAcquisitionMutation(oid: Observation.Id, modeField: String): String =
    updateObservingModeMutation(oid, nonAcquisitionUpdate(modeField))

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

  private def observingModeQuery(modeField: String, subQuery: String): String =
    s"""
      observations {
        observingMode {
          $modeField {
            $subQuery
          }
        }
      }
    """

  private def acquisitionQuery(modeField: String): String = 
    observingModeQuery(modeField, AcquisitionQuery)

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

  private def expectedAcquisition(modeField: String): Json =
    expectedObservingMode(modeField.expectedObservingMode(ExpectedAcquisition))

  protected val ErrorForOngoing: String = "is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Completed)"
  protected val ErrorForCompleted: String = "is ineligible for this operation due to its workflow state (Completed with allowed transition to Ongoing)"

  def testFailedAcquistion(user: User, oid: Observation.Id, modeField: String, error: String = ErrorForOngoing): IO[Unit] =
    expectOdbError(
      user = user,
      query = updateAcquisitionMutation(oid, modeField),
      expected = {
        case OdbError.InvalidObservation(_, Some(msg)) if msg.contains(error) => ()
      }
    )

  def testFailedNonAcquistion(user: User, oid: Observation.Id, modeField: String, error: String = ErrorForOngoing): IO[Unit] =
    expectOdbError(
      user = user,
      query = updateNonAcquisitionMutation(oid, modeField),
      expected = {
        case OdbError.InvalidObservation(_, Some(msg)) if msg.contains(error) => ()
      }
    )

  def testSuccessAcquisition(user: User, oid: Observation.Id, modeField: String): IO[Unit] =
    updateObservation(
      user = user,
      oid,
      update = acquisitionUpdate(modeField),
      query = acquisitionQuery(modeField),
      expected = expectedAcquisition(modeField).asRight
    )

class ShortCut_7658_Flamingos2 extends ExecutionTestSupportForFlamingos2 with ShortCut_7658_Helpers:
  test("flamingos2: pi cannot update acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedAcquistion(pi, oid, "flamingos2LongSlit")

  test("flamingos2: pi cannot update non-acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, "flamingos2LongSlit")

  test("flamingos2: staff can update acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, "flamingos2LongSlit")

  test("flamingos2: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingFlamingos2Observation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, "flamingos2LongSlit", ErrorForCompleted)
    yield ()

  test("flamingos2: staff cannot update non-acquisition for ongoing observation"):
    createOngoingFlamingos2Observation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, "flamingos2LongSlit")

class ShortCut_7658_Gmos extends ExecutionTestSupportForGmos with ShortCut_7658_Helpers:
  test("gmosNorth: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, "gmosNorthLongSlit")

  test("gmosNorth: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, "gmosNorthLongSlit")

  test("gmosNorth: staff can update acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, "gmosNorthLongSlit")

  test("gmosNorth: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGmosNorthObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, "gmosNorthLongSlit", ErrorForCompleted)
    yield ()

  test("gmosNorth: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, "gmosNorthLongSlit")

  test("gmosSouth: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, "gmosSouthLongSlit")

  test("gmosSouth: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, "gmosSouthLongSlit")

  test("gmosSouth: staff can update acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, "gmosSouthLongSlit")

  test("gmosSouth: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGmosSouthObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, "gmosSouthLongSlit", ErrorForCompleted)
    yield ()

  test("gmosSouth: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosSouthObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, "gmosSouthLongSlit")


// GNIRS has no mode-level exposure time mode and its acquisition input is
// shaped differently, so it gets its own update strings.
class ShortCut_7658_Gnirs extends ExecutionTestSupportForGnirs with ShortCut_7658_Helpers:

  private def gnirsUpdate(inner: String): String =
    s"""
      observingMode: {
        gnirsSpectroscopy: {
          $inner
        }
      }
    """

  private val AcquisitionUpdate: String =
    gnirsUpdate("acquisition: { explicitAcquisitionType: BRIGHT }")

  private val NonAcquisitionUpdate: String =
    gnirsUpdate("explicitReadMode: FAINT")

  private val AcquisitionQuery: String =
    """
      observations {
        observingMode {
          gnirsSpectroscopy {
            acquisition { explicitAcquisitionType }
          }
        }
      }
    """

  private val ExpectedAcquisition: Json =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gnirsSpectroscopy": {
                  "acquisition": { "explicitAcquisitionType": "BRIGHT" }
                }
              }
            }
          ]
        }
      }
    """

  private def testFailed(user: User, oid: Observation.Id, update: String, error: String = ErrorForOngoing): IO[Unit] =
    expectOdbError(
      user     = user,
      query    = updateObservationsMutation(oid, update, "observations { id }"),
      expected = {
        case OdbError.InvalidObservation(_, Some(msg)) if msg.contains(error) => ()
      }
    )

  test("gnirs: pi cannot update acquisition for ongoing observation"):
    createOngoingGnirsLongSlitObservation.flatMap: oid =>
      testFailed(pi, oid, AcquisitionUpdate)

  test("gnirs: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGnirsLongSlitObservation.flatMap: oid =>
      testFailed(pi, oid, NonAcquisitionUpdate)

  test("gnirs: staff can update acquisition for ongoing observation"):
    createOngoingGnirsLongSlitObservation.flatMap: oid =>
      updateObservation(
        user     = staff,
        oid      = oid,
        update   = AcquisitionUpdate,
        query    = AcquisitionQuery,
        expected = ExpectedAcquisition.asRight
      )

  test("gnirs: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGnirsLongSlitObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailed(staff, oid, AcquisitionUpdate, ErrorForCompleted)
    yield ()

  test("gnirs: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGnirsLongSlitObservation.flatMap: oid =>
      testFailed(staff, oid, NonAcquisitionUpdate)

class ShortCut_7658_GmosIfu extends ExecutionTestSupportForGmos with ShortCut_7658_Helpers:
  test("gmosNorthIfu: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosNorthIfuObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, "gmosNorthIfu")

  test("gmosNorthIfu: pi cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthIfuObservation.flatMap: oid =>
      testFailedNonAcquistion(pi, oid, "gmosNorthIfu")

  test("gmosNorthIfu: staff can update acquisition for ongoing observation"):
    createOngoingGmosNorthIfuObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, "gmosNorthIfu")

  test("gmosNorthIfu: staff cannot update acquisition for completed observation"):
    for
      oid <- createOngoingGmosNorthIfuObservation
      _   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Completed)
      _   <- testFailedAcquistion(staff, oid, "gmosNorthIfu", ErrorForCompleted)
    yield ()

  test("gmosNorthIfu: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosNorthIfuObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, "gmosNorthIfu")

  test("gmosSouthIfu: pi cannot update acquisition for ongoing observation"):
    createOngoingGmosSouthIfuObservation.flatMap: oid =>
      testFailedAcquistion(pi, oid, "gmosSouthIfu")

  test("gmosSouthIfu: staff can update acquisition for ongoing observation"):
    createOngoingGmosSouthIfuObservation.flatMap: oid =>
      testSuccessAcquisition(staff, oid, "gmosSouthIfu")

  test("gmosSouthIfu: staff cannot update non-acquisition for ongoing observation"):
    createOngoingGmosSouthIfuObservation.flatMap: oid =>
      testFailedNonAcquistion(staff, oid, "gmosSouthIfu")
