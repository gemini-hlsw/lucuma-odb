// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Dataset
import lucuma.odb.graphql.query.DatasetSetupOperations

import scala.concurrent.duration.*


class datasetEdit extends OdbSuite with SubscriptionUtils with DatasetSetupOperations with query.ExecutionTestSupportForGmos:

  val allEventsSubcription = """
    subscription {
      datasetEdit {
        editType
        value {
          filename
          isWritten
        }
      }
    }
  """

  test("trigger for my own new datasets"):
    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "CREATED", "value":  { "filename": "N18630101S0001.fits", "isWritten": false } } }""",
      json"""{ "datasetEdit":  { "editType": "UPDATED", "value":  { "filename": "N18630101S0001.fits", "isWritten": false } } }"""
    )

    subscriptionExpect(
      user      = pi,
      query     = allEventsSubcription,
      mutations = Right(
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 0, 1, 1).flatTap {
          case (_, List((_, did))) => updateDatasets(staff, DatasetQaState.Pass, did)
          case _                   => sys.error("Expected a single step.")
        } >> IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("only care about complete datasets"):
    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "UPDATED", "value":  { "filename": "N18630101S0002.fits", "isWritten": true } } }"""
    )

    val subcription = """
      subscription {
        datasetEdit(input: { isWritten: true }) {
          editType
          value {
            filename
            isWritten
          }
        }
      }
    """

    subscriptionExpect(
      user      = pi,
      query     = subcription,
      mutations = Right(
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 1, 1, 1).flatTap {
          case (_, List((_, List(did)))) =>
            updateDatasets(staff, DatasetQaState.Pass, List(did))     *>
            addDatasetEventAs(serviceUser, did, DatasetStage.StartExpose) *>
            addDatasetEventAs(serviceUser, did, DatasetStage.EndWrite)
          case _                         =>
            sys.error("Expected a single dataset.")
        } >>
        IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("only care about one observation"):
    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "UPDATED", "value":  { "filename": "N18630101S0003.fits", "isWritten": false } } }"""
    )

    def subcription(oid: Observation.Id) = s"""
      subscription {
        datasetEdit(input: { observationId: "$oid" }) {
          editType
          value {
            filename
            isWritten
          }
        }
      }
    """

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 2, 1, 1).flatTap {
      case (oid, List((_, List(did)))) =>

        subscriptionExpect(
          user      = pi,
          query     = subcription(oid),
          mutations = Right(
            recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 3, 1, 1) *>
            updateDatasets(staff, DatasetQaState.Pass, List(did)) >> IO.sleep(1.second)
          ),
          expected  = allEvents
        )

      case _                           =>
        sys.error("Expected a single dataset.")
    }

  test("only care about one dataset"):
    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "UPDATED", "value":  { "filename": "N18630101S0005.fits", "isWritten": false } } }"""
    )

    def subcription(did: Dataset.Id) = s"""
      subscription {
        datasetEdit(input: { datasetId: "$did" }) {
          editType
          value {
            filename
            isWritten
          }
        }
      }
    """

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 4, 1, 2).flatTap {
      case (oid, List((_, List(did0, did1)))) =>

        subscriptionExpect(
          user      = pi,
          query     = subcription(did0),
          mutations = Right(
            updateDatasets(staff, DatasetQaState.Pass, List(did0, did1)) >> IO.sleep(1.second)
          ),
          expected  = allEvents
        )

      case _                           =>
        sys.error("Expected two datasets.")
    }

  test("ignore datasets in programs that aren't visible"):
    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "CREATED", "value":  { "filename": "N18630101S0007.fits", "isWritten": false } } }"""
    )

    subscriptionExpect(
      user      = pi,
      query     = allEventsSubcription,
      mutations = Right(
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 6, 1, 1) *>
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi2, serviceUser, 7, 1, 1).flatTap {
          case (_, List((_, did))) => updateDatasets(staff, DatasetQaState.Pass, did)
          case _                   => sys.error("Expected a single step.")
        } >> IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("datasetId in result works"):
    def allEvents(did: Dataset.Id) = List(
      json"""{ "datasetEdit":  { "datasetId": $did }}"""
    )

    val subscription = s"""
      subscription {
        datasetEdit {
          datasetId
        }
      }
    """

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, serviceUser, 8, 1, 1).flatTap {
      case (oid, List((_, List(did)))) =>

        subscriptionExpect(
          user      = pi,
          query     = subscription,
          mutations = Right(
            updateDatasets(staff, DatasetQaState.Pass, List(did)) >> IO.sleep(1.second)
          ),
          expected  = allEvents(did)
        )

      case _                           =>
        sys.error("Expected a single dataset.")
    }