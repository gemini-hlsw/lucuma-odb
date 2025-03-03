// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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


class datasetEdit extends OdbSuite with SubscriptionUtils with DatasetSetupOperations:

  object Group1 {
    val pi       = TestUsers.Standard.pi(11, 110)
    val service  = TestUsers.service(13)
    val staff    = TestUsers.Standard.staff(14, 114)
  }

  object Group2 {
    val pi       = TestUsers.Standard.pi(21, 210)
    val service  = TestUsers.service(23)
    val staff    = TestUsers.Standard.staff(24, 214)
  }

  def validUsers =
    List(
      Group1.pi, Group1.service, Group1.staff,
      Group2.pi, Group2.service, Group2.staff
    )

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
    import Group1._

    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "CREATED", "value":  { "filename": "N18630101S0001.fits", "isWritten": false } } }""",
      json"""{ "datasetEdit":  { "editType": "UPDATED", "value":  { "filename": "N18630101S0001.fits", "isWritten": false } } }"""
    )

    subscriptionExpect(
      user      = pi,
      query     = allEventsSubcription,
      mutations = Right(
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 0, 1, 1).flatTap {
          case (_, List((_, did))) => updateDatasets(staff, DatasetQaState.Pass, did)
          case _                   => sys.error("Expected a single step.")
        } >> IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("only care about complete datasets"):
    import Group1._

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
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 1, 1, 1).flatTap {
          case (_, List((_, List(did)))) =>
            updateDatasets(staff, DatasetQaState.Pass, List(did))     *>
            addDatasetEventAs(service, did, DatasetStage.StartExpose) *>
            addDatasetEventAs(service, did, DatasetStage.EndWrite)
          case _                         =>
            sys.error("Expected a single dataset.")
        } >>
        IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("only care about one observation"):
    import Group1._

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

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 2, 1, 1).flatTap {
      case (oid, List((_, List(did)))) =>

        subscriptionExpect(
          user      = pi,
          query     = subcription(oid),
          mutations = Right(
            recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 3, 1, 1) *>
            updateDatasets(staff, DatasetQaState.Pass, List(did)) >> IO.sleep(1.second)
          ),
          expected  = allEvents
        )

      case _                           =>
        sys.error("Expected a single dataset.")
    }

  test("only care about one dataset"):
    import Group1._

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

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 4, 1, 2).flatTap {
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
    import Group1._

    val allEvents = List(
      json"""{ "datasetEdit":  { "editType": "CREATED", "value":  { "filename": "N18630101S0007.fits", "isWritten": false } } }"""
    )

    subscriptionExpect(
      user      = pi,
      query     = allEventsSubcription,
      mutations = Right(
        recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 6, 1, 1) *>
        recordDatasets(ObservingModeType.GmosNorthLongSlit, Group2.pi, service, 7, 1, 1).flatTap {
          case (_, List((_, did))) => updateDatasets(staff, DatasetQaState.Pass, did)
          case _                   => sys.error("Expected a single step.")
        } >> IO.sleep(1.second)
      ),
      expected  = allEvents
    )

  test("datasetId in result works"):
    import Group1._

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

    recordDatasets(ObservingModeType.GmosNorthLongSlit, pi, service, 8, 1, 1).flatTap {
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