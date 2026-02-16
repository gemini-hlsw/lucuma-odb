// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.odb.data.ExecutionEventType

class executionEventAdded extends OdbSuite with SubscriptionUtils:

  val pi         = TestUsers.Standard.pi(11,110)
  val service    = TestUsers.service(3)
  val validUsers = List(pi, service)
  val mode       = ObservingModeType.GmosNorthLongSlit

  val initialize: IO[(Program.Id, Observation.Id, Visit.Id)] =
    for
      pid <- createProgramAs(pi)
      oid <- createObservationAs(pi, pid, mode.some)
      vid <- recordVisitAs(service, mode.instrument, oid)
    yield (pid, oid, vid)

  val fieldSelection: String = """
    value {
      visit { id }
      observation {
        id
        program { id }
      }
      eventType
    }
  """

  def eventJson(pid: Program.Id, oid: Observation.Id, vid: Visit.Id, eventType: ExecutionEventType): Json =
    json"""
      {
        "executionEventAdded": {
          "value": {
            "visit": {
              "id": ${vid.asJson}
            },
            "observation": {
              "id": ${oid.asJson},
              "program": {
                "id": ${pid.asJson}
              }
            },
            "eventType": ${eventType.tag.toUpperCase.asJson}
          }
        }
      }
    """

  test("trigger for any event"):
    initialize.flatMap: (pid, oid, vid) =>
      subscriptionExpect(
        user  = pi,
        query = s"""
          subscription {
            executionEventAdded {
              $fieldSelection
            }
          }
        """,
        mutations = Right(
          addSlewEventAs(service, oid, SlewStage.StartSlew)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Slew)
        )
      )

  test("trigger only for one event type"):
    initialize.flatMap: (pid, oid, vid) =>
      subscriptionExpect(
        user  = pi,
        query = s"""
          subscription {
            executionEventAdded(input: { eventType: { EQ: SLEW } }) {
              $fieldSelection
            }
          }
        """,
        mutations = Right(
          recordAtomAs(service, mode.instrument, vid).flatMap: aid =>
            recordStepAs(service, mode.instrument, aid).flatMap: sid =>
              addSlewEventAs(service, oid, SlewStage.StartSlew)       >>
              addSlewEventAs(service, oid, SlewStage.EndSlew)         >>
              addSequenceEventAs(service, vid, SequenceCommand.Start) >>
              addAtomEventAs(service, aid, vid, AtomStage.StartAtom)  >>
              addStepEventAs(service, sid, vid, StepStage.StartStep)
        ),
        expected = List.fill(2)(eventJson(pid, oid, vid, ExecutionEventType.Slew))
      )

  test("trigger anything but one event type"):
    initialize.flatMap: (pid, oid, vid) =>
      subscriptionExpect(
        user  = pi,
        query = s"""
          subscription {
            executionEventAdded(input: { eventType: { NEQ: SLEW } }) {
              $fieldSelection
            }
          }
        """,
        mutations = Right(
          recordAtomAs(service, mode.instrument, vid).flatMap: aid =>
            recordStepAs(service, mode.instrument, aid).flatMap: sid =>
              addSlewEventAs(service, oid, SlewStage.StartSlew)       >>
              addSlewEventAs(service, oid, SlewStage.EndSlew)         >>
              addSequenceEventAs(service, vid, SequenceCommand.Start) >>
              addAtomEventAs(service, aid, vid, AtomStage.StartAtom)  >>
              addStepEventAs(service, sid, vid, StepStage.StartStep)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Sequence),
          eventJson(pid, oid, vid, ExecutionEventType.Atom),
          eventJson(pid, oid, vid, ExecutionEventType.Step)
        )
      )

  test("trigger some event types"):
    initialize.flatMap: (pid, oid, vid) =>
      subscriptionExpect(
        user  = pi,
        query = s"""
          subscription {
            executionEventAdded(input: { eventType: { IN: [ STEP, DATASET ] } }) {
              $fieldSelection
            }
          }
        """,
        mutations = Right(
          recordAtomAs(service, mode.instrument, vid).flatMap: aid =>
            recordStepAs(service, mode.instrument, aid).flatMap: sid =>
              recordDatasetAs(service, sid, "N20250103S0001.fits").flatMap: did =>
                addSlewEventAs(service, oid, SlewStage.StartSlew)         >>
                addSlewEventAs(service, oid, SlewStage.EndSlew)           >>
                addSequenceEventAs(service, vid, SequenceCommand.Start)   >>
                addAtomEventAs(service, aid, vid, AtomStage.StartAtom)    >>
                addStepEventAs(service, sid, vid, StepStage.StartStep)         >>
                addDatasetEventAs(service, did, DatasetStage.StartExpose) >>
                addDatasetEventAs(service, did, DatasetStage.EndExpose)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Step),
          eventJson(pid, oid, vid, ExecutionEventType.Dataset),
          eventJson(pid, oid, vid, ExecutionEventType.Dataset)
        )
      )

  test("trigger not some event types"):
    initialize.flatMap: (pid, oid, vid) =>
      subscriptionExpect(
        user  = pi,
        query = s"""
          subscription {
            executionEventAdded(input: { eventType: { NIN: [ SLEW, STEP, DATASET ] } }) {
              $fieldSelection
            }
          }
        """,
        mutations = Right(
          recordAtomAs(service, mode.instrument, vid).flatMap: aid =>
            recordStepAs(service, mode.instrument, aid).flatMap: sid =>
              recordDatasetAs(service, sid, "N20250103S0002.fits").flatMap: did =>
                addSlewEventAs(service, oid, SlewStage.StartSlew)         >>
                addSlewEventAs(service, oid, SlewStage.EndSlew)           >>
                addSequenceEventAs(service, vid, SequenceCommand.Start)   >>
                addAtomEventAs(service, aid, vid, AtomStage.StartAtom)    >>
                addStepEventAs(service, sid, vid, StepStage.StartStep)         >>
                addDatasetEventAs(service, did, DatasetStage.StartExpose) >>
                addDatasetEventAs(service, did, DatasetStage.EndExpose)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Sequence),
          eventJson(pid, oid, vid, ExecutionEventType.Atom)
        )
      )
