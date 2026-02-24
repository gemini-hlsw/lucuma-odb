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
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ExecutionEventType

class executionEventAdded extends OdbSuite with SubscriptionUtils with query.ExecutionTestSupportForGmos:

  val mode = ObservingModeType.GmosNorthLongSlit

  val initialize: IO[(Program.Id, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationAs(pi, pid, mode.some, tid)
      vid <- recordVisitAs(serviceUser, mode.instrument, oid)
      ids <- scienceSequenceIds(serviceUser, oid)
      aid  = ids.head._1
      sid  = ids.head._2.head
    yield (pid, oid, vid, aid, sid)

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
    initialize.flatMap: (pid, oid, vid, _, _) =>
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
          addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Slew)
        )
      )

  test("trigger only for one event type"):
    initialize.flatMap: (pid, oid, vid, aid, sid) =>
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
          addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)       >>
          addSlewEventAs(serviceUser, oid, SlewStage.EndSlew)         >>
          addSequenceEventAs(serviceUser, vid, SequenceCommand.Start) >>
          addAtomEventAs(serviceUser, aid, vid, AtomStage.StartAtom)  >>
          addStepEventAs(serviceUser, sid, vid, StepStage.StartStep)
        ),
        expected = List.fill(2)(eventJson(pid, oid, vid, ExecutionEventType.Slew))
      )

  test("trigger anything but one event type"):
    initialize.flatMap: (pid, oid, vid, aid, sid) =>
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
          addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)       >>
          addSlewEventAs(serviceUser, oid, SlewStage.EndSlew)         >>
          addSequenceEventAs(serviceUser, vid, SequenceCommand.Start) >>
          addAtomEventAs(serviceUser, aid, vid, AtomStage.StartAtom)  >>
          addStepEventAs(serviceUser, sid, vid, StepStage.StartStep)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Sequence),
          eventJson(pid, oid, vid, ExecutionEventType.Atom),
          eventJson(pid, oid, vid, ExecutionEventType.Step)
        )
      )

  test("trigger some event types"):
    initialize.flatMap: (pid, oid, vid, aid, sid) =>
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
          addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)         >>
          addSlewEventAs(serviceUser, oid, SlewStage.EndSlew)           >>
          addSequenceEventAs(serviceUser, vid, SequenceCommand.Start)   >>
          addAtomEventAs(serviceUser, aid, vid, AtomStage.StartAtom)    >>
          addStepEventAs(serviceUser, sid, vid, StepStage.StartStep)    >>
          recordDatasetAs(serviceUser, sid, vid, "N20250103S0001.fits").flatMap: did =>
            addDatasetEventAs(serviceUser, did, DatasetStage.StartExpose) >>
            addDatasetEventAs(serviceUser, did, DatasetStage.EndExpose)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Step),
          eventJson(pid, oid, vid, ExecutionEventType.Dataset),
          eventJson(pid, oid, vid, ExecutionEventType.Dataset)
        )
      )

  test("trigger not some event types"):
    initialize.flatMap: (pid, oid, vid, aid, sid) =>
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
          addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)         >>
          addSlewEventAs(serviceUser, oid, SlewStage.EndSlew)           >>
          addSequenceEventAs(serviceUser, vid, SequenceCommand.Start)   >>
          addAtomEventAs(serviceUser, aid, vid, AtomStage.StartAtom)    >>
          addStepEventAs(serviceUser, sid, vid, StepStage.StartStep)    >>
          recordDatasetAs(serviceUser, sid, vid, "N20250103S0002.fits").flatMap: did =>
            addDatasetEventAs(serviceUser, did, DatasetStage.StartExpose) >>
            addDatasetEventAs(serviceUser, did, DatasetStage.EndExpose)
        ),
        expected = List(
          eventJson(pid, oid, vid, ExecutionEventType.Sequence),
          eventJson(pid, oid, vid, ExecutionEventType.Atom)
        )
      )
