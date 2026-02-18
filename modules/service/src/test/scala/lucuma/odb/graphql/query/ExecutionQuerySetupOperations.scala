// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*

import scala.collection.immutable.ListMap

trait ExecutionQuerySetupOperations extends DatabaseOperations with GenerationTestSupport { this: OdbSuite =>

  import ExecutionQuerySetupOperations.*

  val DatasetFilenamePrefix: String =
    "N18630101S"

  def recordDataset(
    setup:   Setup,
    user:    User,
    sid:     Step.Id,
    vid:     Visit.Id,
    atom:    Int,
    step:    Int,
    dataset: Int
  ): IO[DatasetNode] =

    val idx = setup.offset                                                     +
               atom * (setup.stepCount * setup.datasetCount)                   +
               step * setup.datasetCount                                       +
               dataset                                                         +
               1

    val stages = List(
      DatasetStage.StartExpose,
      DatasetStage.EndExpose,
      DatasetStage.StartReadout,
      DatasetStage.EndReadout,
      DatasetStage.StartWrite,
      DatasetStage.EndWrite
    )

    for
      did <- recordDatasetAs(user, sid, vid, f"$DatasetFilenamePrefix$idx%04d.fits")
      es  <- stages.traverse { stage => addDatasetEventAs(user, did, stage) }
    yield DatasetNode(did, es)

  def recordStep(
    setup:   Setup,
    user:    User,
    aid:     Atom.Id,
    vid:     Visit.Id,
    atom:    Int,
    step:    Int,
    ids:     ListMap[Atom.Id, List[Step.Id]]
  ): IO[StepNode] =

    val stages0 = List(
      StepStage.StartStep,
      StepStage.StartConfigure,
      StepStage.EndConfigure,
      StepStage.StartObserve
    )

    val stages1 = List(
      StepStage.EndObserve,
      StepStage.EndStep
    )

    val sid = ids(aid)(step)
    for
      es0 <- stages0.traverse { stage => addStepEventAs(user, sid, vid, stage) }
      ds  <- (0 until setup.datasetCount).toList.traverse { d => recordDataset(setup, user, sid, vid, atom, step, d) }
      es1 <- stages1.traverse { stage => addStepEventAs(user, sid, vid, stage) }
    yield StepNode(sid, ds, es0 ::: es1)

  def recordAtom(
    setup: Setup,
    user:  User,
    vid:   Visit.Id,
    atom:  Int,
    ids:   ListMap[Atom.Id, List[Step.Id]]
  ): IO[AtomNode] =
    val aid = ids.toList(atom)._1
    (0 until setup.stepCount)
      .toList
      .traverse: s =>
        recordStep(setup, user, aid, vid, atom, s, ids)
      .map(steps => AtomNode(aid, steps))

  def recordVisit(
    mode:  ObservingModeType,
    setup: Setup,
    user:  User,
    oid:   Observation.Id
  ): IO[VisitNode] =
    for
      vid <- recordVisitAs(user, mode.instrument, oid)
      ids <- scienceSequenceIds(user, oid)
      e0  <- addSequenceEventAs(user, vid, SequenceCommand.Start)
      as  <- (0 until setup.atomCount).toList.traverse { a => recordAtom(setup, user, vid, a, ids) }
      e1  <- addSequenceEventAs(user, vid, SequenceCommand.Stop)
    yield VisitNode(vid, as, List(e0, e1))

  def recordAll(
    user: User,
    serviceUser: User, // user who actually creates the observing events
    mode: ObservingModeType,
    offset: Int       = 0,
    atomCount: Int    = 1,
    stepCount: Int    = 1,
    datasetCount: Int = 1
  ): IO[ObservationNode] =
    val setup = Setup(offset, atomCount, stepCount, datasetCount)
    for
      pid <- createProgramAs(user)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createObservationAs(user, pid, mode.some, tid)
      v   <- recordVisit(mode, setup, serviceUser, oid)
    yield ObservationNode(oid, v)

  def setQaState(
    user:    User,
    qaState: DatasetQaState,
    like:    String
  ): IO[Unit] =

    val q = s"""
      mutation {
        updateDatasets(input: {
          SET: {
            qaState: ${qaState.tag.toScreamingSnakeCase}
          },
          WHERE: {
            filename: {
              LIKE: "$like"
            }
          }
        }) {
          datasets {
            id
          }
        }
      }
    """

    query(user, q).void

}

object ExecutionQuerySetupOperations:

  trait Node:
    def allDatasets: List[Dataset.Id]
    def allEvents: List[ExecutionEvent]

  case class DatasetNode(id: Dataset.Id, events: List[ExecutionEvent]) extends Node:
    def allDatasets = List(id)
    def allEvents   = events

  case class StepNode(id: Step.Id, datasets: List[DatasetNode], events: List[ExecutionEvent]) extends Node:
    def allDatasets = datasets.map(_.id).sorted
    def allEvents   = (datasets.flatMap(_.allEvents) ::: events).sortBy(_.id)

  case class AtomNode(id: Atom.Id, steps: List[StepNode]) extends Node:
    def allDatasets = steps.flatMap(_.allDatasets).sorted
    def allEvents   = steps.flatMap(_.allEvents).sortBy(_.id)

  case class VisitNode(id: Visit.Id, atoms: List[AtomNode], events: List[ExecutionEvent]) extends Node:
    def allDatasets = atoms.flatMap(_.allDatasets).sorted
    def allEvents   = (atoms.flatMap(_.allEvents) ::: events).sortBy(_.id)

  case class ObservationNode(id: Observation.Id, visit: VisitNode) extends Node:
    def allDatasets = visit.allDatasets.sorted
    def allEvents   = visit.allEvents.sortBy(_.id)

  case class Setup(
    offset: Int,
    atomCount: Int,
    stepCount: Int,
    datasetCount: Int
  )