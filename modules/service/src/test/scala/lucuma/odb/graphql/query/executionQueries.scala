// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.odb.data.ObservingModeType

class executionQueries extends OdbSuite with DatabaseOperations {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val service = TestUsers.service(3)

  val mode    = ObservingModeType.GmosNorthLongSlit

  val validUsers = List(pi, pi2, service).toList

  trait Node {
    def allEvents: List[ExecutionEvent.Id]
  }

  case class DatasetNode(id: Dataset.Id, events: List[ExecutionEvent.Id]) extends Node {
    def allEvents: List[ExecutionEvent.Id] = events
  }
  case class StepNode(id: Step.Id, datasets: List[DatasetNode], events: List[ExecutionEvent.Id]) extends Node{
    def allEvents: List[ExecutionEvent.Id] = (datasets.flatMap(_.allEvents) ::: events).sorted
  }
  case class AtomNode(id: Atom.Id, steps: List[StepNode]) extends Node {
    def allEvents: List[ExecutionEvent.Id] = steps.flatMap(_.allEvents).sorted
  }
  case class VisitNode(id: Visit.Id, atoms: List[AtomNode], events: List[ExecutionEvent.Id]) extends Node {
    def allEvents: List[ExecutionEvent.Id] = (atoms.flatMap(_.allEvents) ::: events).sorted
  }
  case class ObservationNode(id: Observation.Id, visits: List[VisitNode]) extends Node {
    def allEvents: List[ExecutionEvent.Id] = visits.flatMap(_.allEvents).sorted
  }

  case class Setup(
    offset: Int       = 0,
    visitCount: Int   = 1,
    atomCount: Int    = 1,
    stepCount: Int    = 1,
    datasetCount: Int = 1
  ) {
    def nextOffset: Int =
      visitCount * atomCount * stepCount * datasetCount
  }

  def recordDataset(
    setup:   Setup,
    user:    User,
    sid:     Step.Id,
    visit:   Int,
    atom:    Int,
    step:    Int,
    dataset: Int
  ): IO[DatasetNode] = {

    val idx = setup.offset                                                     +
              visit * (setup.atomCount * setup.stepCount * setup.datasetCount) +
               atom * (setup.stepCount * setup.datasetCount)                   +
               step * setup.datasetCount                                       +
               dataset                                                         +
               1

    val stages = List(
      DatasetStage.StartObserve,
      DatasetStage.EndObserve,
      DatasetStage.StartReadout,
      DatasetStage.EndReadout,
      DatasetStage.StartWrite,
      DatasetStage.EndWrite
    )

    for {
      did <- recordDatasetAs(user, sid, f"N18630101S$idx%04d.fits")
      es  <- stages.traverse { stage => addDatasetEventAs(user, did, stage) }
    } yield DatasetNode(did, es)
  }

  def recordStep(
    mode:    ObservingModeType,
    setup:   Setup,
    user:    User,
    aid:     Atom.Id,
    visit:   Int,
    atom:    Int,
    step:    Int
  ): IO[StepNode] = {

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
    for {
      sid <- recordStepAs(user, mode.instrument, aid)
      es0 <- stages0.traverse { stage => addStepEventAs(user, sid, stage) }
      ds  <- (0 until setup.datasetCount).toList.traverse { d => recordDataset(setup, user, sid, visit, atom, step, d) }
      es1 <- stages1.traverse { stage => addStepEventAs(user, sid, stage) }
    } yield StepNode(sid, ds, es0 ::: es1)
  }

  def recordAtom(
    mode:    ObservingModeType,
    setup:   Setup,
    user:    User,
    vid:     Visit.Id,
    visit:   Int,
    atom:    Int
  ): IO[AtomNode] =
    for {
      aid <- recordAtomAs(user, mode.instrument, vid)
      ss  <- (0 until setup.stepCount).toList.traverse { s => recordStep(mode, setup, user, aid, visit, atom, s) }
    } yield AtomNode(aid, ss)

  def recordVisit(
    mode:    ObservingModeType,
    setup:   Setup,
    user:    User,
    oid:     Observation.Id,
    visit:   Int
  ): IO[VisitNode] =
    for {
      vid <- recordVisitAs(user, mode.instrument, oid)
      e0  <- addSequenceEventAs(user, vid, SequenceCommand.Start)
      as  <- (0 until setup.atomCount).toList.traverse { a => recordAtom(mode, setup, user, vid, visit, a) }
      e1  <- addSequenceEventAs(user, vid, SequenceCommand.Stop)
    } yield VisitNode(vid, as, List(e0, e1))

  def recordAll(
    mode:  ObservingModeType,
    setup: Setup,
    user:  User
  ): IO[ObservationNode] =
    for {
      pid   <- createProgramAs(user)
      oid   <- createObservationAs(user, pid, mode.some)
      vs    <- (0 until setup.visitCount).toList.traverse { v => recordVisit(mode, setup, user, oid, v) }
    } yield ObservationNode(oid, vs)

  test("`observation` -> `execution` -> `visits`") {
    recordAll(mode, Setup(offset = 0, visitCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              visits() {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val visits = on.visits.map(_.id).map(id => Json.obj("id" -> id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "visits": {
              "matches": $visits
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }

  test("`observation` -> `execution` -> `events`") {
    recordAll(mode, Setup(offset = 2, visitCount = 2, atomCount = 2, stepCount = 3, datasetCount = 2), pi).flatMap { on =>
      val q = s"""
        query {
          observation(observationId: "${on.id}") {
            execution {
              events() {
                matches {
                  id
                }
              }
            }
          }
        }
      """

      val events = on.allEvents.map(id => Json.obj("id" -> id.asJson))

      val e = json"""
      {
        "observation": {
          "execution": {
            "events": {
              "matches": $events
            }
          }
        }
      }
      """.asRight

      expect(pi, q, e)
    }
  }
}
