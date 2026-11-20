// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.StepStage
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.CalculationState
import lucuma.itc.IntegrationTime

class executedSteps extends ExecutionTestSupportForGmos:

  // 10 science steps, plus 4 arcs and 4 flats in the science sequence.
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  val ScienceStepCount: Int = 10

  case class Steps(science: List[Step.Id], calibration: List[Step.Id], acquisition: List[Step.Id])

  // The program, observation and the visit used to execute it, along with the
  // step ids of its (materialized) sequence.
  case class Setup(pid: Program.Id, oid: Observation.Id, vid: Visit.Id, steps: Steps)

  def steps(oid: Observation.Id): IO[Steps] =
    generateGmosNorthOrFail(serviceUser, oid).map: gn =>
      val ec  = gn.executionConfig
      def all(s: Option[ExecutionSequence[?]]) =
        s.toList.flatMap(seq => seq.nextAtom :: seq.possibleFuture).flatMap(_.steps.toList)
      val (sci, cal) = all(ec.science).partition(_.stepConfig.stepType === StepType.Science)
      Steps(sci.map(_.id), cal.map(_.id), all(ec.acquisition).map(_.id))

  val setup: IO[Setup] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, o)
      s <- steps(o)
    yield Setup(p, o, v, s)

  def executedQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            executedSteps {
              calculationState
              value { successful unsuccessful total }
            }
            digest {
              value { science { steps { science { count } } } }
            }
          }
        }
      }
    """

  def result(
    successful:   Int,
    unsuccessful: Int,
    remaining:    Int,
    state:        CalculationState
  ): Json =
    json"""
      {
        "observation": {
          "execution": {
            "executedSteps": {
              "calculationState": ${state.tag.toScreamingSnakeCase},
              "value": {
                "successful": $successful,
                "unsuccessful": $unsuccessful,
                "total": ${successful + unsuccessful}
              }
            },
            "digest": {
              "value": { "science": { "steps": { "science": { "count": $remaining } } } }
            }
          }
        }
      }
    """

  // Runs obscalc, then checks the executed counts and that executed plus
  // remaining still covers every science step.
  def expectExecuted(s: Setup, successful: Int, unsuccessful: Int): IO[Unit] =
    runObscalcUpdate(s.pid, s.oid) *>
      expect(
        user     = pi,
        query    = executedQuery(s.oid),
        expected = result(successful, unsuccessful, ScienceStepCount - successful - unsuccessful, CalculationState.Ready).asRight
      )

  def run(s: Setup, sid: Step.Id, stages: StepStage*): IO[Unit] =
    stages.toList.traverse(stage => addStepEventAs(serviceUser, sid, s.vid, stage)).void

  def complete(s: Setup, sid: Step.Id): IO[Unit] =
    run(s, sid, StepStage.StartStep, StepStage.EndStep)

  // A dataset for the step, then the step completes.
  def completeWithDataset(s: Setup, sid: Step.Id, file: String): IO[Dataset.Id] =
    for
      _ <- run(s, sid, StepStage.StartStep)
      d <- recordDatasetAs(serviceUser, sid, s.vid, file)
      _ <- run(s, sid, StepStage.EndStep)
    yield d

  test("nothing executed"):
    setup.flatMap: s =>
      expectExecuted(s, 0, 0)

  test("pending until obscalc runs"):
    for
      s <- setup
      _ <- runObscalcUpdate(s.pid, s.oid)
      _ <- complete(s, s.steps.science.head)
      _ <- expect(
             user     = pi,
             query    = executedQuery(s.oid),
             expected = result(0, 0, ScienceStepCount, CalculationState.Pending).asRight
           )
    yield ()

  test("completed science steps are successful"):
    for
      s <- setup
      _ <- s.steps.science.take(3).traverse(complete(s, _))
      _ <- expectExecuted(s, 3, 0)
    yield ()

  test("calibration and acquisition steps are not counted"):
    for
      s <- setup
      _ <- s.steps.acquisition.traverse(complete(s, _))
      _ <- s.steps.calibration.take(2).traverse(complete(s, _))
      _ <- expectExecuted(s, 0, 0)
    yield ()

  test("aborted steps are unsuccessful"):
    for
      s <- setup
      _ <- complete(s, s.steps.science.head)
      _ <- run(s, s.steps.science(1), StepStage.StartStep, StepStage.Abort)
      _ <- expectExecuted(s, 1, 1)
    yield ()

  test("stopped steps follow their QA state"):
    for
      s <- setup
      _ <- run(s, s.steps.science.head, StepStage.StartStep)
      d <- recordDatasetAs(serviceUser, s.steps.science.head, s.vid, "N20261120S0101.fits")
      _ <- run(s, s.steps.science.head, StepStage.Stop)
      _ <- expectExecuted(s, 1, 0)
      _ <- setQaState(d, DatasetQaState.Fail)
      _ <- expectExecuted(s, 0, 1)
    yield ()

  test("completed steps follow their QA state"):
    for
      s <- setup
      d <- completeWithDataset(s, s.steps.science.head, "N20261120S0201.fits")
      _ <- expectExecuted(s, 1, 0)  // not yet assessed
      _ <- setQaState(d, DatasetQaState.Pass)
      _ <- expectExecuted(s, 1, 0)
      _ <- setQaState(d, DatasetQaState.Usable)
      _ <- expectExecuted(s, 0, 1)
      _ <- setQaState(d, DatasetQaState.Fail)
      _ <- expectExecuted(s, 0, 1)
      _ <- setQaState(d, DatasetQaState.Pass)
      _ <- expectExecuted(s, 1, 0)
    yield ()

  test("a step left ongoing by a new visit is abandoned, so unsuccessful"):
    for
      s <- setup
      _ <- run(s, s.steps.science.head, StepStage.StartStep)
      _ <- recordVisitAs(serviceUser, s.oid)
      _ <- expectExecuted(s, 0, 1)
    yield ()

  test("a step abandoned by another observation's visit is unsuccessful once its mark is swept"):
    for
      s  <- setup
      t  <- createTargetWithProfileAs(pi, s.pid)
      o2 <- createGmosNorthLongSlitObservationAs(pi, s.pid, List(t))
      _  <- run(s, s.steps.science.head, StepStage.StartStep)
      _  <- expectExecuted(s, 0, 0)  // the ongoing step is still remaining
      _  <- recordVisitAs(serviceUser, o2)
      _  <- invalidateMarkedAs(serviceUser)
      _  <- expect(
              user     = pi,
              query    = executedQuery(s.oid),
              expected = result(0, 0, ScienceStepCount, CalculationState.Pending).asRight
            )
      _  <- expectExecuted(s, 0, 1)
    yield ()

  test("an abort after a calculation updates both counts"):
    for
      s <- setup
      _ <- complete(s, s.steps.science.head)
      _ <- expectExecuted(s, 1, 0)
      _ <- run(s, s.steps.science(1), StepStage.StartStep)
      _ <- expectExecuted(s, 1, 0)
      _ <- run(s, s.steps.science(1), StepStage.Abort)
      _ <- expectExecuted(s, 1, 1)
    yield ()

