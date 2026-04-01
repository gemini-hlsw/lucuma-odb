// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_5017 extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  test("Digest updates after dataset QA"):

    def recordDatasetAndEvents(c: AtomicCell[IO, Int], s: Step.Id, v: Visit.Id): IO[Dataset.Id] =
      val stages = List(
        DatasetStage.StartExpose,  DatasetStage.EndExpose,
        DatasetStage.StartReadout, DatasetStage.EndReadout,
        DatasetStage.StartWrite,   DatasetStage.EndWrite
      )

      for
        i <- c.updateAndGet(_ + 1)
        d <- recordDatasetAs(serviceUser, s, v, f"N20250313S$i%04d.fits")
        _ <- stages.traverse_(addDatasetEventAs(serviceUser, d, _).void)
      yield d

    def executeStep(c: AtomicCell[IO, Int], s: Step.Id, v: Visit.Id): IO[Dataset.Id] =
      for
        _ <- addStepEventAs(serviceUser, s, v, StepStage.StartStep)
        _ <- addStepEventAs(serviceUser, s, v, StepStage.StartConfigure)
        _ <- addStepEventAs(serviceUser, s, v, StepStage.EndConfigure)
        _ <- addStepEventAs(serviceUser, s, v, StepStage.StartObserve)
        d <- recordDatasetAndEvents(c, s, v)
        _ <- addStepEventAs(serviceUser, s, v, StepStage.EndObserve)
        _ <- addStepEventAs(serviceUser, s, v, StepStage.EndStep)
      yield d

    def executeAtom(c: AtomicCell[IO, Int], ids: (Atom.Id, List[Step.Id]), v: Visit.Id): IO[(Atom.Id, List[(Step.Id, Dataset.Id)])] =
      ids._2.traverse(sid => executeStep(c, sid, v).tupleLeft(sid)).tupleLeft(ids._1)

    val setup: IO[(Program.Id, Observation.Id, (Atom.Id, List[(Step.Id, Dataset.Id)]))] =
      for
        p <- createProgram
        _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_NORTH }""")
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        a <- scienceSequenceIds(serviceUser, o).map(_.toList)
        v <- recordVisitAs(serviceUser, o)
        c <- AtomicCell[IO].of(0)
        _ <- executeAtom(c, a(0), v)
        _ <- executeAtom(c, a(1), v)
        _ <- executeAtom(c, a(2), v)
        r <- executeAtom(c, a(3), v)
      yield (p, o, r)

    def currentStateQuery(o: Observation.Id): String = s"""
      query {
        observation(observationId: "$o") {
          execution {
            digest {
              value {
                science {
                  timeEstimate {
                    total { microseconds }
                  }
                }
              }
            }
          }
          workflow { value { state } }
        }
      }
    """

    // workflow state and remaining time (microseconds)
    def currentState(p: Program.Id, o: Observation.Id): IO[(ObservationWorkflowState, Long)] =
      runObscalcUpdate(p, o) *>
      query(
        user  = pi,
        query = currentStateQuery(o)
      ).flatMap: json =>
        val c = json.hcursor.downField("observation")
        (for
          s <- c.downFields("workflow", "value", "state").as[ObservationWorkflowState]
          t <- c.downFields("execution", "digest", "value", "science", "timeEstimate", "total", "microseconds").as[Long]
        yield (s, t)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

    val result =
      for
        // Execute the observation completely and ask for the current state
        init   <- setup
        prog    = init._1
        obs     = init._2
        state0 <- currentState(prog, obs)

        // Now mark a dataset failed and ask for the state again
        dataset = init._3._2.map(_._2).last  // last science dataset
        _      <- setQaState(dataset, DatasetQaState.Fail)
        state1 <- currentState(prog, obs)
      yield (state0, state1)

    assertIOBoolean(
      result.map: (state0, state1) =>
        state0._1 === ObservationWorkflowState.Completed &&
// No longer ongoing automatically -- requires a manual edit
//        state1._1 === ObservationWorkflowState.Ongoing   &&
        state1._1 === ObservationWorkflowState.Completed &&
        state0._2 === 0L  /* no time remaining */        &&
// No longer has pending work automatically -- requires a manual edit
//        state1._2   >   0L  // some time remaining
        state1._2 === 0L  /* no time remaining */
    )
