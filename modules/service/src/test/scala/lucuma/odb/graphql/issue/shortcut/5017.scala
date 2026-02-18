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
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.json.all.transport.given

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

    // Execute an arc, a flat and one science step
    def recordAndExecuteScienceStep(c: AtomicCell[IO, Int], a: Atom.Id, v: Visit.Id, ditherNm: Int, q: Int): IO[(Step.Id, Dataset.Id)] =
      for
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(ditherNm), StepConfig.Science, sciTelescopeConfig(q), ObserveClass.Science)
        d <- executeStep(c, s, v)
      yield (s, d)

    def executeAtom(c: AtomicCell[IO, Int], v: Visit.Id, ditherNm: Int, q0: Int, qs: Int*): IO[(Atom.Id, List[(Step.Id, Dataset.Id)])] =
      for
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        sa <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(ditherNm), ArcStep, gcalTelescopeConfig(q0), ObserveClass.NightCal)
        da <- executeStep(c, sa, v)
        sf <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(ditherNm), FlatStep, gcalTelescopeConfig(q0), ObserveClass.NightCal)
        df <- executeStep(c, sf, v)
        ss <- (q0 :: qs.toList).traverse(q => recordAndExecuteScienceStep(c, a, v, ditherNm, q))
      yield (a, (sa, da) :: (sf, df) :: ss)

    val setup: IO[(Program.Id, Observation.Id, (Atom.Id, List[(Step.Id, Dataset.Id)]))] =
      for
        p <- createProgram
        _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_NORTH }""")
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        c <- AtomicCell[IO].of(0)
        _ <- executeAtom(c, v,  0,  0, 15, -15).void
        _ <- executeAtom(c, v,  5,  0, 15, -15).void
        _ <- executeAtom(c, v, -5,  0, 15, -15).void
        a <- executeAtom(c, v,  0,  0)
      yield (p, o, a)

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
        init      <- setup
        prog       = init._1
        obs        = init._2
        completed <- currentState(prog, obs)

        // Now mark a dataset failed and ask for the state again
        dataset    = init._3._2.map(_._2).last  // last science dataset
        _         <- setQaState(dataset, DatasetQaState.Fail)
        ongoing   <- currentState(prog, obs)
      yield (completed, ongoing)

    assertIOBoolean(
      result.map: (completed, ongoing) =>
        completed._1 === ObservationWorkflowState.Completed &&
        ongoing._1   === ObservationWorkflowState.Ongoing   &&
        completed._2 === 0L  /* no time remaining */        &&
        ongoing._2   >   0L  // some time remaining
    )
