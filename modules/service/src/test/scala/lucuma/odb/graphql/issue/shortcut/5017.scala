// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupport
import lucuma.odb.json.all.transport.given

class ShortCut_5017 extends ExecutionTestSupport:

  def gcalTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Disabled)

  def sciTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Enabled)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      NonNegInt.unsafeFrom(10)
    )

  test("Digest updates after dataset QA"):

    def recordDatasetAndEvents(c: AtomicCell[IO, Int], s: Step.Id): IO[Dataset.Id] =
      val stages = List(
        DatasetStage.StartExpose,  DatasetStage.EndExpose,
        DatasetStage.StartReadout, DatasetStage.EndReadout,
        DatasetStage.StartWrite,   DatasetStage.EndWrite
      )

      for
        i <- c.updateAndGet(_ + 1)
        d <- recordDatasetAs(serviceUser, s, f"N20250313S$i%04d.fits")
        _ <- stages.traverse_(addDatasetEventAs(serviceUser, d, _).void)
      yield d

    def executeStep(c: AtomicCell[IO, Int], s: Step.Id): IO[Dataset.Id] =
      for
        _ <- addStepEventAs(serviceUser, s, StepStage.StartStep)
        _ <- addStepEventAs(serviceUser, s, StepStage.StartConfigure)
        _ <- addStepEventAs(serviceUser, s, StepStage.EndConfigure)
        _ <- addStepEventAs(serviceUser, s, StepStage.StartObserve)
        d <- recordDatasetAndEvents(c, s)
        _ <- addStepEventAs(serviceUser, s, StepStage.EndObserve)
        _ <- addStepEventAs(serviceUser, s, StepStage.EndStep)
      yield d

    // Execute an arc, a flat and one science step
    def recordAndExecuteScienceStep(c: AtomicCell[IO, Int], a: Atom.Id, ditherNm: Int, q: Int): IO[(Step.Id, Dataset.Id)] =
      for
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(ditherNm), StepConfig.Science, sciTelescopeConfig(q), ObserveClass.Science)
        d <- executeStep(c, s)
      yield (s, d)

    def executeAtom(c: AtomicCell[IO, Int], v: Visit.Id, ditherNm: Int, q: Int, stepCount: Int): IO[(Atom.Id, List[(Step.Id, Dataset.Id)])] =
      for
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        sa <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(ditherNm), ArcStep, gcalTelescopeConfig(q), ObserveClass.NightCal)
        da <- executeStep(c, sa)
        sf <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(ditherNm), FlatStep, gcalTelescopeConfig(q), ObserveClass.NightCal)
        df <- executeStep(c, sf)
        ss <- recordAndExecuteScienceStep(c, a, ditherNm, q).replicateA(stepCount)
      yield (a, (sa, da) :: (sf, df) :: ss)

    val setup: IO[(Observation.Id, (Atom.Id, List[(Step.Id, Dataset.Id)]))] =
      for
        p <- createProgram
        _ <- setProgramReference(staff, p, """engineering: { semester: "2025B", instrument: GMOS_NORTH }""")
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        c <- AtomicCell[IO].of(0)
        _ <- executeAtom(c, v,  0,   0, 3).void
        _ <- executeAtom(c, v,  5,  15, 3).void
        _ <- executeAtom(c, v, -5, -15, 3).void
        a <- executeAtom(c, v,  0,   0, 1)
      yield (o, a)

    def currentStateQuery(o: Observation.Id): String = s"""
      query {
        observation(observationId: "$o") {
          execution {
            digest {
              science {
                timeEstimate {
                  total { microseconds }
                }
              }
            }
          }
          workflow { state }
        }
      }
    """

    def cacheItc(o: Observation.Id): IO[Unit] =
      query(
        user  = pi,
        query = s"""
          query {
            observation(observationId: "$o") {
              itc {
                science {
                  selected {
                    exposureTime { seconds }
                  }
                }
              }
            }
          }
        """
      ).void

    // workflow state and remaining time (microseconds)
    def currentState(o: Observation.Id): IO[(ObservationWorkflowState, Long)] =
      query(
        user = pi,
        query = currentStateQuery(o)
      ).flatMap: json =>
        val c = json.hcursor.downField("observation")
        (for
          s <- c.downFields("workflow", "state").as[ObservationWorkflowState]
          t <- c.downFields("execution", "digest", "science", "timeEstimate", "total", "microseconds").as[Long]
        yield (s, t)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

    val result =
      for
        // Execute the observation completely and ask for the current state
        init      <- setup
        obs        = init._1
        _         <- cacheItc(obs) // required for workflow computation
        completed <- currentState(obs)

        // Now mark a dataset failed and ask for the state again
        dataset    = init._2._2.map(_._2).last  // last science dataset
        _         <- setQaState(dataset, DatasetQaState.Fail)
        ongoing   <- currentState(obs)
      yield (completed, ongoing)

    assertIOBoolean(
      result.map: (completed, ongoing) =>
        completed._1 === ObservationWorkflowState.Completed &&
        ongoing._1   === ObservationWorkflowState.Ongoing   &&
        completed._2 === 0L  /* no time remaining */        &&
        ongoing._2   >   0L  // some time remaining
    )