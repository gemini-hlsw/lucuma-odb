// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given
import lucuma.odb.sequence.gmos.longslit.Science
import lucuma.odb.util.Codecs.atom_id
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.step_id
import skunk.*
import skunk.implicits.*

class executionSciGmosNorth extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  extension (Δλnm: Int)
    def description: NonEmptyString =
      NonEmptyString.unsafeFrom(s"${Δλnm}.000 nm")

  test("simple generation - limited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("simple generation with blind offset target uses regular targets for science sequence"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        bt <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithBlindOffset(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid, 1.some),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" -> gmosNorthExpectedScienceAtom(ditherNm = 0, 0, 15, -15),
                  "possibleFuture" -> List(gmosNorthExpectedScienceAtom(ditherNm = 5, 0, 15, -15)).asJson,
                  "hasMore" -> true.asJson
                )
              )
            )
          ).asRight
      )

  test("simple generation - unlimited future"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" ->
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0, 15, -15),
                  "possibleFuture" -> List(
                    gmosNorthExpectedScienceAtom(ditherNm =  5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0)
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  private val ExpectedAfterCalsAndOneScience: Json =
    Json.obj(
      "executionConfig" -> Json.obj(
        "gmosNorth" -> Json.obj(
          "science" -> Json.obj(
            "nextAtom" ->
              gmosNorthExpectedScienceAtom(
                ditherNm = 0,
                List(15, -15).map(q => gmosNorthExpectedScience(0, 0, q))
              ),
            "possibleFuture" -> List(
              gmosNorthExpectedScienceAtom(ditherNm =  5, 0, 15, -15),
              gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
              gmosNorthExpectedScienceAtom(ditherNm =  0, 0)
            ).asJson,
            "hasMore" -> false.asJson
          )
        )
      )
    )

  test("execute arc, flat, one science"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = ExpectedAfterCalsAndOneScience.asRight
      )

  test("arc, flat, <1.5 hours>"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        ic <- generateAfterOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      // We did no science from 0nm but we're out of time.  The next
      // atom should be for a 5nm block.
      assertEquals(gn.nextAtom.description.get, 5.description)

      // The last two atoms will be (0nm, 0").

      // The next to last atom should be full.
      val penultimateAtom = gn.possibleFuture.init.last
      assertEquals(penultimateAtom.description.get, 0.description)

      val penultimateCounts = penultimateAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(penultimateCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(penultimateCounts.get(StepType.Science), 3.some)

      // The last atom will have one left.
      val ultimateAtom    = gn.possibleFuture.last
      assertEquals(ultimateAtom.description.get, 0.description)

      val ultimateCounts = ultimateAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(ultimateCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(ultimateCounts.get(StepType.Science), 1.some)

  test("arc, flat, science, <1.5 hours>"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        ic <- generateAfterOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      // We only did one step of (0nm, 0") but we're out of time.  The next
      // atom should be for a 5nm block.
      assertEquals(gn.nextAtom.description.get, 5.description)

      // The last atom will be 0nm
      val lastAtom = gn.possibleFuture.last
      assertEquals(lastAtom.description.get, 0.description)

      // and it now has 3 science steps left
      val counts = lastAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(counts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(counts.get(StepType.Science), 3.some)

  // Adjust the timestamp of atom records precisely
  def adjustAtomTime(a: Atom.Id, t: Timestamp): IO[Unit] =
    val query: Command[(Atom.Id, Timestamp)] =
      sql"""
        UPDATE t_atom_record
           SET c_created = $core_timestamp
         WHERE c_atom_id = $atom_id
      """.command.contramap { (a, t) => (t, a) }

    withSession: session =>
      session.execute(query)(a, t).void

  // Adjust the timestamp of step records precisely
  def adjustStepTime(s: Step.Id, t: Timestamp): IO[Unit] =
    val query: Command[(Step.Id, Timestamp)] =
      sql"""
        UPDATE t_step_record
           SET c_created = $core_timestamp
         WHERE c_step_id = $step_id
      """.command.contramap { (s, t) => (t, s) }

    withSession: session =>
      session.execute(query)(s, t).void

  test("arc, flat, <1.5 hours>, science"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        nw <- timestampNow

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        _  <- adjustAtomTime(a, nw)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        _  <- adjustStepTime(s0, nw.plusMicrosOption(1).get)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        _  <- adjustStepTime(s1, nw.plusMicrosOption(2).get)

        longDelay = Science.CalValidityPeriod +| 2.microsecondTimeSpan
        timestamp = nw.plusMicrosOption(longDelay.toMicroseconds).get

        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        _  <- adjustStepTime(s2, timestamp)

        ic <- generateOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      // We only did one step of 0nm but we're out of time.  There is a
      // science dataset that doesn't have valid calibrations, but could be
      // salvaged if we repeated calibrations.
      assertEquals(gn.nextAtom.description.get, 0.description)
      val nextCounts = gn.nextAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(nextCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(nextCounts.get(StepType.Science), none) // no time for more sci

      // The last atom will also be 0nm
      val lastAtom = gn.possibleFuture.last
      assertEquals(lastAtom.description.get, 0.description)

      // and it now has 3 science steps left
      val lastCounts = lastAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(lastCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(lastCounts.get(StepType.Science), 3.some)

  // About how long it takes to move the science fold into position
  val ScienceFoldTime: TimeSpan = 15.secondTimeSpan

  // Rough flat or arc time estimate
  val CalTime: TimeSpan = 60.secondTimeSpan

  // About how long it takes to do a science dataset w/ 20 min exposure time
  val ScienceTime: TimeSpan = 1300.secondTimeSpan

  // leave time for just one science
  test("arc, flat, <1.5 hours - 1*science>"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        nw <- timestampNow

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        _  <- adjustAtomTime(a, nw)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        _  <- adjustStepTime(s0, nw.plusMicrosOption(1).get)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        _  <- adjustStepTime(s1, nw.plusMicrosOption(2).get)

        longDelay = Science.CalValidityPeriod -| ScienceFoldTime -| ScienceTime
        timestamp = nw.plusMicrosOption(longDelay.toMicroseconds)

        ic <- generateOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      // There's only time left for one science step
      assertEquals(gn.nextAtom.description.get, 0.description)
      val nextCounts = gn.nextAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(nextCounts.get(StepType.Gcal), none) // cals still valid
      assertEquals(nextCounts.get(StepType.Science), 1.some)

      // The last atom will also be (0nm, 0")
      val lastAtom = gn.possibleFuture.last
      assertEquals(lastAtom.description.get, 0.description)

      // and it now has 3 science steps left
      val lastCounts = lastAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(lastCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(lastCounts.get(StepType.Science), 3.some)

  // leave time for two science
  test("arc, flat, <1.5 hours - 2*science>"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        nw <- timestampNow

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        _  <- adjustAtomTime(a, nw)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        _  <- adjustStepTime(s0, nw.plusMicrosOption(1).get)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        _  <- adjustStepTime(s1, nw.plusMicrosOption(2).get)

        longDelay = Science.CalValidityPeriod -| ScienceFoldTime -| ScienceTime -| ScienceTime
        timestamp = nw.plusMicrosOption(longDelay.toMicroseconds)

        ic <- generateOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      // There's only time left for two science steps
      assertEquals(gn.nextAtom.description.get, 0.description)
      val nextCounts = gn.nextAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(nextCounts.get(StepType.Gcal), none) // cals still valid
      assertEquals(nextCounts.get(StepType.Science), 2.some)

      // The last atom will also be 0nm
      val lastAtom = gn.possibleFuture.last
      assertEquals(lastAtom.description.get, 0.description)

      // and it now has 2 science steps left
      val lastCounts = lastAtom.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)
      assertEquals(lastCounts.get(StepType.Gcal), 2.some) // arc + flat
      assertEquals(lastCounts.get(StepType.Science), 2.some)

  test("we can start anywhere"):
    val setup: IO[InstrumentExecutionConfig] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        // We'll add steps for the second block 5 nm
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)

        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(5), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        ic <- generateOrFail(o)
      yield ic

    import lucuma.odb.testsyntax.execution.*

    setup.map(_.gmosNorthScience).map: gn =>
      assertEquals(
        gn.nextAtom.description.get :: gn.possibleFuture.map(_.description.get),
        List(
           5.description,
          -5.description,
           0.description,
           0.description
        )
      )

      def scienceCount(a: Atom[DynamicConfig.GmosNorth]): Int =
        (a.steps.groupMapReduce(_.stepConfig.stepType)(_ => 1)).get(StepType.Science).get

      assertEquals(
        scienceCount(gn.nextAtom) :: gn.possibleFuture.map(scienceCount),
        List(2, 3, 3, 1)
      )

  test("order doesn't matter"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s2)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = ExpectedAfterCalsAndOneScience.asRight
      )

  test("irrelevant steps may be inserted"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        x0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(8), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(x0)

        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s1)

        x1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(8), StepConfig.Science, sciTelescopeConfig(10), ObserveClass.Science)
        _  <- addEndStepEvent(x1)

        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s2)

        x2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(8), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(x2)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected = ExpectedAfterCalsAndOneScience.asRight
      )

  test("execute arc, flat, one science, fail the flat"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)
      yield o

    // since the atom is still in progress, finish out the science datasets to
    // avoid moving the science fold.  then add the failed flat

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" ->
                    gmosNorthExpectedScienceAtom(
                      ditherNm = 0,
                      List(
                        gmosNorthExpectedScience(ditherNm = 0, p = 0, q =  15),
                        gmosNorthExpectedScience(ditherNm = 0, p = 0, q = -15),
                        gmosNorthExpectedFlat(ditherNm = 0, p = 0, q = -15)
                      )
                    ),
                  "possibleFuture" -> List(
                    gmosNorthExpectedScienceAtom(ditherNm =  5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0)
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  test("execute the first atom, a step of the second, then fail a science dataset from the first"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)
        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s5)
        s6 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s6)
        s7 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthScience(5), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s7)

        d  <- recordDatasetAs(serviceUser, s2, "N20240905S1001.fits")
        _  <- setQaState(d, DatasetQaState.Usable)
      yield o

    // since the atom is done, redo the failed step at the end
    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthScienceQuery(oid),
        expected =
          Json.obj(
            "executionConfig" -> Json.obj(
              "gmosNorth" -> Json.obj(
                "science" -> Json.obj(
                  "nextAtom" ->
                    gmosNorthExpectedScienceAtom(
                      ditherNm = 5,
                      List(
                        gmosNorthExpectedScience(ditherNm = 5, p = 0, q =  15),
                        gmosNorthExpectedScience(ditherNm = 5, p = 0, q = -15),
                      )
                    ),
                  "possibleFuture" -> List(
                    gmosNorthExpectedScienceAtom(ditherNm = -5, 0, 15, -15),
                    gmosNorthExpectedScienceAtom(ditherNm =  0, 0, 0)  // 1 left over, 1 to make up failure
                  ).asJson,
                  "hasMore" -> false.asJson
                )
              )
            )
          ).asRight
      )

  def nextAtomId(o: Observation.Id): IO[Atom.Id] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(o, 5.some).map(_.gmosNorthScience.nextAtom.id)

  test("nextAtom id doesn't change while executing"):
    val setup: IO[(List[Atom.Id], List[Atom.Id])] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        x0 <- nextAtomId(o)

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        x1 <- nextAtomId(o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)

        x2 <- nextAtomId(o)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        x3 <- nextAtomId(o)

        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        x4 <- nextAtomId(o)

        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        x5 <- nextAtomId(o)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s5)

        x6 <- nextAtomId(o)
      yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map: (firstAtomIds, secondAtomIds) =>
      assertEquals(firstAtomIds.distinct.length, 1, "first")
      assertEquals(secondAtomIds.distinct.length, 1, "second")

  test("explicit offsets"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitSpatialOffsets: [
                { arcseconds: -20.0 },
                { arcseconds:   0.0 },
                { arcseconds:  20.0 }
              ]
            }
          """
        )
        _ <- runObscalcUpdate(p, o)
      yield o

    def telescopeConfig(arcsec: Int): Json =
      json"""
        {
          "offset": {
            "q": {
              "arcseconds": ${Json.fromBigDecimal(BigDecimal(arcsec).setScale(6))}
            }
          }
        }
      """

    def gcalStep(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          },
          "telescopeConfig": ${telescopeConfig(arcsec)}
        }
      """

    def scienceStep(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "SCIENCE"
          },
          "telescopeConfig": ${telescopeConfig(arcsec)}
        }
      """

    def atom(nm: Int, o0: Int, os: Int*): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${gcalStep(o0) :: gcalStep(o0) :: (o0 :: os.toList).map(scienceStep)}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     value {
                       science {
                         telescopeConfigs {
                           offset { q { arcseconds } }
                           guiding
                         }
                       }
                     }
                   }
                 }
               }
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "value": {
                      "science": {
                        "telescopeConfigs": [
                          {
                            "offset": { "q": { "arcseconds": -20.000000 } },
                            "guiding": "ENABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": -20.000000 } },
                            "guiding": "DISABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": 0.000000 } },
                            "guiding": "ENABLED"
                          },
                          {
                            "offset": { "q": { "arcseconds": 20.000000 } },
                            "guiding": "ENABLED"
                          }
                        ]
                      }
                    }
                  }
                }
              },
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": ${atom(0, -20, 0, 20)},
                    "possibleFuture": ${List(
                      atom( 5, -20, 0, 20),
                      atom(-5, -20, 0, 20),
                      atom( 0, -20)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  test("explicit wavelength dithers"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitWavelengthDithers: [
                { nanometers: -7.0 },
                { nanometers:  0.0 },
                { nanometers:  7.0 }
              ]
            }
          """
        )
      yield o

    def step(nm: Int): Json =
      json"""
        {
          "instrumentConfig": {
            "gratingConfig": {
              "wavelength": {
                "nanometers": ${Json.fromBigDecimal(BigDecimal(500 + nm).setScale(3))}
              }
            }
          }
        }
      """

    def atom(nm: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${List.fill(scienceSteps + 2)(step(nm))}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         instrumentConfig {
                           gratingConfig {
                             wavelength { nanometers }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         instrumentConfig {
                           gratingConfig {
                             wavelength { nanometers }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": ${atom(-7, 3)},
                    "possibleFuture": ${List(
                      atom( 0,  3),
                      atom( 7,  3),
                      atom(-7,  1)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  test("select min x-binning"):
    val gaussianProfile = gaussianBandNormalizedProfile(Angle.fromMicroarcseconds(647_200L))
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t0 <- createTargetWithProfileAs(pi, p, gaussianProfile)  // X-binning of 4
        t1 <- createTargetWithProfileAs(pi, p)  // X-binning of 1
        o  <- createObservationWithModeAs(pi, p, List(t0, t1),
               // use a 5" slit so that won't be a factor
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_5_00,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitYBin: TWO
                 }
               """
             )
      yield o

    val step: Json =
      json"""
        {
          "instrumentConfig": {
            "readout": {
              "xBin": "ONE",
              "yBin": "TWO"
            }
          }
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       steps {
                         instrumentConfig {
                           readout {
                             xBin
                             yBin
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": {
                      "steps": ${List.fill(5)(step)}
                    }
                  }
                }
              }
            }
          """.asRight
      )

  test("duplicate offsets and dithers"):

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitSpatialOffsets: [
                { arcseconds: 0.0 },
                { arcseconds: 0.0 },
                { arcseconds: 0.0 }
              ],
              explicitWavelengthDithers: [
                { nanometers: 5.0 },
                { nanometers: 5.0 },
                { nanometers: 5.0 }
              ]
            }
          """
        )
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)

        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(5), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
      yield o

    def telescopeConfigJson(arcsec: Int): Json =
      json"""
        {
          "offset": {
            "q": {
              "arcseconds": ${Json.fromBigDecimal(BigDecimal(arcsec).setScale(6))}
            }
          }
        }
      """

    def gcalStepJson(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "GCAL"
          },
          "telescopeConfig": ${telescopeConfigJson(arcsec)}
        }
      """

    def scienceStepJson(arcsec: Int): Json =
      json"""
        {
          "stepConfig": {
            "stepType": "SCIENCE"
          },
          "telescopeConfig": ${telescopeConfigJson(arcsec)}
        }
      """

    def atom(nm: Int, arcsec: Int, scienceSteps: Int): Json =
      val desc = s"${BigDecimal(nm).setScale(3)} nm"
      json"""
        {
          "description": $desc,
          "steps": ${gcalStepJson(arcsec) :: gcalStepJson(arcsec) :: List.fill(scienceSteps)(scienceStepJson(arcsec))}
        }
      """

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               executionConfig(observationId: "$oid") {
                 gmosNorth {
                   science {
                     nextAtom {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                     possibleFuture {
                       description
                       steps {
                         stepConfig {
                           stepType
                         }
                         telescopeConfig {
                           offset {
                             q { arcseconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "science": {
                    "nextAtom": {
                      "description": ${s"5.000 nm".asJson},
                      "steps": ${List.fill(2)(scienceStepJson(0))}
                    },
                    "possibleFuture": ${List(
                      atom(5, 0, 3),
                      atom(5, 0, 3),
                      atom(5, 0, 1)
                    )}
                  }
                }
              }
            }
          """.asRight
      )

  def firstAcquisitionStepId(o: Observation.Id): IO[Step.Id] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(o, 5.some).map(_.gmosNorthAcquisition.nextAtom.steps.head.id)

  test("acquisition step ids do not change while executing science"):
    val execSci: IO[Set[Step.Id]] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        x0 <- firstAcquisitionStepId(o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        x1 <- firstAcquisitionStepId(o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)

        x2 <- firstAcquisitionStepId(o)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        x3 <- firstAcquisitionStepId(o)

        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        x4 <- firstAcquisitionStepId(o)

        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        x5 <- firstAcquisitionStepId(o)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s5)

        x5 <- firstAcquisitionStepId(o)
      yield Set(x0, x1, x2, x3, x4, x5)

    assertIO(execSci.map(_.size), 1)

  def nextAtomStepIds(o: Observation.Id): IO[NonEmptyList[Step.Id]] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(o, 5.some).map(_.gmosNorthScience.nextAtom.steps.map(_.id))

  test("nextAtom step ids don't change while executing"):
    val setup: IO[(List[NonEmptyList[Step.Id]], List[NonEmptyList[Step.Id]])] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        x0 <- nextAtomStepIds(o)

        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)

        x1 <- nextAtomStepIds(o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)

        x2 <- nextAtomStepIds(o)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s2)

        x3 <- nextAtomStepIds(o)

        s3 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        x4 <- nextAtomStepIds(o)

        s4 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        // Next atom

        x5 <- nextAtomStepIds(o)

        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s5 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(5), ArcStep, gcalTelescopeConfig(0), ObserveClass.NightCal)
        _  <- addEndStepEvent(s5)

        x6 <- nextAtomStepIds(o)

      yield (List(x0, x1, x2, x3, x4), List(x5, x6))

    setup.map: (atom0Ids, atom1Ids) =>
      def checkAtom(atom: String, ids: List[NonEmptyList[Step.Id]]): Unit =
        ids.zip(ids.tail).foreach: (before, after) =>
          assertEquals(before.tail, after.toList, s"atom $atom, before: $before, after: $after")

      checkAtom("Atom 0", atom0Ids)
      checkAtom("Atom 1", atom1Ids)