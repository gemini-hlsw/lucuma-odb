// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.json.gmos.given
import lucuma.odb.json.time.transport.given
import lucuma.odb.json.wavelength.transport.given

class ShortCut_6589 extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      1.secTimeSpan,
      PosInt.unsafeFrom(4)
    )

  case class Setup(
    p: Program.Id,
    o: Observation.Id,
    v: Visit.Id,
    g: Option[Atom.Id],
    a: Atom.Id,
    d: Dataset.Id
  )

  object Setup:

    def init(i: Int): IO[Setup] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createObservationWithModeAs(pi, p, List(t),
               s"""
                 gmosNorthLongSlit: {
                   grating: R831_G5302
                   filter: R_PRIME
                   fpu: LONG_SLIT_0_50
                   centralWavelength: {
                     nanometers: 500
                   }
                   explicitYBin: TWO
                   explicitWavelengthDithers: [
                     { nanometers: 45 },
                     { nanometers: 35 }
                   ]
                   explicitSpatialOffsets: [
                     { arcseconds:  15.0 },
                     { arcseconds: -15.0 }
                   ]
                 }
               """
             )
        g  <- nextAtomId(o)
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(45), ArcStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(45), FlatStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(45), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        s3 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(45), StepConfig.Science, sciTelescopeConfig(-15), ObserveClass.Science)
        _  <- addEndStepEvent(s3)

        d  <- recordDatasetAs(serviceUser, s2, f"N20240905S1$i%03d.fits")
      yield Setup(p, o, v, g, a, d)

  def nextAtomId(o: Observation.Id): IO[Option[Atom.Id]] =
    query(
      pi,
      s"""
        query {
          executionConfig(observationId: "$o") {
            gmosNorth {
              science {
                nextAtom {
                  id
                }
              }
            }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("executionConfig", "gmosNorth", "science", "nextAtom", "id").as[Option[Atom.Id]]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  test("A new atom is created for the second dither block"):
    Setup
      .init(10)
      .flatMap: s =>
        assertIOBoolean(nextAtomId(s.o).map(_ =!= s.g))

  test("... but not if there was a failed step"):
    Setup
      .init(20)
      .flatMap: s =>
        setQaState(s.d, DatasetQaState.Usable) *>
        assertIO(nextAtomId(s.o), s.g)

  test("... even if there is an end atom event"):
    Setup
      .init(30)
      .flatMap: s =>
        addAtomEventAs(serviceUser, s.a, AtomStage.EndAtom) *>
        setQaState(s.d, DatasetQaState.Usable)              *>
        assertIO(nextAtomId(s.o), s.g)

  def nextAtomStepTypes(o: Observation.Id): IO[List[StepType]] =
    query(
      pi,
      s"""
        query {
          executionConfig(observationId: "$o") {
            gmosNorth {
              science {
                nextAtom {
                  steps {
                    stepConfig {
                      stepType
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downFields("executionConfig", "gmosNorth", "science", "nextAtom", "steps")
        .values.toList.flatMap(_.toList).traverse: j =>
          j.hcursor
           .downFields("stepConfig", "stepType").as[StepType]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  test("If you put the failed step into a new atom, it won't close the original one."):
    Setup
      .init(50)
      .flatMap: setup =>
        for
          _ <- setQaState(setup.d, DatasetQaState.Usable)

          // re-execute the failed step, but put it in a new atom
          a <- recordAtomAs(serviceUser, Instrument.GmosNorth, setup.v, SequenceType.Science)
          s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(45), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
          _ <- addEndStepEvent(s)
          // now we think we need an arc and a flat to make this step valid
          _ <- assertIO(nextAtomStepTypes(setup.o), List(StepType.Gcal, StepType.Gcal))
        yield ()

  test("Now we'll generate an arc and flat AND the (still) missing science step."):
    Setup
      .init(60)
      .flatMap: setup =>
        for
          _ <- setQaState(setup.d, DatasetQaState.Usable)

          // re-execute the failed step, but put it in a new atom
          a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, setup.v, SequenceType.Science)
          s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(45), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
          _  <- addEndStepEvent(s0)

          // Make yet another atom and do the arc in that one.
          a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, setup.v, SequenceType.Science)
          s1 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthArc(45), ArcStep, gcalTelescopeConfig(15), ObserveClass.NightCal)
          _  <- addEndStepEvent(s1)

          // now we think we need a flat and the original missing science step
          _ <- assertIO(nextAtomStepTypes(setup.o), List(StepType.Gcal, StepType.Science))
        yield ()
