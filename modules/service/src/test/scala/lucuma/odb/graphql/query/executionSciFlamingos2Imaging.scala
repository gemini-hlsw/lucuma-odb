// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode

class executionSciFlamingos2Imaging extends ExecutionTestSupportForFlamingos2:

  private val TimeY = IntegrationTime(10.secondTimeSpan, PosInt.unsafeFrom(2))
  private val TimeJ = IntegrationTime(20.secondTimeSpan, PosInt.unsafeFrom(3))

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.Flamingos2Imaging(filter = f) =>
        f match
          case Flamingos2Filter.Y => TimeY.some
          case Flamingos2Filter.J => TimeJ.some
          case _                  => none
      case _                                            => none

  private def imagingDynamic(filter: Flamingos2Filter, exposureTime: TimeSpan): Flamingos2DynamicConfig =
    Flamingos2DynamicConfig(
      exposureTime,
      none,
      filter,
      Flamingos2ReadMode.Faint,
      Flamingos2LyotWheel.F16,
      Flamingos2FpuMask.Imaging,
      Flamingos2Decker.Imaging,
      Flamingos2ReadoutMode.Science,
      Flamingos2ReadMode.Faint.readCount
    )

  private def sciAtom(filter: Flamingos2Filter, exposureTime: TimeSpan, p: Int = 0, q: Int = 0): Json =
    json"""
      {
        "description": null,
        "observeClass": "SCIENCE",
        "steps": [
          {
            "instrumentConfig": ${flamingos2ExpectedInstrumentConfig(imagingDynamic(filter, exposureTime))},
            "stepConfig": { "stepType": "SCIENCE" },
            "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Enabled)},
            "observeClass": "SCIENCE",
            "breakpoint": "DISABLED"
          }
        ]
      }
    """

  private def expectedScience(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "flamingos2": {
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail.asJson},
              "hasMore": false
            }
          }
        }
      }
    """

  test("grouped, no offsets, no sky (INCREASING)"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { grouped: { skyCount: 0 } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Increasing wavelength: Y (1.02um) before J (1.25um).
    val atoms =
      List.fill(2)(sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan)) ++
      List.fill(3)(sciAtom(Flamingos2Filter.J, 20.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, flamingos2ScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("grouped, no offsets, no sky (DECREASING)"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { grouped: { order: DECREASING, skyCount: 0 } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Decreasing wavelength: J before Y.
    val atoms =
      List.fill(3)(sciAtom(Flamingos2Filter.J, 20.secondTimeSpan)) ++
      List.fill(2)(sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, flamingos2ScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("interleaved, no offsets, no sky"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { interleaved: { skyCount: 0 } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // groupCount = min(2, 3) = 2.  Group 0 gets Y x1, J x2; group 1 gets Y x1, J x1.
    val atoms =
      List(
        sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan),
        sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan)
      )

    setup.flatMap: oid =>
      expect(pi, flamingos2ScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("preImaging"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: {
            preImaging: {
              offset1: { p: { arcseconds: 1 }, q: { arcseconds: 1 } }
              offset2: { p: { arcseconds: 2 }, q: { arcseconds: 2 } }
              offset3: { p: { arcseconds: 3 }, q: { arcseconds: 3 } }
              offset4: { p: { arcseconds: 4 }, q: { arcseconds: 4 } }
            }
          }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Increasing order (Y, J); each filter cycles through the 4 offsets
    val atoms =
      List(
        sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan, 1, 1),
        sciAtom(Flamingos2Filter.Y, 10.secondTimeSpan, 2, 2),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan, 1, 1),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan, 2, 2),
        sciAtom(Flamingos2Filter.J, 20.secondTimeSpan, 3, 3)
      )

    setup.flatMap: oid =>
      expect(pi, flamingos2ScienceQuery(oid, 100.some), expectedScience(atoms).asRight) *>
      expect(
        pi,
        s"""query { executionConfig(observationId: "$oid") { flamingos2 { static { mosPreImaging } } } }""",
        Right(json"""
          { "executionConfig": { "flamingos2": { "static": { "mosPreImaging": "IS_MOS_PRE_IMAGING" } } } }
        """)
      )

  test("Flamingos2 imaging materializes and serves the persisted sequence"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { grouped: { skyCount: 0 } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[(Atom.Id, Atom.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createObservationWithModeAs(pi, p, List(t), mode)
        v  <- recordVisitAs(serviceUser, o)
        a0 <- firstScienceAtomId(serviceUser, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)
        _  <- ss.traverse_(sid => addEndStepEvent(sid, v))
        a1 <- firstScienceAtomId(serviceUser, o)
      yield (a0, a1)

    setup.map: (a0, a1) =>
      assertNotEquals(a0, a1)
