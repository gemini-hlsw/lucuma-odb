// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
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
import lucuma.refined.*

class executionSciFlamingos2Imaging extends ExecutionTestSupportForFlamingos2:

  private val TimeY = IntegrationTime(10.secondTimeSpan, 2.refined)
  private val TimeJ = IntegrationTime(20.secondTimeSpan, 3.refined)

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

  private def sciStep(
    filter:       Flamingos2Filter,
    exposureTime: TimeSpan,
    p:            Int            = 0,
    q:            Int            = 0,
    guiding:      StepGuideState = StepGuideState.Enabled
  ): Json =
    json"""
      {
        "instrumentConfig": ${flamingos2ExpectedInstrumentConfig(imagingDynamic(filter, exposureTime))},
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, guiding)},
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  private def sciAtomOf(steps: Json*): Json =
    json"""
      {
        "description": null,
        "observeClass": "SCIENCE",
        "steps": ${steps.toList.asJson}
      }
    """

  private def sciAtom(filter: Flamingos2Filter, exposureTime: TimeSpan, p: Int = 0, q: Int = 0): Json =
    sciAtomOf(sciStep(filter, exposureTime, p, q))

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

  // Two guide-disabled sky positions, shared across filters.  Sky steps must be
  // observeClass SCIENCE (not NIGHT_CAL) even though guiding is disabled.
  private val skyOffsetsInput: String =
    s"""
      skyOffsets: {
        enumerated: {
          values: [
            { offset: { p: { arcseconds: 10 }, q: { arcseconds: 10 } }, guiding: DISABLED },
            { offset: { p: { arcseconds: 20 }, q: { arcseconds: 20 } }, guiding: DISABLED }
          ]
        }
      }
    """

  private def skyPair(filter: Flamingos2Filter, t: TimeSpan): List[Json] =
    List(
      sciStep(filter, t, 10, 10, StepGuideState.Disabled),
      sciStep(filter, t, 20, 20, StepGuideState.Disabled)
    )

  test("grouped, no offsets, sky"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { grouped: { skyCount: 2, $skyOffsetsInput } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // One atom per filter: sky, sky, object exposures, sky, sky.
    def mkAtom(filter: Flamingos2Filter, t: TimeSpan, count: Int): Json =
      sciAtomOf((skyPair(filter, t) ++ List.fill(count)(sciStep(filter, t)) ++ skyPair(filter, t))*)

    val atoms =
      List(
        mkAtom(Flamingos2Filter.Y, 10.secondTimeSpan, 2),
        mkAtom(Flamingos2Filter.J, 20.secondTimeSpan, 3)
      )

    setup.flatMap: oid =>
      expect(pi, flamingos2ScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("interleaved, no offsets, sky"):
    val mode =
      s"""
        flamingos2Imaging: {
          variant: { interleaved: { skyCount: 2, $skyOffsetsInput } }
          filters: [ { filter: Y }, { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    val Y = Flamingos2Filter.Y
    val J = Flamingos2Filter.J
    val ty = 10.secondTimeSpan
    val tj = 20.secondTimeSpan

    // Each atom is bracketed by the same guide-disabled sky steps (forward at
    // the head, reversed at the tail), with the interleaved object exposures in
    // between.  groupCount = min(2, 3) = 2, so there are two atoms.
    val skyHead = skyPair(Y, ty) ++ skyPair(J, tj)
    val skyTail = skyPair(J, tj).reverse ++ skyPair(Y, ty).reverse

    val atoms =
      List(
        sciAtomOf((skyHead ++ List(sciStep(Y, ty), sciStep(J, tj), sciStep(J, tj)) ++ skyTail)*),
        sciAtomOf((skyHead ++ List(sciStep(Y, ty), sciStep(J, tj)) ++ skyTail)*)
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

  test("Flamingos2 imaging ITC results are exposed"):
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

    setup.flatMap: oid =>
      expect(
        pi,
        s"""
          query {
            observation(observationId: "$oid") {
              itc {
                itcType
                ... on ItcFlamingos2Imaging {
                  flamingos2ImagingScience { filter }
                }
              }
            }
          }
        """,
        Right(json"""
          {
            "observation": {
              "itc": {
                "itcType": "FLAMINGOS_2_IMAGING",
                "flamingos2ImagingScience": [
                  { "filter": "Y" },
                  { "filter": "J" }
                ]
              }
            }
          }
        """)
      )
