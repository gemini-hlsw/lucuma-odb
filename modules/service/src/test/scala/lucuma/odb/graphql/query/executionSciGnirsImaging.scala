// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.StepGuideState
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.refined.*

class executionSciGnirsImaging extends ExecutionTestSupportForGnirs:

  // 10s (Bright) for J; 25s (Faint) so the derived read mode differs per filter.
  private val TimeJ = IntegrationTime(10.secondTimeSpan, 2.refined)
  private val TimeH = IntegrationTime(25.secondTimeSpan, 3.refined)

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(filter = f) =>
        f match
          case GnirsFilter.J      => TimeJ.some
          case GnirsFilter.Order4 => TimeH.some
          case _                  => none
      case _                                       => none

  private def sciStep(
    filter:       GnirsFilter,
    exposureTime: TimeSpan,
    p:            Int            = 0,
    q:            Int            = 0,
    guiding:      StepGuideState = StepGuideState.Enabled,
    coadds:       Int            = 1
  ): Json =
    json"""
      {
        "instrumentConfig": {
          "exposure":             { "seconds": ${exposureTime.toSeconds} },
          "coadds":               $coadds,
          "centralWavelength":    { "nanometers": ${filter.centralWavelength.toNanometers.value.value} },
          "filter":               ${filter.tag.toScreamingSnakeCase.asJson},
          "decker":               "ACQUISITION",
          "fpuSlit":              null,
          "fpuOther":             "ACQUISITION",
          "fpuIfu":               null,
          "acquisitionMirrorOut": null,
          "camera":               "SHORT_BLUE",
          "focusMotorSteps":      null,
          "readMode":             ${GnirsReadMode.forExposureTime(exposureTime).tag.toScreamingSnakeCase.asJson}
        },
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

  private def sciAtom(filter: GnirsFilter, exposureTime: TimeSpan, p: Int = 0, q: Int = 0): Json =
    sciAtomOf(sciStep(filter, exposureTime, p, q))

  private def expectedScience(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "gnirs": {
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
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Increasing wavelength: J (1.25um) before H/Order4 (1.65um).
    val atoms =
      List.fill(2)(sciAtom(GnirsFilter.J, 10.secondTimeSpan)) ++
      List.fill(3)(sciAtom(GnirsFilter.Order4, 25.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("grouped, per-filter coadds"):
    // Coadds live with the filter's exposure time mode, so each filter's steps
    // carry its own value.  Signal-to-noise doesn't support coadds, so the
    // ORDER4 filter is forced back to 1.
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [
            {
              filter: J
              exposureTimeMode: {
                timeAndCount: { time: { seconds: 10.0 }, count: 2, at: { nanometers: 1250.0 } }
              }
              coadds: 4
            },
            {
              filter: ORDER4
              exposureTimeMode: {
                signalToNoise: { value: 100.0, at: { nanometers: 1650.0 } }
              }
              coadds: 4
            }
          ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    val atoms =
      List.fill(2)(sciAtomOf(sciStep(GnirsFilter.J, 10.secondTimeSpan, coadds = 4))) ++
      List.fill(3)(sciAtomOf(sciStep(GnirsFilter.Order4, 25.secondTimeSpan)))

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("grouped, no offsets, no sky (DECREASING)"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { order: DECREASING, skyCount: 0 } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Decreasing wavelength: H/Order4 before J.
    val atoms =
      List.fill(3)(sciAtom(GnirsFilter.Order4, 25.secondTimeSpan)) ++
      List.fill(2)(sciAtom(GnirsFilter.J, 10.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

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

  private def skyPair(filter: GnirsFilter, t: TimeSpan): List[Json] =
    List(
      sciStep(filter, t, 10, 10, StepGuideState.Disabled),
      sciStep(filter, t, 20, 20, StepGuideState.Disabled)
    )

  test("grouped, no offsets, sky"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 2, $skyOffsetsInput } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // One atom per filter: sky, sky, object exposures, sky, sky.
    def mkAtom(filter: GnirsFilter, t: TimeSpan, count: Int): Json =
      sciAtomOf((skyPair(filter, t) ++ List.fill(count)(sciStep(filter, t)) ++ skyPair(filter, t))*)

    val atoms =
      List(
        mkAtom(GnirsFilter.J, 10.secondTimeSpan, 2),
        mkAtom(GnirsFilter.Order4, 25.secondTimeSpan, 3)
      )

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("interleaved, no offsets, sky"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { interleaved: { skyCount: 2, $skyOffsetsInput } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    val fj = GnirsFilter.J
    val fh = GnirsFilter.Order4
    val tj = 10.secondTimeSpan
    val th = 25.secondTimeSpan

    // Each atom is bracketed by the same guide-disabled sky steps (forward at
    // the head, reversed at the tail), with the interleaved object exposures in
    // between.  groupCount = min(2, 3) = 2, so there are two atoms.
    val skyHead = skyPair(fj, tj) ++ skyPair(fh, th)
    val skyTail = skyPair(fh, th).reverse ++ skyPair(fj, tj).reverse

    val atoms =
      List(
        sciAtomOf((skyHead ++ List(sciStep(fj, tj), sciStep(fh, th), sciStep(fh, th)) ++ skyTail)*),
        sciAtomOf((skyHead ++ List(sciStep(fj, tj), sciStep(fh, th)) ++ skyTail)*)
      )

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  // The acquisition sequence itself is asserted in detail in executionAcqGnirsImaging;
  // here we just confirm it is now generated (no longer null).
  test("acquisition sequence is generated"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          filters: [ { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    setup.flatMap: oid =>
      query(pi, gnirsAcquisitionQuery(oid)).map: json =>
        assert(json.hcursor.downFields("executionConfig", "gnirs", "acquisition").focus.exists(!_.isNull))

  test("[gnirs] an imaging filter change costs the configured overhead"):
    // Grouped imaging runs all of J then all of Order4, so the wheel moves once,
    // entering the second filter's block.
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    setup.flatMap: oid =>
      query(
        pi,
        s"""
          query {
            executionConfig(observationId: "$oid") {
              gnirs {
                science {
                  nextAtom { steps { estimate { configChange { all { name estimate { seconds } } } } } }
                  possibleFuture { steps { estimate { configChange { all { name estimate { seconds } } } } } }
                }
              }
            }
          }
        """
      ).map: json =>
        val sci = json.hcursor.downField("executionConfig").downField("gnirs").downField("science")
        val atoms = sci.downField("nextAtom").focus.toList ++
                    sci.downField("possibleFuture").values.toList.flatten
        val filterChanges =
          atoms.flatMap: atom =>
            atom.hcursor.downField("steps").values.toList.flatten.flatMap: step =>
              step.hcursor
                .downField("estimate").downField("configChange").downField("all")
                .values.toList.flatten
                .flatMap: c =>
                  (for
                    n <- c.hcursor.downField("name").as[String].toOption
                    v <- c.hcursor.downField("estimate").downField("seconds").as[BigDecimal].toOption
                    if n == "GNIRS Filter"
                  yield v).toList

        assertEquals(filterChanges, List(BigDecimal("10.000000")))
