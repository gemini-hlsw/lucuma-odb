// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.ItcResultReadMode
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.refined.*

class executionSciGnirsImaging extends ExecutionTestSupportForGnirs:

  // The S/N results carry the read mode the ITC chose, each one no exposure time rule
  // would derive (10s and 25s both default to Bright), so a step can only have it from
  // the ITC.
  private val ReadModeJ = GnirsReadMode.Faint
  private val ReadModeH = GnirsReadMode.VeryFaint
  private val TimeJ     = IntegrationTime(10.secondTimeSpan, 2.refined, readMode = ItcResultReadMode.Gnirs(ReadModeJ).some)
  private val TimeH     = IntegrationTime(25.secondTimeSpan, 3.refined, readMode = ItcResultReadMode.Gnirs(ReadModeH).some)

  private def itcReadMode(filter: GnirsFilter): GnirsReadMode =
    filter match
      case GnirsFilter.J => ReadModeJ
      case _             => ReadModeH

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(exposureTimeMode = etm, filter = f) =>
        etm match
          case ExposureTimeMode.SignalToNoiseMode(_, _)   =>
            f match
              case GnirsFilter.J      => TimeJ.some
              case GnirsFilter.Order4 => TimeH.some
              case _                  => none
          // Time-and-count echoes the request; OdbSuite adds the requested read mode.
          case ExposureTimeMode.TimeAndCountMode(t, c, _) =>
            IntegrationTime(t, c).some
      case _                                                             => none

  private def sciStep(
    filter:       GnirsFilter,
    exposureTime: TimeSpan,
    p:            Int            = 0,
    q:            Int            = 0,
    guiding:      StepGuideState = StepGuideState.Enabled,
    coadds:       Int            = 1,
    readMode:     Option[GnirsReadMode] = None
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
          "readMode":             ${readMode.getOrElse(itcReadMode(filter)).tag.toScreamingSnakeCase.asJson}
        },
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, guiding)},
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  // One smart flat per filter expands into the fixture's lamp-on and lamp-off
  // exposures, unguided at the last science position. The read mode follows
  // the flat's own exposure, and the coadds (1) come from the fixture too.
  private def flatStep(filter: GnirsFilter, exposure: TimeSpan, p: Int, q: Int): Json =
    json"""
      {
        "instrumentConfig": {
          "exposure":             { "seconds": ${exposure.toSeconds} },
          "coadds":               1,
          "centralWavelength":    { "nanometers": ${filter.centralWavelength.toNanometers.value.value} },
          "filter":               ${filter.tag.toScreamingSnakeCase.asJson},
          "decker":               "ACQUISITION",
          "fpuSlit":              null,
          "fpuOther":             "ACQUISITION",
          "fpuIfu":               null,
          "acquisitionMirrorOut": null,
          "camera":               "SHORT_BLUE",
          "focusMotorSteps":      null,
          "readMode":             ${GnirsReadMode.forExposureTime(exposure).tag.toScreamingSnakeCase.asJson}
        },
        "stepConfig": { "stepType": "GCAL" },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, StepGuideState.Disabled)},
        "observeClass": "NIGHT_CAL",
        "breakpoint": "DISABLED"
      }
    """

  private def flatExposure(filter: GnirsFilter): TimeSpan =
    filter match
      case GnirsFilter.J => GnirsImagingFlatExposureJ
      case _             => GnirsImagingFlatExposureH

  // The flats closing a filter's science, in the same atom.
  private def flatSteps(filter: GnirsFilter, p: Int, q: Int): List[Json] =
    List.fill(2 * GnirsImagingFlatCount)(flatStep(filter, flatExposure(filter), p, q))

  private def sciAtomOf(steps: List[Json]): Json =
    json"""
      {
        "description": null,
        "observeClass": "SCIENCE",
        "steps": ${steps.asJson}
      }
    """

  private def sciAtom(filter: GnirsFilter, exposureTime: TimeSpan, p: Int, q: Int): Json =
    sciAtomOf(List(sciStep(filter, exposureTime, p, q)))

  // A filter's science exposures and its flats, at the last science position.
  private def filterAtom(science: List[Json], filter: GnirsFilter, p: Int = 0, q: Int = 0): Json =
    sciAtomOf(science ++ flatSteps(filter, p, q))

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

    // Increasing wavelength: J (1.25um) before H/Order4 (1.65um). Each filter
    // is one atom: its exposures and then its flats.
    val atoms =
      List(
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 10.secondTimeSpan)), GnirsFilter.J),
        filterAtom(List.fill(3)(sciStep(GnirsFilter.Order4, 25.secondTimeSpan)), GnirsFilter.Order4)
      )

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

    // Time-and-count takes the default read mode (Bright at 10s), S/N the ITC's.
    val atoms =
      List(
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 10.secondTimeSpan, coadds = 4, readMode = GnirsReadMode.Bright.some)), GnirsFilter.J),
        filterAtom(List.fill(3)(sciStep(GnirsFilter.Order4, 25.secondTimeSpan)), GnirsFilter.Order4)
      )

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("time-and-count read mode defaults to Bright, Very Bright under 0.6s (sc-10909)"):
    // Unlike spectroscopy, a long imaging exposure does not drop to Faint.
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [
            {
              filter: J
              exposureTimeMode: {
                timeAndCount: { time: { seconds: 25.0 }, count: 2, at: { nanometers: 1250.0 } }
              }
            },
            {
              filter: ORDER4
              exposureTimeMode: {
                timeAndCount: { time: { seconds: 0.5 }, count: 3, at: { nanometers: 1650.0 } }
              }
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
      List(
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 25.secondTimeSpan, readMode = GnirsReadMode.Bright.some)), GnirsFilter.J),
        filterAtom(List.fill(3)(sciStep(GnirsFilter.Order4, 500.msTimeSpan, readMode = GnirsReadMode.VeryBright.some)), GnirsFilter.Order4)
      )

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("an explicit read mode overrides the ITC's"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          explicitReadMode: BRIGHT
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // The flats' read mode follows their own exposure, not the explicit one.
    val atoms =
      List(
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 10.secondTimeSpan, readMode = GnirsReadMode.Bright.some)), GnirsFilter.J),
        filterAtom(List.fill(3)(sciStep(GnirsFilter.Order4, 25.secondTimeSpan, readMode = GnirsReadMode.Bright.some)), GnirsFilter.Order4)
      )

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
      List(
        filterAtom(List.fill(3)(sciStep(GnirsFilter.Order4, 25.secondTimeSpan)), GnirsFilter.Order4),
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 10.secondTimeSpan)), GnirsFilter.J)
      )

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

    // One atom per filter: sky, sky, object exposures, sky, sky, then the
    // flats at the last (sky) position.
    def mkAtom(filter: GnirsFilter, t: TimeSpan, count: Int): Json =
      filterAtom(skyPair(filter, t) ++ List.fill(count)(sciStep(filter, t)) ++ skyPair(filter, t), filter, 20, 20)

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

    // Interleaving spreads each filter over the whole sequence, so the flats
    // of both filters close the last atom, at its last (sky) position.
    val atoms =
      List(
        sciAtomOf(skyHead ++ List(sciStep(fj, tj), sciStep(fh, th), sciStep(fh, th)) ++ skyTail),
        sciAtomOf(skyHead ++ List(sciStep(fj, tj), sciStep(fh, th)) ++ skyTail ++ flatSteps(fj, 10, 10) ++ flatSteps(fh, 10, 10))
      )

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  // Three guided object positions, at (1, 1), (2, 2) and (3, 3).
  private val objectOffsetsInput: String =
    s"""
      offsets: {
        enumerated: {
          values: [
            { offset: { p: { arcseconds: 1 }, q: { arcseconds: 1 } }, guiding: ENABLED },
            { offset: { p: { arcseconds: 2 }, q: { arcseconds: 2 } }, guiding: ENABLED },
            { offset: { p: { arcseconds: 3 }, q: { arcseconds: 3 } }, guiding: ENABLED }
          ]
        }
      }
    """

  private def exposuresPerOffsetTest(variant: String, atoms: List[Json]): IO[Unit] =
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { $variant }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
      _ <- expect(pi, gnirsScienceQuery(o, 100.some), expectedScience(atoms).asRight)
    yield ()

  test("grouped, offsets, exposuresPerOffset"):
    // Two exposures per position: J (2) fills position 1, Order4 (3) fills
    // position 1 and takes one more at position 2.
    exposuresPerOffsetTest(
      s"grouped: { skyCount: 0, exposuresPerOffset: 2, $objectOffsetsInput }",
      List(
        filterAtom(
          List(
            sciStep(GnirsFilter.J, 10.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.J, 10.secondTimeSpan, 1, 1)
          ),
          GnirsFilter.J, 1, 1
        ),
        filterAtom(
          List(
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 2, 2)
          ),
          GnirsFilter.Order4, 2, 2
        )
      )
    )

  test("interleaved, offsets, exposuresPerOffset"):
    // Stops: J [2], Order4 [2, 1].  A single group, with an offset position per
    // stop.
    exposuresPerOffsetTest(
      s"interleaved: { skyCount: 0, exposuresPerOffset: 2, $objectOffsetsInput }",
      List(
        sciAtom(GnirsFilter.J,      10.secondTimeSpan, 1, 1),
        sciAtom(GnirsFilter.J,      10.secondTimeSpan, 1, 1),
        sciAtom(GnirsFilter.Order4, 25.secondTimeSpan, 2, 2),
        sciAtom(GnirsFilter.Order4, 25.secondTimeSpan, 2, 2),
        sciAtomOf(List(sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 3, 3)) ++ flatSteps(GnirsFilter.J, 3, 3) ++ flatSteps(GnirsFilter.Order4, 3, 3))
      )
    )

  test("preImaging, exposuresPerOffset"):
    exposuresPerOffsetTest(
      s"""
        preImaging: {
          offset1: { p: { arcseconds: 1 }, q: { arcseconds: 1 } }
          offset2: { p: { arcseconds: 2 }, q: { arcseconds: 2 } }
          offset3: { p: { arcseconds: 3 }, q: { arcseconds: 3 } }
          offset4: { p: { arcseconds: 4 }, q: { arcseconds: 4 } }
          exposuresPerOffset: 2
        }
      """,
      List(
        filterAtom(
          List(
            sciStep(GnirsFilter.J, 10.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.J, 10.secondTimeSpan, 1, 1)
          ),
          GnirsFilter.J, 1, 1
        ),
        filterAtom(
          List(
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 1, 1),
            sciStep(GnirsFilter.Order4, 25.secondTimeSpan, 2, 2)
          ),
          GnirsFilter.Order4, 2, 2
        )
      )
    )

  test("a filter without a smart flat row gets no flats"):
    // L lies past the table's last imaging row (2.49 µm): GCAL saturates in the
    // thermal IR, so L's exposures stay one atom each while J gets its flats.
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [
            { filter: J },
            {
              filter: ORDER2
              exposureTimeMode: {
                timeAndCount: { time: { seconds: 10.0 }, count: 1, at: { nanometers: 3500.0 } }
              }
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
      List(
        filterAtom(List.fill(2)(sciStep(GnirsFilter.J, 10.secondTimeSpan)), GnirsFilter.J),
        sciAtomOf(List(sciStep(GnirsFilter.Order2, 10.secondTimeSpan, readMode = GnirsReadMode.Bright.some)))
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
