// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
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
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.odb.sequence.gnirs.imaging.Acquisition.RepeatingAtomCount
import lucuma.refined.*

// GNIRS imaging acquisition. The brightness type (Very Bright / Bright / Faint) is
// classified from a fixed S/N=10 ITC pass on the first (wavelength-ordered) imaging
// filter, exactly as for spectroscopy.
class executionAcqGnirsImaging extends ExecutionTestSupportForGnirs:

  // The acquisition ITC keys on the filter alone, deliberately ignoring the requested
  // signal-to-noise:
  //   - the first-filter classification pass sizes the exposure and picks the type;
  //   - Very Bright additionally re-images the target through H2.
  // Being S/N-independent means the second pass -- which runs at the *derived* S/N
  // (Very Bright 30, Bright 20, Faint 10) -- returns the same per-filter exposure as the
  // classification pass, so these tests stay focused on the type and filter selection.
  // See executionAcqGnirsDerivedSignalToNoise for the derived S/N itself.
  // Different first filters drive different types across the tests:
  //   J  → 2s × 3 = 6s   → Bright
  //   K  → 30s × 1 = 30s → Faint
  //   Y  → 0.3s × 1      → Very Bright (with an H2 pass of 2s × 3)
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(ExposureTimeMode.SignalToNoiseMode(_, _), filter, _, _, _, _, _) =>
        filter match
          case GnirsFilter.J      => IntegrationTime(2.secTimeSpan,  3.refined).some
          case GnirsFilter.K      => IntegrationTime(30.secTimeSpan, 1.refined).some
          case GnirsFilter.Y      => IntegrationTime(300.msTimeSpan, 1.refined).some
          case GnirsFilter.H2     => IntegrationTime(2.secTimeSpan,  3.refined).some
          case GnirsFilter.Order4 => IntegrationTime(5.secTimeSpan,  1.refined).some
          case _                  => none
      // Time-and-count echoes the user's request, as the real ITC does.
      case InstrumentMode.GnirsImaging(ExposureTimeMode.TimeAndCountMode(time, count, _), _, _, _, _, _, _) =>
        IntegrationTime(time, count).some
      case _ => none

  /** Applies an acquisition customization block to a GNIRS imaging observation. */
  private def setImagingAcquisition(oid: Observation.Id, acquisition: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsImaging: {
                  acquisition: $acquisition
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private val AcqStepsQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { microseconds }
          coadds
          filter
          decker
          fpuOther
          camera
          readMode
        }
        telescopeConfig {
          offset { p { arcseconds } q { arcseconds } }
          guiding
        }
        observeClass
        breakpoint
      }
    """

  private def gnirsAcqImagingQuery(oid: Observation.Id): String =
    executionConfigQuery(oid, "gnirs", "acquisition", AcqStepsQuery, None)

  private def readMode(t: TimeSpan): String =
    GnirsReadMode.forExposureTime(t).tag.toScreamingSnakeCase

  private def acqStep(
    exposure:   TimeSpan,
    coadds:     Int,
    filter:     GnirsFilter,
    camera:     String,
    p:          Int,
    q:          Int,
    guiding:    StepGuideState,
    breakpoint: String = "DISABLED"
  ): Json =
    json"""
      {
        "instrumentConfig": {
          "exposure":  { "microseconds": ${exposure.toMicroseconds} },
          "coadds":    $coadds,
          "filter":    ${filter.tag.toScreamingSnakeCase.asJson},
          "decker":    "ACQUISITION",
          "fpuOther":  "ACQUISITION",
          "camera":    ${camera.asJson},
          "readMode":  ${readMode(exposure).asJson}
        },
        "telescopeConfig": ${expectedTelescopeConfig(p, q, guiding)},
        "observeClass": "ACQUISITION",
        "breakpoint":   ${breakpoint.asJson}
      }
    """

  // The expected acquisition: an "Initial Acquisition" atom (field image + on-target,
  // the on-target carrying a breakpoint), then RepeatingAtomCount "Fine Adjustments"
  // atoms, each a single on-target image. `onTargetBreak` is the on-target with its
  // breakpoint enabled; `onTarget` is the same step without a breakpoint.
  private def expectedAcquisition(field: Json, onTargetBreak: Json, onTarget: Json): Json =
    val fineAdjustment =
      json"""{ "description": "Fine Adjustments", "observeClass": "ACQUISITION", "steps": [ $onTarget ] }"""
    json"""
      {
        "executionConfig": {
          "gnirs": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [ $field, $onTargetBreak ]
              },
              "possibleFuture": ${List.fill(RepeatingAtomCount)(fineAdjustment).asJson},
              "hasMore": false
            }
          }
        }
      }
    """

  // The on-target step, with and without an enabled breakpoint (for the initial atom
  // and the repeating "Fine Adjustments" atoms respectively).
  private def onTargetSteps(exposure: TimeSpan, coadds: Int, filter: GnirsFilter, camera: String): (Json, Json) =
    (acqStep(exposure, coadds, filter, camera, 0, 0, StepGuideState.Enabled, "ENABLED"),
     acqStep(exposure, coadds, filter, camera, 0, 0, StepGuideState.Enabled))

  private def imagingObs(camera: String, filter: String): IO[Observation.Id] =
    val mode =
      s"""
        gnirsImaging: {
          camera: $camera
          filters: [ { filter: $filter } ]
        }
      """
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
    yield o

  test("Bright: field image in first filter (fixed short-camera exposure), on-target in first filter"):
    // First filter J classifies as Bright (6s). Field image: (10,0), J, 3s, 1 coadd.
    // On-target: (0,0), J, 2s (ITC), 3 coadds.
    val field                     = acqStep(3.secTimeSpan, 1, GnirsFilter.J, "SHORT_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(2.secTimeSpan, 3, GnirsFilter.J, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "J").flatMap: oid =>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("Very Bright: field image in H (Order4), on-target in H2"):
    // First filter Y classifies as Very Bright (0.3s). Field image: (10,0), H (Order4),
    // 3s, 1 coadd. On-target: (0,0), H2, 2s (H2 ITC pass), 3 coadds.
    val field                     = acqStep(3.secTimeSpan, 1, GnirsFilter.Order4, "SHORT_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(2.secTimeSpan, 3, GnirsFilter.H2, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "Y").flatMap: oid =>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("Faint: both images in first filter with the ITC exposure, field offset (0,10)"):
    // First filter K classifies as Faint (30s). Field/keyhole image: (0,10), K, 30s
    // (ITC), 1 coadd. On-target: (0,0), K, 30s, 1 coadd.
    val field                     = acqStep(30.secTimeSpan, 1, GnirsFilter.K, "SHORT_BLUE", 0, 10, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(30.secTimeSpan, 1, GnirsFilter.K, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "K").flatMap: oid =>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("Long camera uses a 15s keyhole exposure"):
    // Same Bright classification as the short-camera case, but the long camera's fixed
    // keyhole exposure is 15s (vs 3s short).
    val field                     = acqStep(15.secTimeSpan, 1, GnirsFilter.J, "LONG_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(2.secTimeSpan, 3, GnirsFilter.J, "LONG_BLUE")
    imagingObs("LONG_BLUE", "J").flatMap: oid =>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("An explicit BRIGHT type overrides the Faint classification"):
    // First filter K would classify as Faint (30s), but the explicit type wins: the field
    // image reverts to the fixed keyhole exposure at (10,0) with a single coadd.
    val field                     = acqStep(3.secTimeSpan, 1, GnirsFilter.K, "SHORT_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(30.secTimeSpan, 1, GnirsFilter.K, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "K").flatMap: oid =>
      setImagingAcquisition(oid, "{ explicitAcquisitionType: BRIGHT }") *>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("An explicit FAINT type uses the configured sky offset for the field image"):
    // First filter J would classify as Bright, but the explicit FAINT type puts the field
    // image at the configured sky offset with the ITC exposure and coadds.
    val field                     = acqStep(2.secTimeSpan, 3, GnirsFilter.J, "SHORT_BLUE", 0, 5, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(2.secTimeSpan, 3, GnirsFilter.J, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "J").flatMap: oid =>
      setImagingAcquisition(oid,
        """{
          explicitAcquisitionType: FAINT
          skyOffset: { p: { arcseconds: 0.0 }, q: { arcseconds: 5.0 } }
        }"""
      ) *>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("An explicit acquisition filter replaces the first science filter"):
    // The science filter is J, but the acquisition images through Order4 — both in the
    // ITC call (5s => Bright) and in the generated steps.
    val field                     = acqStep(3.secTimeSpan, 1, GnirsFilter.Order4, "SHORT_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(5.secTimeSpan, 1, GnirsFilter.Order4, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "J").flatMap: oid =>
      setImagingAcquisition(oid, "{ explicitFilter: ORDER4 }") *>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("An explicit acquisition filter is kept even for a Very Bright classification"):
    // Y classifies as Very Bright, which would normally image the target through H2. An
    // explicit filter overrides that; only the field image stays on H (Order4).
    val field                     = acqStep(3.secTimeSpan, 1, GnirsFilter.Order4, "SHORT_BLUE", 10, 0, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(5.secTimeSpan, 1, GnirsFilter.Order4, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "Y").flatMap: oid =>
      setImagingAcquisition(oid, "{ explicitAcquisitionType: VERY_BRIGHT, explicitFilter: ORDER4 }") *>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)

  test("Acquisition coadds come from the acquisition config in time-and-count mode"):
    // In time-and-count mode the ITC echoes the requested exposure and the explicit
    // coadds size the steps (rather than the ITC exposure count).
    val field                     = acqStep(4.secTimeSpan, 5, GnirsFilter.J, "SHORT_BLUE", 0, 10, StepGuideState.Disabled)
    val (onTargetBreak, onTarget) = onTargetSteps(4.secTimeSpan, 5, GnirsFilter.J, "SHORT_BLUE")
    imagingObs("SHORT_BLUE", "J").flatMap: oid =>
      setImagingAcquisition(oid,
        """{
          explicitAcquisitionType: FAINT
          skyOffset: { p: { arcseconds: 0.0 }, q: { arcseconds: 10.0 } }
          explicitExposureTimeMode: {
            timeAndCount: { time: { seconds: 4.0 }, count: 1, at: { nanometers: 1250.0 } }
          }
          coadds: 5
        }"""
      ) *>
      expect(pi, gnirsAcqImagingQuery(oid), expectedAcquisition(field, onTargetBreak, onTarget).asRight)
