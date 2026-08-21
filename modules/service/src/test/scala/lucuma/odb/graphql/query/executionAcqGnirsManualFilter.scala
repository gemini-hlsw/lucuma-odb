// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Band
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.odb.sequence.gnirs.AcquisitionClassificationSignalToNoise

/**
 * Reproduces story 8880 comment 9470: with a manually selected acquisition filter, the
 * slit image follows that filter for a faint target but jumps to H for a bright one.
 *
 * Setup taken from the reporter's program p-1388 — long camera, 1.60 µm, acquisition in
 * S/N mode at S/N=10 with an explicit K (Order3) filter — on two targets differing only in
 * H magnitude (12 vs 14). The real ITC returned 0.22 s and 0.60 s for those, which straddle
 * the 0.5 s Very Bright / Bright boundary of `GnirsAcquisitionMode.defaultFor`, so the fake
 * ITC here keys on the H magnitude and returns those two times.
 *
 * The requested acquisition S/N is 10, which is exactly
 * `AcquisitionClassificationSignalToNoise`, so the fixed-S/N classification pass would
 * measure the same brightness this single pass does: the Very Bright classification is real,
 * not an artifact of the user's requested S/N.
 */
class executionAcqGnirsManualFilter extends ExecutionTestSupportForGnirs:

  // The observed ITC integration times for the reporter's two targets, keyed on H magnitude.
  // 0.22 s ⇒ Very Bright (< 0.5 s); 0.60 s ⇒ Bright.
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    val hMag: Option[BigDecimal] =
      SourceProfile
        .integratedBrightnesses
        .getOption(input.asterism.head.sourceProfile)
        .flatMap(_.get(Band.H))
        .map(_.value.value.value)
    hMag.map: m =>
      IntegrationTime(if m < 13 then 220.msTimeSpan else 600.msTimeSpan, PosInt.unsafeFrom(1))

  private val VeryBrightµs: Long = 220_000L
  private val Brightµs:     Long = 600_000L
  private val KLongµs:      Long = 15_000_000L  // long-camera K slit-image exposure

  /** The reporter's targets: a point source with a single H-band Vega magnitude. */
  private def hBandProfile(hMag: BigDecimal): String =
    s"""
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: { stellarLibrary: O5_V },
            brightnesses: [ { band: H, value: $hMag, units: VEGA_MAGNITUDE } ]
          }
        }
      }
    """

  private def acqConfigQuery(oid: Observation.Id): String =
    s"""
      query {
        executionConfig(observationId: "$oid") {
          gnirs { acquisition { nextAtom { steps {
            instrumentConfig { exposure { microseconds } coadds filter readMode }
          } } } }
        }
      }
    """

  private def step(expµs: Long, filter: String, readMode: String): Json =
    json"""{ "instrumentConfig": { "exposure": { "microseconds": $expµs }, "coadds": 1, "filter": $filter, "readMode": $readMode } }"""

  /** The reporter's observation: long camera, explicit K acquisition filter, S/N=10 at 1.60 µm. */
  private def setup(hMag: BigDecimal): IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p, hBandProfile(hMag))
      o <- createGnirsLongSlitObservationAs(pi, p, t)
      _ <- setCamera(o, "LONG_BLUE")
      _ <- setAcquisitionSignalToNoise(o, AcquisitionClassificationSignalToNoise.toBigDecimal, 1600)
      _ <- setAcquisitionFilter(o, "ORDER3")
    yield o

  test("manual K filter, faint target (H=14): the slit image uses K"):
    setup(BigDecimal(14)).flatMap: oid =>
      expect(
        user     = pi,
        query    = acqConfigQuery(oid),
        expected = json"""
          {
            "executionConfig": { "gnirs": { "acquisition": { "nextAtom": { "steps": [
              ${step(KLongµs,  "ORDER3", "BRIGHT")},
              ${step(Brightµs, "ORDER3", "BRIGHT")},
              ${step(Brightµs, "ORDER3", "BRIGHT")},
              ${step(Brightµs, "ORDER3", "BRIGHT")}
            ] } } } }
          }
        """.asRight
      )

  // Documents the behaviour reported in comment 9470: the explicit K filter is honoured on
  // every step except the slit image, which the Very Bright branch of
  // `firstStepFilterAndExposure` forces to H (Order4) regardless of the selected filter.
  test("manual K filter, bright target (H=12): the slit image drops to H (story 8880 comment 9470)"):
    setup(BigDecimal(12)).flatMap: oid =>
      expect(
        user     = pi,
        query    = acqConfigQuery(oid),
        expected = json"""
          {
            "executionConfig": { "gnirs": { "acquisition": { "nextAtom": { "steps": [
              ${step(KLongµs,      "ORDER4", "BRIGHT")},
              ${step(VeryBrightµs, "ORDER3", "VERY_BRIGHT")},
              ${step(VeryBrightµs, "ORDER3", "VERY_BRIGHT")},
              ${step(VeryBrightµs, "ORDER3", "VERY_BRIGHT")}
            ] } } } }
          }
        """.asRight
      )
