// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import cats.syntax.eq.*
import cats.syntax.either.*
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

// Definitions that are shared across GNIRS modes.
val MinAcquisitionExposureTime: TimeSpan = 100.msTimeSpan  // 0.1 s; actually determined by the read mode, but this is a reasonable lower bound for all modes.
val MaxAcquisitionExposureTime: TimeSpan = 60.secondTimeSpan

// The fixed, single-coadd exposure time for the initial "keyhole"/field image of an
// acquisition, as a function of the camera: 15s for the long cameras (0.05"/pix) and
// 3s for the short cameras (0.15"/pix). This is the H-band value the spectroscopy
// acquisition falls back to, reused as-is for imaging acquisitions.
def keyholeExposureTime(camera: GnirsCamera): TimeSpan =
  if camera.pixelScale === GnirsPixelScale.PixelScale_0_05 then 15.secTimeSpan else 3.secTimeSpan

// The fixed signal-to-noise used to classify the acquisition mode (Very Bright /
// Bright / Faint) from target brightness, independent of the user's requested S/N.
// See the two-pass acquisition ITC in ItcService.
//
// By construction this equals `acquisitionSignalToNoise(Faint)`, which is why a
// target that classifies as Faint needs no second ITC pass: the classification
// call already computed its exposure at the S/N the acquisition will use.
val AcquisitionClassificationSignalToNoise: SignalToNoise =
  acquisitionSignalToNoise(GnirsAcquisitionType.Faint)

// The signal-to-noise an automatic acquisition targets, as a function of the
// brightness classification: brighter targets are acquired at a higher S/N because
// the exposure needed to reach it is still short.
//
// ATTENTION: duplicated in the migration that backfills c_is_explicit. Modify in sync.
def acquisitionSignalToNoise(acquisitionType: GnirsAcquisitionType): SignalToNoise =
  val sn = acquisitionType match
    case GnirsAcquisitionType.VeryBright => 30
    case GnirsAcquisitionType.Bright     => 20
    case GnirsAcquisitionType.Faint      => 10
  SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(sn))

// The acquisition exposure time mode to use when the user has not set one: always
// signal-to-noise, at the value the brightness classification calls for. An explicit
// acquisition type determines the classification outright; otherwise we start from Faint
// and the ITC rewrites it once it has classified the target.
def derivedAcquisitionExposureTimeMode(
  acquisitionType: Option[GnirsAcquisitionType],
  at:              Wavelength
): ExposureTimeMode =
  ExposureTimeMode.SignalToNoiseMode(
    acquisitionSignalToNoise(acquisitionType.getOrElse(GnirsAcquisitionType.Faint)),
    at
  )

/**
 * The 1-based occurrence ordinal of each wavelength within `ws`, in list order, or
 * `None` where that wavelength occurs only once.
 *
 * A repeated GNIRS central wavelength is an independent configuration -- its own
 * exposure time mode, coadds and ITC result -- so anything that names a configuration
 * by its wavelength has to say which occurrence it means.  Everything that does
 * (sequence atom titles, the low signal-to-noise warning, the configuration and
 * exposure checks) shares this rule, so the observer sees one numbering throughout;
 * and `None` for a wavelength that occurs once leaves every message an all-distinct
 * observation produces exactly as it was.
 */
def wavelengthOccurrences(ws: List[Wavelength]): List[Option[Int]] =
  ws.zipWithIndex.map: (w, i) =>
    Option.when(ws.count(_ === w) > 1)(ws.take(i).count(_ === w) + 1)

/** Appends an occurrence ordinal to an already-formatted wavelength, if there is one. */
def withOccurrence(label: String, occurrence: Option[Int]): String =
  occurrence.fold(label)(n => s"$label #$n")

/**
 * The filter and (fixed, single-coadd) exposure time for the first acquisition image —
 * the slit image in spectroscopy, the keyhole image in imaging — as a function of the
 * camera (short = 0.15"/pix, long = 0.05"/pix) and the selected acquisition filter.
 *
 * PAH can never be used on the short camera (the sky is too bright) — that yields an
 * error. Otherwise the values come from a per-camera table, whatever the acquisition
 * mode: VeryBright only changes the *default* selected filter (H2, which maps to H
 * here), an explicit filter is honoured like any other:
 *
 *   Short:  X=10s, J=15s, H=3s, K=3s, H2→H(3s), PAH→error (sky too bright)
 *   Long:   X→H, J→H, H=15s, K=15s, H2→H(15s), PAH=0.5s
 *
 * See https://app.shortcut.com/lucuma/story/8880/gnirs-acquisition-initial-slit-image
 *
 * The table is keyed on the *band*, not on a single filter, because GNIRS has two J and
 * two K filters: the spectroscopy order filters (Order5, Order3) that automatic
 * spectroscopic selection produces, and the photometric ones (J, K) that an imaging
 * science sequence uses. A matched row keeps the selected filter and only fixes the
 * exposure; every other filter (H2, L/M orders, Y, …) falls back to H.
 */
def firstStepFilterAndExposure(
  camera:         GnirsCamera,
  selectedFilter: GnirsFilter
): Either[String, (GnirsFilter, TimeSpan)] =
  // "Use H": image in H (Order4) at the camera's H exposure (short 3s, long 15s).
  val useH: (GnirsFilter, TimeSpan) =
    (GnirsFilter.Order4, keyholeExposureTime(camera))
  (selectedFilter, camera.pixelScale) match
    case (GnirsFilter.PAH, GnirsPixelScale.PixelScale_0_15)                    =>
      s"PAH acquisition filter cannot be used with short camera".asLeft
    case (GnirsFilter.Order6,                 GnirsPixelScale.PixelScale_0_15) => (selectedFilter, 10.secTimeSpan).asRight // X, short
    case (GnirsFilter.Order5 | GnirsFilter.J, GnirsPixelScale.PixelScale_0_15) => (selectedFilter, 15.secTimeSpan).asRight // J, short
    case (GnirsFilter.Order3 | GnirsFilter.K, GnirsPixelScale.PixelScale_0_15) => (selectedFilter,  3.secTimeSpan).asRight // K, short
    case (GnirsFilter.Order3 | GnirsFilter.K, GnirsPixelScale.PixelScale_0_05) => (selectedFilter, 15.secTimeSpan).asRight // K, long
    case (GnirsFilter.PAH,                    GnirsPixelScale.PixelScale_0_05) => (selectedFilter, 500.msTimeSpan).asRight // PAH, long
    case _                                                                     => useH.asRight // H, H2, long-camera X/J, L/M orders, Y, …
