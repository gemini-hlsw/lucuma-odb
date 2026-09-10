// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.syntax.all.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.util.Enumerated
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcResult
import lucuma.odb.data.ItcScience.Flamingos2Imaging
import lucuma.odb.data.ItcScience.GhostIfu
import lucuma.odb.data.ItcScience.GmosNorthImaging
import lucuma.odb.data.ItcScience.GmosSouthImaging
import lucuma.odb.data.ItcScience.GnirsImaging
import lucuma.odb.data.ItcScience.GnirsSpectroscopy
import lucuma.odb.data.ItcScience.Spectroscopy
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.gnirs.wavelengthOccurrences
import lucuma.odb.sequence.gnirs.withOccurrence

// warn if < 3
case class TotalSignalToNoiseValidator(itcFor: Observation.Id => Option[Itc]) extends ObservationValidator:
  import TotalSignalToNoiseValidator.*
  
  def warningsForZipper(zr: Zipper[ItcResult], extra: Option[String] = None): ObservationValidationMap =
    zr.focus
      .signalToNoise
      .map(_.total)
      .filter(_ < MinRecommended)
      .map(warning(extra, _))
      .foldMap(ObservationValidationMap.singleton)

  def warningsForKeyed[A](keyed: NonEmptyList[(A, Zipper[ItcResult])])(f: A => Option[String]): ObservationValidationMap =
    keyed.foldMap: (a, z) =>
      warningsForZipper(z, f(a))

  def warningsForMap[A](map: NonEmptyMap[A, Zipper[ItcResult]])(f: A => Option[String]): ObservationValidationMap =
    warningsForKeyed(map.toNel)(f)

  def warningsForMap[A](map: NonEmptyMap[A, Zipper[ItcResult]])(using e: Enumerated[A]): ObservationValidationMap =
    warningsForMap(map)(e.tag(_).some)

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    itcFor(info.oid).foldMap: itc =>
      itc.science match
        case Flamingos2Imaging(science) => warningsForMap(science)
        case GhostIfu(red, blue)        => warningsForZipper(red, "red".some) |+| warningsForZipper(blue, "blue".some)
        case GmosNorthImaging(science)  => warningsForMap(science)
        case GmosSouthImaging(science)  => warningsForMap(science)
        case GnirsImaging(science)      => warningsForMap(science)
        case Spectroscopy(science)      => warningsForZipper(science)
        case GnirsSpectroscopy(science) => warningsForKeyed(gnirsSpectroscopyLabels(science.map(_._1)).zipWith(science)((l, r) => (l, r._2)))(_.some)
 
object TotalSignalToNoiseValidator:

  val MinRecommended = TotalSN(SignalToNoise.unsafeFromBigDecimalExact(3))

  /**
   * Names each GNIRS spectroscopy central wavelength for the warning that may refer to
   * it, in list order.
   *
   * A repeated central wavelength is an independent configuration with its own exposure
   * time mode, coadds and ITC result, so the wavelength alone does not say which one
   * fell short.  The ordinal comes from `gnirs.wavelengthOccurrences`, shared with the
   * sequence atom titles and the GNIRS configuration checks, so a warning and the
   * sequence segment it refers to name the configuration the same way.
   */
  private[validator] def gnirsSpectroscopyLabels(
    ws: NonEmptyList[Wavelength]
  ): NonEmptyList[String] =
    def nm(w: Wavelength): String =
      f"${Wavelength.decimalNanometers.reverseGet(w)}%4.3f nm"

    val occurrences = wavelengthOccurrences(ws.toList)
    ws.zipWithIndex.map: (w, i) =>
      withOccurrence(nm(w), occurrences(i))

  def warning(extra: Option[String], actual: TotalSN): ObservationValidation =
    ObservationValidation.Warning.lowTotalSignalToNoise(extra, MinRecommended, actual)

