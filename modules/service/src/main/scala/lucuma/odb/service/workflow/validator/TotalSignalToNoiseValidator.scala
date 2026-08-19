// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyMap
import cats.syntax.all.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.math.TotalSN
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
import lucuma.odb.data.ItcScience.Spectroscopy
import lucuma.odb.data.ObservationValidationMap

// warn if < 3
case class TotalSignalToNoiseValidator(itcFor: Observation.Id => Option[Itc]) extends ObservationValidator:
  
  val MinRecommended = TotalSN(SignalToNoise.unsafeFromBigDecimalExact(3))

  def warningsForZipper(zr: Zipper[ItcResult], extra: Option[String] = None): ObservationValidationMap =
    zr.focus
      .signalToNoise
      .map(_.total)
      .filter(_ < MinRecommended)
      .foldMap: sn =>
        val msg = s"Total S/N ${extra.foldMap(s => s"($s) ")} is $sn (min. 3 recommended)"
        ObservationValidationMap.singleton(ObservationValidation.genericWaning(msg))

  def warningsForMap[A](map: NonEmptyMap[A, Zipper[ItcResult]])(using e: Enumerated[A]): ObservationValidationMap =
    map.toNel.foldMap: (a, z) =>
      warningsForZipper(z, e.tag(a).some)

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    itcFor(info.oid).foldMap: itc =>
      itc.science match
        case Flamingos2Imaging(science) => warningsForMap(science)
        case GhostIfu(red, blue)        => warningsForZipper(red, "red".some) |+| warningsForZipper(blue, "blue".some)
        case GmosNorthImaging(science)  => warningsForMap(science)
        case GmosSouthImaging(science)  => warningsForMap(science)
        case GnirsImaging(science)      => warningsForMap(science)
        case Spectroscopy(science)      => warningsForZipper(science)
 
