// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.SecondOrderOverlap
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig

/**
 * Finds the steps whose spectrum second order light may overlap.  A grating
 * sends second order light of wavelength λ to where first order light of 2λ
 * falls, so if the shortest wavelength reaching the grating is λmin, everything
 * past 2λmin is exposed to second order light.  λmin is the filter's cut-on or,
 * without a filter, `NoFilterBlueLimit`.  A step is affected when its
 * wavelength coverage reaches past 2λmin and the filter passes first order
 * light there.  (A band pass filter that ends before 2λmin leaves only second
 * order light past it.)
 *
 * Steps are checked as the sequence digest is computed, so generated and
 * edited sequences alike are covered.  Only science steps are reported.
 */
trait SecondOrderOverlapCheck[D]:
  def check(step: Step[D]): Option[SecondOrderOverlap]

object SecondOrderOverlapCheck:

  def apply[D](using ev: SecondOrderOverlapCheck[D]): SecondOrderOverlapCheck[D] =
    ev

  /**
   * The shortest wavelength reaching the grating when there is no filter.  With
   * no filter there is no sharp cut-on, so this is the approximate blue limit of
   * GMOS sensitivity: the point below which the atmosphere (ozone absorption),
   * the telescope and GMOS optics, and the detector quantum efficiency together
   * pass little light.  The science team chose 360 nm as a starting value and
   * may adjust it in the future (sc-10811).
   */
  val NoFilterBlueLimit: Wavelength =
    Wavelength.unsafeFromIntPicometers(360_000)

  /**
   * Where second order light starts, given the filter's transmission range if
   * any.  `None` when it falls beyond the longest representable wavelength.
   */
  def onset(filterRange: Option[BoundedInterval[Wavelength]]): Option[Wavelength] =
    val pm = filterRange.fold(NoFilterBlueLimit)(_.lower).pm.value.value.toLong * 2
    Option.when(pm.isValidInt)(pm.toInt).flatMap(Wavelength.fromIntPicometers)

  /**
   * The overlap, if any, for a spectrum of the given coverage centered at
   * `centralWavelength` behind a filter with the given transmission range.
   */
  def overlap(
    coverage:          WavelengthDelta,
    centralWavelength: Wavelength,
    filterRange:       Option[BoundedInterval[Wavelength]]
  ): Option[SecondOrderOverlap] =
    onset(filterRange)
      .filter(o => filterRange.forall(o < _.upper) && o < coverage.centeredAt(centralWavelength).upper)
      .map(SecondOrderOverlap(_, centralWavelength))

  private def science[D](step: Step[D])(f: D => Option[SecondOrderOverlap]): Option[SecondOrderOverlap] =
    step.stepConfig match
      case StepConfig.Science => f(step.instrumentConfig)
      case _                  => none

  private def never[D]: SecondOrderOverlapCheck[D] =
    _ => none

  given SecondOrderOverlapCheck[DynamicConfig.GmosNorth] = step =>
    science(step): d =>
      d.gratingConfig.flatMap(g => overlap(g.grating.simultaneousCoverage, g.wavelength, d.filter.map(_.width)))

  given SecondOrderOverlapCheck[DynamicConfig.GmosSouth] = step =>
    science(step): d =>
      d.gratingConfig.flatMap(g => overlap(g.grating.simultaneousCoverage, g.wavelength, d.filter.map(_.width)))

  given SecondOrderOverlapCheck[Flamingos2DynamicConfig] = never
  given SecondOrderOverlapCheck[GhostDynamicConfig]      = never
  given SecondOrderOverlapCheck[GnirsDynamicConfig]      = never
  given SecondOrderOverlapCheck[Igrins2DynamicConfig]    = never
