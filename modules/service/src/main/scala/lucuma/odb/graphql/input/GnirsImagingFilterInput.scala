// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

case class GnirsImagingFilterInput(
  filter:           GnirsFilter,
  exposureTimeMode: Option[ExposureTimeMode],
  coadds:           Option[PosInt]
)

object GnirsImagingFilterInput:

  val Binding: Matcher[GnirsImagingFilterInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsFilterBinding("filter", rFilter),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm),
        PosIntBinding.Option("coadds", rCoadds)
      ) =>
        (rFilter, rEtm, rCoadds).parMapN: (filter, etm, coadds) =>
          GnirsImagingFilterInput(filter, etm, coaddsForEtm(etm, coadds))

  /**
   * Signal-to-noise exposure time mode does not support coadds.  When the ETM is
   * set to signal-to-noise, force coadds to 1 so a previously-set value doesn't
   * linger.
   */
  private def coaddsForEtm(
    etm:    Option[ExposureTimeMode],
    coadds: Option[PosInt]
  ): Option[PosInt] =
    etm match
      case Some(ExposureTimeMode.SignalToNoiseMode(_, _)) => PosInt.from(1).toOption
      case _                                              => coadds
