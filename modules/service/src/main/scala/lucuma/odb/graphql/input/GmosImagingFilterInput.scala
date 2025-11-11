// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

case class GmosImagingFilterInput[L](
  filter:           L,
  exposureTimeMode: Option[ExposureTimeMode]
)

object GmosImagingFilterInput:

  val NorthBinding: Matcher[GmosImagingFilterInput[GmosNorthFilter]] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthFilterBinding("filter", rFilter),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
      ) =>
        (rFilter, rEtm).parMapN(apply)

  val SouthBinding: Matcher[GmosImagingFilterInput[GmosSouthFilter]] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthFilterBinding("filter", rFilter),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
      ) =>
        (rFilter, rEtm).parMapN(apply)