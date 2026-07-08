// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

case class GnirsImagingFilterInput(
  filter:           GnirsFilter,
  exposureTimeMode: Option[ExposureTimeMode]
)

object GnirsImagingFilterInput:

  val Binding: Matcher[GnirsImagingFilterInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GnirsFilterBinding("filter", rFilter),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
      ) =>
        (rFilter, rEtm).parMapN(apply)
