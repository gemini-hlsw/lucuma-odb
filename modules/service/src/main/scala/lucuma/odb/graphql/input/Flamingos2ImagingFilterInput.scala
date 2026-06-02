// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*

case class Flamingos2ImagingFilterInput(
  filter:           Flamingos2Filter,
  exposureTimeMode: Option[ExposureTimeMode]
)

object Flamingos2ImagingFilterInput:

  val Binding: Matcher[Flamingos2ImagingFilterInput] =
    ObjectFieldsBinding.rmap:
      case List(
        Flamingos2FilterBinding("filter", rFilter),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rEtm)
      ) =>
        (rFilter, rEtm).parMapN(apply)
