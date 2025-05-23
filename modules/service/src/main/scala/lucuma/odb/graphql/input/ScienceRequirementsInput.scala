// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ScienceRequirementsInput(
  exposureTimeMode: Nullable[ExposureTimeMode],
  spectroscopy:     Option[SpectroscopyScienceRequirementsInput],
  imaging:          Option[ImagingScienceRequirementsInput]
) {
  require(!(spectroscopy.isDefined && imaging.isDefined))
  def scienceMode: Option[ScienceMode] =
    (spectroscopy, imaging) match {
      case (Some(_), None) => ScienceMode.Spectroscopy.some
      case (None, Some(_)) => ScienceMode.Imaging.some
      case _               => none
    }
}

object ScienceRequirementsInput:

  val Binding: Matcher[ScienceRequirementsInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ExposureTimeModeInput.Binding.Nullable("exposureTimeMode", rExposureTimeMode),
        SpectroscopyScienceRequirementsInput.Binding.Option("spectroscopy", rSpectroscopy),
        ImagingScienceRequirementsInput.Binding.Option("imaging", rImaging)
      ) =>
        (rExposureTimeMode, rSpectroscopy, rImaging).parTupled.flatMap:
          case (expTimeMode, spec, img) =>
            atMostOne(
              spec -> "spectroscopy",
              img  -> "imaging"
            ).as(ScienceRequirementsInput(expTimeMode, spec, img))

