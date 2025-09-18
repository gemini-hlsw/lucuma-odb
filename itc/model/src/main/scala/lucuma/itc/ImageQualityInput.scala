// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.core.model.ImageQuality
import lucuma.core.util.NewType

object ImageQualityInput extends NewType[Either[ImageQuality.Preset, BigDecimal]]:
  def preset(p: ImageQuality.Preset): ImageQualityInput = ImageQualityInput(Left(p))
  def arcsec(a: BigDecimal): ImageQualityInput          = ImageQualityInput(Right(a))

type ImageQualityInput = ImageQualityInput.Type
