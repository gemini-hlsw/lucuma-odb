// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.core.model.CloudExtinction
import lucuma.core.util.NewType

object CloudExtinctionInput extends NewType[Either[CloudExtinction.Preset, BigDecimal]]:
  def preset(p:     CloudExtinction.Preset): CloudExtinctionInput = CloudExtinctionInput(Left(p))
  def extinction(e: BigDecimal): CloudExtinctionInput             = CloudExtinctionInput(Right(e))

type CloudExtinctionInput = CloudExtinctionInput.Type
