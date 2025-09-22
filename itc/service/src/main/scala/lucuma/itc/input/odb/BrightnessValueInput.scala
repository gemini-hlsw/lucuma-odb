// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input.sourceprofile

import lucuma.core.math.BrightnessValue
import lucuma.odb.graphql.binding.BigDecimalBinding
import lucuma.odb.graphql.binding.Matcher

val BrightnessValueBinding: Matcher[BrightnessValue] =
  BigDecimalBinding.emap(BrightnessValue.from)
