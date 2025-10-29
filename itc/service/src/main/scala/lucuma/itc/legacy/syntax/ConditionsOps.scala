// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality

trait ConditionsSyntax:
  extension (self: ImageQuality.Preset)
    def ocs2Tag: String =
      self match
        case ImageQuality.Preset.PointOne     => "PERCENT_20"
        case ImageQuality.Preset.PointTwo     => "PERCENT_20"
        case ImageQuality.Preset.PointThree   => "PERCENT_20"
        case ImageQuality.Preset.PointFour    => "PERCENT_20"
        case ImageQuality.Preset.PointSix     => "PERCENT_70"
        case ImageQuality.Preset.PointEight   => "PERCENT_70"
        case ImageQuality.Preset.OnePointZero => "PERCENT_85"
        case ImageQuality.Preset.OnePointTwo  => "PERCENT_85"
        case ImageQuality.Preset.OnePointFive => "ANY"
        case ImageQuality.Preset.TwoPointZero => "ANY"

  extension (self: CloudExtinction.Preset)
    def ocs2Tag: String =
      self match
        case CloudExtinction.Preset.PointOne | CloudExtinction.Preset.Zero               => "PERCENT_50"
        case CloudExtinction.Preset.PointThree                                           => "PERCENT_70"
        case CloudExtinction.Preset.PointFive | CloudExtinction.Preset.OnePointZero      => "PERCENT_80"
        case CloudExtinction.Preset.TwoPointZero | CloudExtinction.Preset.ThreePointZero =>
          "ANY"

  extension (self: WaterVapor)
    def ocs2Tag: String =
      self match
        case WaterVapor.VeryDry => "PERCENT_20"
        case WaterVapor.Dry     => "PERCENT_50"
        case WaterVapor.Median  => "PERCENT_80"
        case WaterVapor.Wet     => "ANY"

  extension (self: SkyBackground)
    def ocs2Tag: String =
      self match
        case SkyBackground.Darkest => "PERCENT_20"
        case SkyBackground.Dark    => "PERCENT_50"
        case SkyBackground.Gray    => "PERCENT_80"
        case SkyBackground.Bright  => "ANY"

object conditions extends ConditionsSyntax
