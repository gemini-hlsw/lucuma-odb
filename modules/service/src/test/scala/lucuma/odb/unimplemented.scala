// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import lucuma.core.enums.ObservingModeType

val UnImplementedModes: Set[ObservingModeType] = Set(ObservingModeType.GnirsLongSlit)

extension (mode: ObservingModeType)
  def isImplemented: Boolean =
    !UnImplementedModes.contains(mode)
