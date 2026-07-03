// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import lucuma.core.enums.ObservingModeType

// Modes that the generic, mode-iterating test harnesses (clone, configuration
// requests, ...) do not exercise.  The exchange modes are excluded because they
// require a matching exchange proposal and do not participate in configuration
// requests; they are covered by their own dedicated suites instead.
val UnImplementedModes: Set[ObservingModeType] =
  Set(
    ObservingModeType.ExchangeKeck,
    ObservingModeType.ExchangeSubaru,
    ObservingModeType.Flamingos2Imaging,
    ObservingModeType.GnirsLongSlit,
    ObservingModeType.GnirsIfu
  )

extension (mode: ObservingModeType)
  def isImplemented: Boolean =
    !UnImplementedModes.contains(mode)
