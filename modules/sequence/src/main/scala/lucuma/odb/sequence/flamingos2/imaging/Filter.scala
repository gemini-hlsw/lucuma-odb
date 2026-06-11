// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.imaging

import cats.Eq
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.model.ExposureTimeMode

/**
 * Flamingos2 imaging pairs a filter with an exposure time mode.
 */
final case class Filter(
  filter:           Flamingos2Filter,
  exposureTimeMode: ExposureTimeMode
)

object Filter:

  given Eq[Filter] =
    Eq.by(f => (f.filter, f.exposureTimeMode))
