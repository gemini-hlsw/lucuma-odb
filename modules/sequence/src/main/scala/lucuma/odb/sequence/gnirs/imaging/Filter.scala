// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.sequence.util.HashBytes

case class Filter(
  filter:           GnirsFilter,
  exposureTimeMode: ExposureTimeMode
) derives Eq

object Filter:

  given HashBytes[Filter] =
    HashBytes.by2(_.filter, _.exposureTimeMode)
