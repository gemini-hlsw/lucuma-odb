// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.Eq
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.sequence.util.HashBytes

/**
 * GMOS imaging pairs a filter with an exposure time mode.
 */
final case class Filter[L](
  filter:           L,
  exposureTimeMode: ExposureTimeMode
)

object Filter:

  given [A](using Eq[A]): Eq[Filter[A]] =
    Eq.by(f => (f.filter, f.exposureTimeMode))

  given [A](using HashBytes[A]): HashBytes[Filter[A]] =
    HashBytes.by2(_.filter, _.exposureTimeMode)