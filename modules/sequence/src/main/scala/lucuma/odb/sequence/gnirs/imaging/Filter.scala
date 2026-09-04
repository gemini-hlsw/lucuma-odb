// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs.imaging

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

/**
 * One GNIRS imaging science configuration: a filter together with the exposure
 * time mode and coadds that apply to it.  Each is a separate ITC calculation.
 */
case class Filter(
  filter:           GnirsFilter,
  exposureTimeMode: ExposureTimeMode,
  coadds:           PosInt
) derives Eq

object Filter:

  given HashBytes[Filter] with
    def hashBytes(a: Filter): Array[Byte] =
      Array.concat(
        a.filter.hashBytes,
        a.exposureTimeMode.hashBytes,
        a.coadds.value.hashBytes
      )
