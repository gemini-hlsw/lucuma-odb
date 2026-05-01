// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import skunk.Codec
import skunk.data.Type

trait GnirsCodecs:

  import Codecs.enumerated

  val gnirs_filter: Codec[GnirsFilter] =
    enumerated(Type.varchar)

  val gnirs_fpu_slit: Codec[GnirsFpuSlit] =
    enumerated(Type.varchar)

  val gnirs_grating: Codec[GnirsGrating] =
    enumerated(Type.varchar)

object GnirsCodecs extends GnirsCodecs
