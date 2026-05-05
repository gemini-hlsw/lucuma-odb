// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import skunk.Codec
import skunk.data.Type

trait GnirsCodecs:

  import Codecs.enumerated

  val gnirs_camera: Codec[GnirsCamera] =
    enumerated(Type.varchar)

  val gnirs_filter: Codec[GnirsFilter] =
    enumerated(Type.varchar)

  val gnirs_fpu_slit: Codec[GnirsFpuSlit] =
    enumerated(Type.varchar)

  val gnirs_grating: Codec[GnirsGrating] =
    enumerated(Type.varchar)

  val gnirs_prism: Codec[GnirsPrism] =
    enumerated(Type.varchar)

object GnirsCodecs extends GnirsCodecs
