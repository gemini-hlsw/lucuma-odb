// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import skunk.*
import skunk.data.Type

trait F2Codecs:

  import Codecs.enumerated

  val f2_disperser: Codec[F2Disperser] =
    enumerated(Type.varchar)

  val f2_fpu: Codec[F2Fpu] =
    enumerated(Type.varchar)

  val f2_filter: Codec[F2Filter] =
    enumerated(Type.varchar)

object F2Codecs extends F2Codecs
