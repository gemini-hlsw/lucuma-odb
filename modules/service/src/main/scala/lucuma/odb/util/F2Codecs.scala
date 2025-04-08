// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import skunk.*
import skunk.data.Type
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2WindowCover
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads

trait F2Codecs:

  import Codecs.enumerated

  val f2_disperser: Codec[F2Disperser] =
    enumerated(Type.varchar)

  val f2_fpu: Codec[F2Fpu] =
    enumerated(Type.varchar)

  val f2_filter: Codec[F2Filter] =
    enumerated(Type.varchar)

  val f2_read_mode: Codec[F2ReadMode] =
    enumerated(Type.varchar)

  val f2_decker: Codec[F2Decker] =
    enumerated(Type.varchar)

  val f2_readout_mode: Codec[F2ReadoutMode] =
    enumerated(Type.varchar)

  val f2_reads: Codec[F2Reads] =
    enumerated(Type.varchar)

  val f2_window_cover: Codec[F2WindowCover] =
    enumerated(Type.varchar)

object F2Codecs extends F2Codecs
