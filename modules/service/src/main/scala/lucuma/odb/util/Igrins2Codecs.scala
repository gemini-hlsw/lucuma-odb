// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.Igrins2OffsetMode
import skunk.*
import skunk.data.Type

trait Igrins2Codecs:

  import Codecs.enumerated

  val igrins_2_offset_mode: Codec[Igrins2OffsetMode] =
    enumerated(Type.varchar)

object Igrins2Codecs extends Igrins2Codecs
