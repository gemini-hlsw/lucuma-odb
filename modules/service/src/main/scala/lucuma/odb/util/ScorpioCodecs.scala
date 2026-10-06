// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioFpu
import skunk.Codec
import skunk.data.Type

trait ScorpioCodecs:

  import Codecs.enumerated

  val scorpio_filter: Codec[ScorpioFilter] =
    enumerated(Type.varchar)

  val scorpio_fpu: Codec[ScorpioFpu] =
    enumerated(Type.varchar)

object ScorpioCodecs extends ScorpioCodecs
