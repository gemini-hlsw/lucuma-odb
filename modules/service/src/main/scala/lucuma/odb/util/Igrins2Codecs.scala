// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import skunk.*
import skunk.codec.boolean.bool
import skunk.data.Type

trait Igrins2Codecs:

  import Codecs.enumerated
  import Codecs.time_span

  val igrins_2_offset_mode: Codec[Igrins2OffsetMode] =
    enumerated(Type.varchar)

  val igrins_2_dynamic: Codec[Igrins2DynamicConfig] =
    time_span.imap(Igrins2DynamicConfig(_))(_.exposure)

  val igrins_2_static: Codec[Igrins2StaticConfig] =
    (bool *: igrins_2_offset_mode).imap { case (svc, off) =>
      Igrins2StaticConfig(Igrins2SVCImages(svc), off)
    } { cfg => (cfg.saveSVCImages.value, cfg.offsetMode) }

object Igrins2Codecs extends Igrins2Codecs
