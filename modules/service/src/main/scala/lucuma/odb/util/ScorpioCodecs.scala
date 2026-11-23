// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.ScorpioChannelSelection
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.model.sequence.scorpio.ScorpioChannelConfig
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioFpuMask
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import skunk.Codec
import skunk.data.Type

trait ScorpioCodecs:

  import Codecs.enumerated
  import Codecs.int4_pos
  import Codecs.time_span

  val scorpio_channel_selection: Codec[ScorpioChannelSelection] =
    enumerated(Type("e_scorpio_channel_selection"))

  val scorpio_disperser: Codec[ScorpioDisperser] =
    enumerated(Type("e_scorpio_disperser"))

  val scorpio_filter: Codec[ScorpioFilter] =
    enumerated(Type.varchar)

  val scorpio_fpu: Codec[ScorpioFpu] =
    enumerated(Type.varchar)

  val scorpio_vis_readout_mode: Codec[ScorpioVisReadoutMode] =
    enumerated(Type("e_scorpio_vis_readout_mode"))

  val scorpio_vis_channel: Codec[Option[ScorpioChannelConfig.Vis]] =
    (
      time_span.opt *:
      int4_pos.opt  *:
      scorpio_vis_readout_mode.opt
    ).eimap {
      case (Some(t), Some(n), Some(m)) => ScorpioChannelConfig.Vis(t, n, m).some.asRight
      case (None, None, None)          => none.asRight
      case other                       => s"Inconsistent SCORPIO channel: $other".asLeft
    }(c => (c.map(_.exposureTime), c.map(_.exposureCount), c.map(_.readoutMode)))

  val scorpio_static: Codec[ScorpioStaticConfig] =
    scorpio_channel_selection.imap(ScorpioStaticConfig(_))(_.channelSelection)

  // The IR channels are not stored, and a step that is not long slit is
  // rejected by the NOT NULL constraint on c_fpu.
  val scorpio_dynamic: Codec[ScorpioDynamicConfig] =
    (
      scorpio_vis_channel *:
      scorpio_vis_channel *:
      scorpio_vis_channel *:
      scorpio_vis_channel *:
      scorpio_fpu.opt     *:
      scorpio_disperser
    ).eimap { (g, r, i, z, fpu, disperser) =>
      fpu
        .toRight("Missing SCORPIO slit")
        .map(f => ScorpioDynamicConfig(g, r, i, z, none, none, none, none, ScorpioFpuMask.LongSlit(f), disperser))
    }(d => (d.g, d.r, d.i, d.z, d.fpu.longSlit, d.disperser))

object ScorpioCodecs extends ScorpioCodecs
