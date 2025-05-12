// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.model.sequence.f2.F2StaticConfig
import skunk.*
import skunk.codec.boolean.bool
import skunk.data.Type

trait Flamingos2Codecs:

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

  val flamingos_2_static: Codec[F2StaticConfig] =
    (
      Codecs.mos_pre_imaging *:
      bool
    ).imap { case (m, e) => F2StaticConfig(m, e) } { f2 => (
      f2.mosPreImaging,
      f2.useElectronicOffseting
    )}

object Flamingos2Codecs extends Flamingos2Codecs
