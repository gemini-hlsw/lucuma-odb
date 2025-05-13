// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.F2CustomSlitWidth
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2ReadoutMode
import lucuma.core.enums.F2Reads
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.model.sequence.f2.F2FpuMask
import lucuma.core.model.sequence.f2.F2StaticConfig
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.varchar
import skunk.data.Type

trait Flamingos2Codecs:

  import Codecs.enumerated
  import Codecs.time_span

  val f2_custom_slit_width: Codec[F2CustomSlitWidth] =
    enumerated(Type.varchar)

  val f2_decker: Codec[F2Decker] =
    enumerated(Type.varchar)

  val f2_disperser: Codec[F2Disperser] =
    enumerated(Type.varchar)

  val f2_filter: Codec[F2Filter] =
    enumerated(Type.varchar)

  val f2_fpu: Codec[F2Fpu] =
    enumerated(Type.varchar)

  val f2_lyot_wheel: Codec[F2LyotWheel] =
    enumerated(Type.varchar)

  val f2_read_mode: Codec[F2ReadMode] =
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

  val f2_fpu_mask_custom: Codec[F2FpuMask.Custom] =
    (varchar *: f2_custom_slit_width).eimap { case (n, w) =>
      NonEmptyString.from(n).leftMap(_ => "Custom mask filename cannot be empty").map { ne =>
        F2FpuMask.Custom(ne, w)
      }
    } { c => (c.filename.value, c.slitWidth)}

  val f2_fpu_mask: Codec[F2FpuMask] =
     (f2_fpu_mask_custom.opt *: f2_fpu.opt).eimap {
       case (None, None)       => F2FpuMask.Imaging.asRight.widen[F2FpuMask]
       case (Some(c), None)    => c.asRight.widen[F2FpuMask]
       case (None, Some(b))    => F2FpuMask.Builtin(b).asRight.widen[F2FpuMask]
       case (Some(_), Some(_)) => Left("Both custom and builtin mask options are defined")
     } { m => (m.custom, m.builtinFpu) }

  val flamingos_2_dynamic: Codec[F2DynamicConfig] =
    (
      time_span           *:
      f2_disperser.opt    *:
      f2_filter           *:
      f2_read_mode        *:
      f2_lyot_wheel       *:
      f2_fpu_mask         *:
      f2_readout_mode.opt *:
      f2_reads.opt
    ).to[F2DynamicConfig]

object Flamingos2Codecs extends Flamingos2Codecs
