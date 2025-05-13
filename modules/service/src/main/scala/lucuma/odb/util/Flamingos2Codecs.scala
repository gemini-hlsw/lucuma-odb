// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import cats.syntax.functor.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.varchar
import skunk.data.Type

trait Flamingos2Codecs:

  import Codecs.enumerated
  import Codecs.time_span

  val f2_custom_slit_width: Codec[Flamingos2CustomSlitWidth] =
    enumerated(Type.varchar)

  val f2_decker: Codec[Flamingos2Decker] =
    enumerated(Type.varchar)

  val f2_disperser: Codec[Flamingos2Disperser] =
    enumerated(Type.varchar)

  val f2_filter: Codec[Flamingos2Filter] =
    enumerated(Type.varchar)

  val f2_fpu: Codec[Flamingos2Fpu] =
    enumerated(Type.varchar)

  val f2_lyot_wheel: Codec[Flamingos2LyotWheel] =
    enumerated(Type.varchar)

  val f2_read_mode: Codec[Flamingos2ReadMode] =
    enumerated(Type.varchar)

  val f2_readout_mode: Codec[Flamingos2ReadoutMode] =
    enumerated(Type.varchar)

  val f2_reads: Codec[Flamingos2Reads] =
    enumerated(Type.varchar)

  val flamingos_2_static: Codec[Flamingos2StaticConfig] =
    (
      Codecs.mos_pre_imaging *:
      bool
    ).imap { case (m, e) => Flamingos2StaticConfig(m, e) } { f2 => (
      f2.mosPreImaging,
      f2.useElectronicOffseting
    )}

  val f2_fpu_mask_custom: Codec[Flamingos2FpuMask.Custom] =
    (varchar *: f2_custom_slit_width).eimap { case (n, w) =>
      NonEmptyString.from(n).leftMap(_ => "Custom mask filename cannot be empty").map { ne =>
        Flamingos2FpuMask.Custom(ne, w)
      }
    } { c => (c.filename.value, c.slitWidth)}

  val f2_fpu_mask: Codec[Flamingos2FpuMask] =
     (f2_fpu_mask_custom.opt *: f2_fpu.opt).eimap {
       case (None, None)       => Flamingos2FpuMask.Imaging.asRight.widen[Flamingos2FpuMask]
       case (Some(c), None)    => c.asRight.widen[Flamingos2FpuMask]
       case (None, Some(b))    => Flamingos2FpuMask.Builtin(b).asRight.widen[Flamingos2FpuMask]
       case (Some(_), Some(_)) => Left("Both custom and builtin mask options are defined")
     } { m => (m.custom, m.builtinFpu) }

  val flamingos_2_dynamic: Codec[Flamingos2DynamicConfig] =
    (
      time_span           *:
      f2_disperser.opt    *:
      f2_filter           *:
      f2_read_mode        *:
      f2_lyot_wheel       *:
      f2_fpu_mask         *:
      f2_readout_mode.opt *:
      f2_reads.opt
    ).to[Flamingos2DynamicConfig]

object Flamingos2Codecs extends Flamingos2Codecs
