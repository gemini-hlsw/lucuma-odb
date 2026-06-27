// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.syntax.either.*
import cats.syntax.option.*
import coulomb.syntax.*
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFocus
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStep
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import skunk.Codec
import skunk.codec.numeric.int4
import skunk.data.Type

trait GnirsCodecs:

  import Codecs.enumerated
  import Codecs.int4_pos
  import Codecs.time_span
  import Codecs.wavelength_pm

  val gnirs_camera: Codec[GnirsCamera] =
    enumerated(Type.varchar)

  val gnirs_decker: Codec[GnirsDecker] =
    enumerated(Type("e_gnirs_decker"))

  val gnirs_filter: Codec[GnirsFilter] =
    enumerated(Type.varchar)

  val gnirs_fpu_slit: Codec[GnirsFpuSlit] =
    enumerated(Type.varchar)

  val gnirs_fpu_ifu: Codec[GnirsFpuIfu] =
    enumerated(Type("e_gnirs_fpu_ifu"))

  val gnirs_fpu_other: Codec[GnirsFpuOther] =
    enumerated(Type("e_gnirs_fpu_other"))

  val gnirs_grating: Codec[GnirsGrating] =
    enumerated(Type.varchar)

  val gnirs_pixel_scale: Codec[GnirsPixelScale] =
    enumerated(Type.varchar)

  val gnirs_prism: Codec[GnirsPrism] =
    enumerated(Type.varchar)

  val gnirs_read_mode: Codec[GnirsReadMode] =
    enumerated(Type.varchar)

  val gnirs_acquisition_type: Codec[GnirsAcquisitionType] =
    enumerated(Type("e_gnirs_acquisition_type"))

  val gnirs_well_depth: Codec[GnirsWellDepth] =
    enumerated(Type("e_gnirs_well_depth"))

  val gnirs_grating_wavelength: Codec[GnirsGratingWavelength] =
    wavelength_pm.imap(GnirsGratingWavelength(_))(_.value)

  val gnirs_static: Codec[GnirsStaticConfig] =
    gnirs_well_depth.imap(GnirsStaticConfig(_))(_.wellDepth)

  // Spectroscopy FPU: exactly one of the long-slit slit value or the IFU value.
  val gnirs_fpu_spectroscopy: Codec[GnirsFpu.Spectroscopy] =
    (gnirs_fpu_slit.opt *: gnirs_fpu_ifu.opt).eimap {
      case (Some(s), None) => GnirsFpu.Spectroscopy.Slit(s).asRight[String]
      case (None, Some(i)) => GnirsFpu.Spectroscopy.Ifu(i).asRight[String]
      case (None, None)    => "GNIRS spectroscopy FPU: neither slit nor ifu defined".asLeft
      case _               => "GNIRS spectroscopy FPU: both slit and ifu defined".asLeft
    } { fpu =>
      (GnirsFpu.Spectroscopy.slit.getOption(fpu), GnirsFpu.Spectroscopy.ifu.getOption(fpu))
    }

  // FPU: exactly one of the long-slit slit value, the IFU value or the non-slit
  // "other" value.
  private val gnirs_fpu: Codec[GnirsFpu] =
    (gnirs_fpu_slit.opt *: gnirs_fpu_other.opt *: gnirs_fpu_ifu.opt).eimap {
      case (Some(s), None,    None)    => GnirsFpu.Spectroscopy.Slit(s).asRight[String]
      case (None,    Some(o), None)    => GnirsFpu.Other(o).asRight[String]
      case (None,    None,    Some(i)) => GnirsFpu.Spectroscopy.Ifu(i).asRight[String]
      case (None,    None,    None)    => "GNIRS FPU: no slit, ifu or other defined".asLeft
      case _                           => "GNIRS FPU: more than one of slit, ifu or other defined".asLeft
    } { fpu =>
      (GnirsFpu.slit.getOption(fpu), GnirsFpu.other.getOption(fpu), GnirsFpu.ifu.getOption(fpu))
    }

  // Spectroscopy config carried when the acquisition mirror is "out". The
  // three values are present together (mirror out) or absent together (in).
  private val gnirs_acquisition_mirror_mode: Codec[GnirsAcquisitionMirrorMode] =
    (gnirs_prism.opt *: gnirs_grating.opt *: gnirs_grating_wavelength.opt).eimap {
      case (Some(p), Some(g), Some(w)) => GnirsAcquisitionMirrorMode.Out(p, g, w).asRight
      case (None,    None,    None)    => GnirsAcquisitionMirrorMode.In.asRight
      case _                           => "GNIRS acquisition mirror: prism, grating and wavelength must all be present or all absent".asLeft
    } {
      case GnirsAcquisitionMirrorMode.Out(p, g, w) => (p.some, g.some, w.some)
      case GnirsAcquisitionMirrorMode.In           => (none, none, none)
    }

  // Focus motor steps. Absent means "Best" (instrument-chosen) focus.
  private val gnirs_focus: Codec[GnirsFocus] =
    int4.opt.eimap[GnirsFocus] {
      case None    => GnirsFocus.Best.asRight
      case Some(i) => GnirsFocusMotorStepsValue.from(i).map(v => GnirsFocus.Custom(v.withUnit[GnirsFocusMotorStep]))
    } {
      case GnirsFocus.Best      => none
      case GnirsFocus.Custom(q) => q.value.value.value.some
    }

  val gnirs_dynamic: Codec[GnirsDynamicConfig] =
    (
      time_span                     *:
      int4_pos                      *:
      gnirs_filter                  *:
      gnirs_decker                  *:
      gnirs_fpu                     *:
      gnirs_acquisition_mirror_mode *:
      gnirs_camera                  *:
      gnirs_focus                   *:
      gnirs_read_mode
    ).to[GnirsDynamicConfig]

object GnirsCodecs extends GnirsCodecs
