// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.igrins2.longslit

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.enums.Igrins2SlitOffsetPreset
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.*
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class Config(
  scienceExposureTimeMode: ExposureTimeMode,
  svc: Option[Config.Svc],
  telescopeConfigs: SlitTelescopeConfigs,
  telluricType: TelluricType
) derives Eq:

  /** The telescope configs flattened for step generation. */
  def steps: NonEmptyList[TelescopeConfig] =
    telescopeConfigs.telescopeConfigs

  /**
   * The offset preset recorded in the instrument static config, derived from the shape of
   * the telescope configs.
   */
  def offsetPreset: Igrins2SlitOffsetPreset =
    telescopeConfigs.offsetsType match
      case SlitOffsetMode.NodAlongSlit => Igrins2SlitOffsetPreset.NodAlongSlit
      case SlitOffsetMode.NodToSky     => Igrins2SlitOffsetPreset.NodToSky

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(256)
    val out = new DataOutputStream(bao)

    out.write(scienceExposureTimeMode.hashBytes)
    // Must stay byte-identical to the old boolean-false encoding when svc is absent, so that
    // no existing observation's hash changes. See docs/adr/igrins2-svc-acquisition-generation.md.
    svc match
      case None      =>
        out.writeBoolean(false)
      case Some(cfg) =>
        out.writeBoolean(true)
        out.write(cfg.exposure.hashBytes)
        out.write(cfg.telescopeConfigs.hashBytes)
    out.writeChars(telescopeConfigs.offsetsType.tag)
    steps.toList.foreach: tc =>
      out.write(tc.hashBytes)
    out.write(telluricType.hashBytes)

    out.close()
    bao.toByteArray

object Config:

  /** The SVC (Slit-Viewing Camera) acquisition sub-config, with already-resolved effective values. */
  case class Svc(
    exposure: TimeSpan,
    telescopeConfigs: NonEmptyList[TelescopeConfig]
  ) derives Eq

  /** The default telescope configs — the nod-along-slit ABBA pattern. */
  val DefaultTelescopeConfigs: SlitTelescopeConfigs =
    defaultSlitTelescopeConfigs(Igrins2SlitOffsetPreset.NodAlongSlit)
