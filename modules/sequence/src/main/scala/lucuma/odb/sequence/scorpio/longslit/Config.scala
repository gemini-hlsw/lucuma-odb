// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.scorpio.longslit

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.ScorpioSlitOffsetPreset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.scorpio.defaultSlitTelescopeConfigs
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

/** Only the visible arm is configured, one exposure time mode per channel. */
case class Config(
  fpu:              ScorpioFpu,
  g:                ExposureTimeMode,
  r:                ExposureTimeMode,
  i:                ExposureTimeMode,
  z:                ExposureTimeMode,
  telescopeConfigs: SlitTelescopeConfigs
) derives Eq:

  def exposureTimeModes: NonEmptyList[(ScorpioFilter, ExposureTimeMode)] =
    NonEmptyList.of(
      ScorpioFilter.G -> g,
      ScorpioFilter.R -> r,
      ScorpioFilter.I -> i,
      ScorpioFilter.Z -> z
    )

  def steps: NonEmptyList[TelescopeConfig] =
    telescopeConfigs.telescopeConfigs

  def hashBytes: Array[Byte] =
    val bao = new ByteArrayOutputStream(256)
    val out = new DataOutputStream(bao)

    out.writeChars(fpu.tag)
    exposureTimeModes.toList.foreach: (_, etm) =>
      out.write(etm.hashBytes)
    out.writeChars(telescopeConfigs.offsetsType.tag)
    steps.toList.foreach: tc =>
      out.write(tc.hashBytes)

    out.close()
    bao.toByteArray

object Config:

  val DefaultTelescopeConfigs: SlitTelescopeConfigs =
    defaultSlitTelescopeConfigs(ScorpioSlitOffsetPreset.NodAlongSlit)
