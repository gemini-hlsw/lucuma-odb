// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.ghost.ifu

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostResolutionMode

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream



case class Config(
  resolutionMode:       GhostResolutionMode,
  red:                  DetectorConfig.Red,
  blue:                 DetectorConfig.Blue,
  explicitIfu1Agitator: Option[GhostIfu1FiberAgitator],
  explicitIfu2Agitator: Option[GhostIfu2FiberAgitator]
) derives Eq:

  def defaultIfu1Agitator:  GhostIfu1FiberAgitator =
    GhostIfu1FiberAgitator.Disabled

  def ifu1Agitator: GhostIfu1FiberAgitator =
    explicitIfu1Agitator.getOrElse(defaultIfu1Agitator)

  def defaultIfu2Agitator:  GhostIfu2FiberAgitator =
    GhostIfu2FiberAgitator.Disabled

  def ifu2Agitator: GhostIfu2FiberAgitator =
    explicitIfu2Agitator.getOrElse(defaultIfu2Agitator)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.writeChars(resolutionMode.tag)
    out.write(red.value.hashBytes)
    out.write(blue.value.hashBytes)
    out.writeChars(ifu1Agitator.tag)
    out.writeChars(ifu2Agitator.tag)

    out.close()
    bao.toByteArray