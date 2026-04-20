// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.ghost

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.NewType
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class DetectorConfig(
  exposureTimeMode: ExposureTimeMode,
  defaultBinning:   GhostBinning,
  explicitBinning:  Option[GhostBinning],
  defaultReadMode:  GhostReadMode,
  explicitReadMode: Option[GhostReadMode]
) derives Eq:

  def binning: GhostBinning =
    explicitBinning.getOrElse(defaultBinning)

  def readMode: GhostReadMode =
    explicitReadMode.getOrElse(defaultReadMode)

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(exposureTimeMode.hashBytes)
    out.writeChars(defaultBinning.tag)
    out.writeChars(explicitBinning.foldMap(_.tag))
    out.writeChars(defaultReadMode.tag)
    out.writeChars(explicitReadMode.foldMap(_.tag))

    out.close()
    bao.toByteArray

object DetectorConfig:
  object Blue extends NewType[DetectorConfig]
  type Blue = Blue.Type

  object Red extends NewType[DetectorConfig]
  type Red = Red.Type

  def blue(
    exposureTimeMode: ExposureTimeMode,
    explicitBinning:  Option[GhostBinning] = None,
    explicitReadMode: Option[GhostReadMode] = None
  ): Blue.Type =
    Blue.apply:
      DetectorConfig(
        exposureTimeMode,
        GhostBinning.Default,
        explicitBinning,
        GhostReadMode.DefaultRed,
        explicitReadMode
      )

  def red(
    exposureTimeMode: ExposureTimeMode,
    explicitBinning:  Option[GhostBinning] = None,
    explicitReadMode: Option[GhostReadMode] = None
  ): Red.Type =
    Red.apply:
      DetectorConfig(
        exposureTimeMode,
        GhostBinning.Default,
        explicitBinning,
        GhostReadMode.DefaultRed,
        explicitReadMode
      )