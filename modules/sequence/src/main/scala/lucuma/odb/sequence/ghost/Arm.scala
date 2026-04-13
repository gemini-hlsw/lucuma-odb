// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.ghost

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.NewType
import lucuma.odb.sequence.syntax.all.*

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

case class Arm(
  exposureTimeMode: ExposureTimeMode,
  detector:         Detector
) derives Eq:

  def hashBytes: Array[Byte] =
    val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
    val out: DataOutputStream      = new DataOutputStream(bao)

    out.write(exposureTimeMode.hashBytes)
    out.write(detector.hashBytes)

    out.close()
    bao.toByteArray

object Arm:

  object Blue extends NewType[Arm]
  type Blue = Blue.Type

  object Red extends NewType[Arm]
  type Red = Red.Type

  def blue(
    exposureTimeMode: ExposureTimeMode,
    explicitBinning:  Option[GhostBinning] = None,
    explicitReadMode: Option[GhostReadMode] = None
  ): Blue.Type =
    Blue.apply:
      Arm(
        exposureTimeMode,
        Detector(
          GhostBinning.Default,
          explicitBinning,
          GhostReadMode.DefaultRed,
          explicitReadMode
        )
      )

  def red(
    exposureTimeMode: ExposureTimeMode,
    explicitBinning:  Option[GhostBinning] = None,
    explicitReadMode: Option[GhostReadMode] = None
  ): Red.Type =
    Red.apply:
      Arm(
        exposureTimeMode,
        Detector(
          GhostBinning.Default,
          explicitBinning,
          GhostReadMode.DefaultRed,
          explicitReadMode
        )
      )