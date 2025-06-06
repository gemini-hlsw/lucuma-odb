// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.syntax.either.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.ObservingMode.given
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

case class GeneratorParams(
  itcInput:         Either[MissingParamSet, ItcInput],
  scienceBand:      Option[ScienceBand],
  observingMode:    ObservingMode,
  calibrationRole:  Option[CalibrationRole],
  declaredComplete: Boolean,
  acqResetTime:     Option[Timestamp]
)

object GeneratorParams:

  given Eq[GeneratorParams] =
    Eq.by: a =>
      (
        a.itcInput,
        a.scienceBand,
        a.observingMode,
        a.calibrationRole,
        a.declaredComplete,
        a.acqResetTime
      )

  given HashBytes[GeneratorParams] with
    def hashBytes(a: GeneratorParams): Array[Byte] =
      Array.concat(
        a.itcInput.bimap(_.hashBytes, _.hashBytes).merge,
        a.scienceBand.hashBytes,
        a.observingMode.hashBytes,
        a.calibrationRole.hashBytes,
        a.declaredComplete.hashBytes,
        a.acqResetTime.hashBytes
      )
