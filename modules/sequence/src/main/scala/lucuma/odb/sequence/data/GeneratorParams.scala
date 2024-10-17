// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.syntax.either.*
import lucuma.core.enums.CalibrationRole
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

import java.nio.charset.StandardCharsets.UTF_8

case class GeneratorParams(
  itcInput:        Either[ItcInput.Missing, ItcInput.Defined],
  observingMode:   ObservingMode,
  calibrationRole: Option[CalibrationRole]
)

object GeneratorParams:

  given Eq[GeneratorParams] =
    Eq.by { a => (
      a.itcInput,
      a.observingMode,
      a.calibrationRole
    )}

  given HashBytes[GeneratorParams] with
    def hashBytes(a: GeneratorParams): Array[Byte] =
      Array.concat(
        a.itcInput.bimap(_.hashBytes, _.hashBytes).merge,
        a.observingMode.hashBytes,
        a.calibrationRole.fold(Array.emptyByteArray)(_.tag.getBytes(UTF_8))
      )