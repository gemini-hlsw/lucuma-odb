// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

case class Flamingos2ImagingRow(
  img:    ImagingRow,
  filter: Flamingos2Filter
)

object Flamingos2ImagingRow:

  val flamingos2: Parser[List[Flamingos2ImagingRow]] =
    ImagingRow.rows.flatMap: rs =>
      rs.traverse: r =>
        val row =
          for {
            _ <- Either.raiseWhen(r.instrument =!= Instrument.Flamingos2)(s"Cannot parse a ${r.instrument.tag} as ${Instrument.Flamingos2.tag}")
            f <- Enumerated[Flamingos2Filter].all.find(_.shortName === r.filter).toRight(s"Cannot find flamingos 2 filter: ${r.filter}.")
          } yield Flamingos2ImagingRow(r, f)

        row.fold(Parser.failWith, Parser.pure)
