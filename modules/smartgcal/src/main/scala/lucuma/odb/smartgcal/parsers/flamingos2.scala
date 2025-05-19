// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.odb.smartgcal.data.Flamingos2.FileEntry
import lucuma.odb.smartgcal.data.Flamingos2.FileKey

trait Flamingos2Parsers {
  import common.*
  import util.*

  val filter: Parser[NonEmptyList[Flamingos2Filter]] =
    // This works with the current files. Maybe it is fragile?
    manyOfEnumeratedBy[Flamingos2Filter](_.longName).withContext("Flamingos 2 filter")

  val fpu: Parser[NonEmptyList[Option[Flamingos2Fpu]]] =
    // pinhole and subpinhole don't exist in the current version of the files
    manyOfOption("Imaging (none)",
      "1-pix longslit" -> Flamingos2Fpu.LongSlit1,
      "2-pix longslit" -> Flamingos2Fpu.LongSlit2,
      "3-pix longslit" -> Flamingos2Fpu.LongSlit3,
      "4-pix longslit" -> Flamingos2Fpu.LongSlit4,
      "6-pix longslit" -> Flamingos2Fpu.LongSlit6,
      "8-pix longslit" -> Flamingos2Fpu.LongSlit8,
    ).withContext("Flamingos 2 fpu")

  val disperser: Parser[NonEmptyList[Option[Flamingos2Disperser]]] =
    manyOfOption("None",
      "R=1200 (J + H) grism"       -> Flamingos2Disperser.R1200JH,
      "R=1200 (H + K) grism"       -> Flamingos2Disperser.R1200HK,
      "R=3000 (J or H or K) grism" -> Flamingos2Disperser.R3000
    ).withContext("Flamingos 2 disperser")

  val fileKey: Parser[FileKey] =
    (
      (disperser <* columnSep) ~
      (filter    <* columnSep) ~
      fpu
    ).map { case (((disperser, filter), fpu)) =>
      FileKey(disperser, filter, fpu)
    }

  def fileEntry: Parser[FileEntry] =
    ((fileKey <* file.keyValueSep) ~ file.legacyValue).map { case (k, v) =>
      FileEntry(k, v)
    }

}

object flamingos2 extends Flamingos2Parsers
