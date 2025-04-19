// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.functor.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning

trait GmosCommonParsers {

  import util.*

  val xBinning: Parser[GmosXBinning] =
    oneOf(GmosXBinning.values.toList.fproductLeft(_.count.toString)*).withContext("GMOS X-Binning")

  val yBinning: Parser[GmosYBinning] =
    oneOf(GmosYBinning.values.toList.fproductLeft(_.count.toString)*).withContext("GMOS Y-Binning")

  val order: Parser[NonEmptyList[GmosGratingOrder]] =
    manyOf(GmosGratingOrder.all.fproductLeft(_.count.toString)*).withContext("GMOS grating order")

  val gain: Parser[NonEmptyList[GmosAmpGain]] =
    manyOfEnumerated[GmosAmpGain].withContext("GMOS amp gain")

}
