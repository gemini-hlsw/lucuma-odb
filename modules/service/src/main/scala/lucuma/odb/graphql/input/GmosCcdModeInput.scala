// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*


object GmosCcdModeInput {

  val Binding: Matcher[GmosCcdMode] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosBinningBinding.Option("xBin", rXBin),
        GmosBinningBinding.Option("yBin", rYBin),
        GmosAmpCountBinding.Option("ampCount", rAmpCount),
        GmosAmpGainBinding.Option("ampGain", rAmpGain),
        GmosAmpReadModeBinding.Option("ampReadMode", rAmpReadMode)
      ) => (rXBin, rYBin, rAmpCount, rAmpGain, rAmpReadMode).parMapN { (xBin, yBin, ampCount, ampGain, ampReadMode) =>
        GmosCcdMode(
          xBin.fold(GmosXBinning.One)(GmosXBinning.apply),
          yBin.fold(GmosYBinning.One)(GmosYBinning.apply),
          ampCount.getOrElse(GmosAmpCount.Twelve),
          ampGain.getOrElse(GmosAmpGain.Low),
          ampReadMode.getOrElse(GmosAmpReadMode.Slow)
        )
      }
    }

}
