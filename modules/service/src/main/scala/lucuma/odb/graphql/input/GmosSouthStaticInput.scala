// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.graphql.binding.*

object GmosSouthStaticInput {

  val Binding: Matcher[GmosSouth] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosSouthStageModeBinding("stageMode", rStageMode),
        GmosSouthDetectorBinding("detector", rDetector),
        MosPreImagingBinding("mosPreImaging", rPreImaging),
        GmosNodAndShuffleInput.Binding.Option("nodAndShuffle", rNodAndShuffle)
      ) =>
        (rStageMode, rDetector, rPreImaging, rNodAndShuffle).parMapN(GmosSouth(_, _, _, _))
    }

}