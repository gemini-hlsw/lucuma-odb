// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthStageMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.odb.graphql.binding.*

object GmosSouthStaticInput {

  val Binding: Matcher[GmosSouth] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosSouthStageModeBinding.Option("stageMode", rStageMode),
        GmosSouthDetectorBinding.Option("detector", rDetector),
        MosPreImagingBinding.Option("mosPreImaging", rPreImaging),
        GmosNodAndShuffleInput.Binding.Option("nodAndShuffle", rNodAndShuffle)
      ) =>
        (rStageMode, rDetector, rPreImaging, rNodAndShuffle)
          .parMapN { (stageMode, detector, preImage, ns) =>
            GmosSouth(
              stageMode.getOrElse(GmosSouthStageMode.FollowXyz),
              detector.getOrElse(GmosSouthDetector.Hamamatsu),
              preImage.getOrElse(MosPreImaging.IsNotMosPreImaging),
              ns
            )
          }
    }

}