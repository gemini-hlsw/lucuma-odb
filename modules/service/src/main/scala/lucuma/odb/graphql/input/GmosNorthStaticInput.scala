// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthStageMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.odb.graphql.binding.*

object GmosNorthStaticInput {

  val Binding: Matcher[GmosNorth] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthStageModeBinding.Option("stageMode", rStageMode),
        GmosNorthDetectorBinding.Option("detector", rDetector),
        MosPreImagingBinding.Option("mosPreImaging", rPreImaging),
        GmosNodAndShuffleInput.Binding.Option("nodAndShuffle", rNodAndShuffle)
      ) =>
        (rStageMode, rDetector, rPreImaging, rNodAndShuffle)
          .parMapN { (stageMode, detector, preImage, ns) =>
            GmosNorth(
              stageMode.getOrElse(GmosNorthStageMode.FollowXy),
              detector.getOrElse(GmosNorthDetector.Hamamatsu),
              preImage.getOrElse(MosPreImaging.IsNotMosPreImaging),
              ns
            )
          }
    }

} 