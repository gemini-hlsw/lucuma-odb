// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.odb.graphql.binding._

object GmosNorthStaticInput {

  val Binding: Matcher[GmosNorth] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthStageModeBinding("stageMode", rStageMode), //.map(_.getOrElse(GmosNorthStageMode.FollowXy)),
        GmosNorthDetectorBinding("detector", rDetector),
        MosPreImagingBinding("mosPreImaging", rPreImaging),
        GmosNodAndShuffleInput.Binding.Option("nodAndShuffle", rNodAndShuffle)
      ) =>
        (rStageMode, rDetector, rPreImaging, rNodAndShuffle).parMapN(GmosNorth(_, _, _, _))
    }

} 