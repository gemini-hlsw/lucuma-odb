// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.gmos.GmosNodAndShuffle
import lucuma.odb.graphql.binding.*

object GmosNodAndShuffleInput {

  val Binding: Matcher[GmosNodAndShuffle] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetInput.Binding("posA", rPosA),
        OffsetInput.Binding("posB", rPosB),
        GmosEOffsettingBinding("eOffset", rEOffset),
        PosIntBinding("shuffleOffset", rShuffleOffset),
        PosIntBinding("shuffleCycle", rShuffleCycles)
      ) =>
        (rPosA, rPosB, rEOffset, rShuffleOffset, rShuffleCycles).parMapN(GmosNodAndShuffle(_, _, _, _, _))
    }
}
