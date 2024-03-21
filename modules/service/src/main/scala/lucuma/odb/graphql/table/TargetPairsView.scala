// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait TargetPairsView[F[_]] extends BaseMapping[F] {

  object TargetPairsView extends TableDef("v_target_pairs") {
    val Left = col("c_left", target_id)
    val Right = col("c_right", target_id)
  }

}
