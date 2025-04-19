// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait GroupPairsView[F[_]] extends BaseMapping[F] {

  object GroupPairsView extends TableDef("v_group_pairs") {
    val Left = col("c_left", group_id)
    val Right = col("c_right", group_id)
  }

}
