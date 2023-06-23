// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._

trait ObservationPairsView[F[_]] extends BaseMapping[F] {

  object ObservationPairsView extends TableDef("v_observation_pairs") {
    val Left = col("c_left", observation_id)
    val Right = col("c_right", observation_id)
  }

}
