// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.circe.codec.all._

trait AsterismGroupView[F[_]] extends BaseMapping[F] {

  object AsterismGroupView extends TableDef("v_asterism_group") {
    val ProgramId            = col("c_program_id", program_id)
    val AsterismGroup        = col("c_asterism_group",     jsonb)
    val ExampleObservationId = col("c_example_observation_id", observation_id)
  }

}