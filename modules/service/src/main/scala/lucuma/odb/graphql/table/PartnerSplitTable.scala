// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._
import skunk.data.Type

trait PartnerSplitTable[F[_]] { self: SkunkMapping[F] =>

  object PartnerSplitTable extends TableDef("t_partner_split") {
    val ProgramId  = col("c_program_id", program_id)
    val Partner    = col("c_partner", tag)
    val Percent    = col("c_percent", int2)
  }

}