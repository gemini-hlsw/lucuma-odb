// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait PartnerTable[F[_]] { self: SkunkMapping[F] =>

  object PartnerTable extends TableDef("t_partner") {
    val Tag       = col("c_tag", tag)
    val ShortName = col("c_short_name", varchar)
    val LongName  = col("c_long_name", varchar)
    val Active    = col("c_active", bool)
    // val c_sites   = col("c_sites", _site) // need array support in grackle
  }

}