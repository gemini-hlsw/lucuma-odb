// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait FilterTypeTable[F[_]] extends BaseMapping[F] {

  object FilterTypeTable extends TableDef("t_filter_type") {
    val Tag       = col("c_tag", tag)
    val ShortName = col("c_short_name", varchar)
    val LongName  = col("c_long_name", varchar)
  }

}