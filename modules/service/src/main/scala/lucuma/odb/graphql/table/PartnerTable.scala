// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait PartnerTable[F[_]] extends BaseMapping[F] {

  object PartnerTable extends TableDef("t_partner") {
    val Tag       = col("c_tag", tag)
    val ShortName = col("c_short_name", varchar)
    val LongName  = col("c_long_name", varchar)
    val Active    = col("c_active", bool)
    // val c_sites   = col("c_sites", _site) // need array support in grackle
  }

}