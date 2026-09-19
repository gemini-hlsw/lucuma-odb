// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql.table

import resource.server.Codecs.*
import resource.server.graphql.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8
import skunk.codec.temporal.date

trait PublishedSemesterTables[F[_]] extends BaseMapping[F]:

  object PublishedSemesterTable extends TableDef("t_published_semester"):
    val Id          = col("c_id", int8)
    val Site        = col("c_site", site)
    val Semester    = col("c_semester", semester)
    val Title       = col("c_title", text_nonempty)
    val Version     = col("c_version", text_nonempty.opt)
    val Demo        = col("c_demo", bool)
    val NightsStart = col("c_nights_start", date)
    val NightsEnd   = col("c_nights_end", date)
    val Holidays    = col("c_holidays", date_array)

  object MoonEventTable extends TableDef("t_moon_event"):
    val Site     = col("c_site", site)
    val Semester = col("c_semester", semester)
    val Date     = col("c_date", date)
    val Phase    = col("c_phase", moon_phase)
