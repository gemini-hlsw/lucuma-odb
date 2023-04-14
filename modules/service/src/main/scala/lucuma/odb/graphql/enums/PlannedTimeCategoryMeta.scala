// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.functor.*
import skunk.*
import skunk.implicits.*

case class PlannedTimeCategoryMeta(
  name:        String,
  description: String
)

object PlannedTimeCategoryMeta {

  private object statements {
    import skunk.codec.text.varchar

    val decoder: Decoder[PlannedTimeCategoryMeta] =
      (varchar ~ varchar).gmap[PlannedTimeCategoryMeta]

    val select: Query[Void, (String, PlannedTimeCategoryMeta)] =
      sql"""
        SELECT
          c_tag,
          c_name,
          c_description
        FROM
          t_planned_time_category
      """.query(varchar ~ decoder)
  }

  def select[F[_]: Functor](s: Session[F]): F[Map[String, PlannedTimeCategoryMeta]] =
    s.execute(statements.select).map(_.toMap)

}
