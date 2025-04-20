// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.functor.*
import skunk.*
import skunk.implicits.*

case class ProposalStatusMeta(
  tag:     String,
  name:    String,
  ordinal: Short
)

object ProposalStatusMeta {
  private object statements {
    import skunk.codec.numeric.int2
    import skunk.codec.text.varchar

    val decoder: Decoder[ProposalStatusMeta] =
      (varchar *: varchar *: int2).to[ProposalStatusMeta]
  
    val select: Query[Void, ProposalStatusMeta] =
      sql"""
        SELECT
          c_tag,
          c_name,
          c_ordinal
        FROM
          t_proposal_status
      """.query(decoder)
  }

  def select[F[_]: Functor](s: Session[F]): F[Map[(String, Short), ProposalStatusMeta]] =
    s.execute(statements.select).map(_.map(m => ((m.tag, m.ordinal), m)).toMap)
}
