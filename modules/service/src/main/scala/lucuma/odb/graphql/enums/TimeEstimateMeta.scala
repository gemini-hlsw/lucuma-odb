// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.enums

import cats.Functor
import cats.syntax.functor.*
import lucuma.core.enums.Instrument
import lucuma.core.util.TimeSpan
import skunk.*
import skunk.implicits.*

case class TimeEstimateMeta(
  name:        String,
  description: String,
  instrument:  Option[Instrument],
  time:        TimeSpan
)

object TimeEstimateMeta {

  private object statements {

    import lucuma.odb.util.Codecs.instrument
    import lucuma.odb.util.Codecs.time_span
    import skunk.codec.text.varchar

    val decoder: Decoder[TimeEstimateMeta] =
      (varchar        *:
       varchar        *:
       instrument.opt *:
       time_span
      ).to[TimeEstimateMeta]

    val select: Query[Void, (String, TimeEstimateMeta)] =
      sql"""
        SELECT
          c_tag,
          c_name,
          c_description,
          c_instrument,
          c_time
        FROM
          t_time_estimate
      """.query(varchar ~ decoder)
  }

  def select[F[_]: Functor](s: Session[F]): F[Map[String, TimeEstimateMeta]] =
    s.execute(statements.select).map(_.toMap)

}