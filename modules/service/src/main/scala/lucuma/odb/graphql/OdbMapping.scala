// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import _root_.skunk.Session
import cats.data.NonEmptyList
import cats.effect.{Unique => _, _}
import cats.syntax.all._
import edu.gemini.grackle._
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.skunk.SkunkMonitor
import lucuma.core.model.User
import natchez.Trace
import lucuma.odb.graphql.snippet._

object OdbMapping {

  case class Channels[F[_]]()

  object Channels {
    def apply[F[_]](pool: Resource[F, Session[F]]): Resource[F, Channels[F]] =
      pool.map { _ =>
        Channels()
      }
  }

  def apply[F[_]: Async: Trace](
    channels: Channels[F],
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
  ): F[User => Mapping[F]] = {

    val m: Mapping[F] =
      new SkunkMapping[F](pool, monitor) with SnippetMapping[F] {
        val snippet: Snippet =
          NonEmptyList.of(
            FilterTypeSnippet(this),
            PartnerSnippet(this),
          ).reduce
        val schema = snippet.schema
        val typeMappings = snippet.typeMappings
      }

    ((_: User) => m).pure[F]

  }


}