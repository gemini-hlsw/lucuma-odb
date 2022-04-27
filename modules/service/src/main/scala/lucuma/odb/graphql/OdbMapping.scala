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
import lucuma.core.model.GuestRole
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff
import org.tpolecat.sourcepos.SourcePos
import cats.Applicative
import lucuma.odb.graphql.util._
import fs2.concurrent.Topic
import lucuma.odb.graphql.topic.ProgramTopic
import cats.effect.std.Supervisor
import org.typelevel.log4cats.Logger
import cats.Traverse

object OdbMapping {

  case class Topics[F[_]](
    program: Topic[F, ProgramTopic.Element]
  )

  object Topics {
    def apply[F[_]: Concurrent: Logger](pool: Resource[F, Session[F]]): Resource[F, Topics[F]] =
      for {
        sup <- Supervisor[F]
        ses <- pool
        pro <- Resource.eval(ProgramTopic(ses, 1024, sup))
      } yield Topics(pro)
  }

  def apply[F[_]: Sync: Trace](
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    user:     User,
    topics:   Topics[F],
  ):  F[Mapping[F]] =
    Trace[F].span(s"Creating mapping for ${user.displayName} (${user.id}, ${user.role})") {
      pool.use(enumSchema(_)).map { enums =>
        val m: Mapping[F] =
           new SkunkMapping[F](pool, monitor)
          with SnippetMapping[F]
          with ComputeMapping[F]
          with MutationCompanionOps[F] {

          val snippet: Snippet =
            NonEmptyList.of(
              BaseSnippet(this),
              Snippet(enums, Nil),
              FilterTypeSnippet(this),
              PartnerSnippet(this),
              UserSnippet(this),
              ProgramSnippet(this, pool, user, topics),
              AllocationSnippet(this, pool, user),
              ObservationSnippet(this, pool, user),
            ).reduce

          val schema = snippet.schema
          val typeMappings = snippet.typeMappings
          override val selectElaborator = snippet.selectElaborator

        }
        // m.validator.validateMapping().map(_.toErrorMessage).foreach(println)
        m
      }
    }

  def enumSchema[F[_]: Applicative](s: Session[F]): F[Schema] =
    List(FilterTypeSnippet.enumType(s), PartnerSnippet.enumType(s)).sequence.map { tpes =>
      new Schema {
        def pos: SourcePos = SourcePos.instance
        def types: List[NamedType] = tpes
        def directives: List[Directive] = Nil
      }
    }

}