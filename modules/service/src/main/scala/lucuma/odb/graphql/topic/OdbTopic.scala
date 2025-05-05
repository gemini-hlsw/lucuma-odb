// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.Concurrent
import cats.effect.std.Supervisor
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import fs2.concurrent.Topic
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.util.Codecs.user_id
import org.typelevel.log4cats.Logger
import skunk.Session
import skunk.data.Identifier
import skunk.implicits.*

trait OdbTopic[E]:
  def create[F[_]](
    s:         Session[F],
    maxQueued: Int,
    sup:       Supervisor[F]
  )(using Concurrent[F], Logger[F]): F[Topic[F, E]]

object OdbTopic:

  // Ok for some reason the stream handling is broken; something may have changed
  // in fs2 or skunk that releases the portal too early and you get portal not found
  // asynchronously when doing other things. This is a workaround for now that just
  // interpolates the strings directly rather than preparing a statement.
  def selectProgramUsers[F[_]: Concurrent: Logger](
    s:   Session[F],
    pid: Program.Id,
  ): F[List[User.Id]] =
    s.execute(
      sql"""
        select c_user_id from t_program_user where c_program_id = '#${pid.toString}' and c_user_id notnull
      """.query(user_id)
    )

  def define[U, E](
    name:    String,
    channel: Identifier,
    pid:     U => Program.Id,
    element: (U, List[User.Id]) => E
  )(update: PartialFunction[Array[String], Option[U]]): OdbTopic[E] =
    new OdbTopic[E]:

      def updates[F[_]: Concurrent: Logger](
        s:         Session[F],
        maxQueued: Int
      ): Stream[F, U] =
        s.channel(channel).listen(maxQueued).flatMap: n =>
          update
            .lift(n.value.split(","))
            .flatten
            .fold(Stream.exec(Logger[F].warn(s"Invalid $name event: $n")))(Stream(_))

      def elements[F[_]: Concurrent: Logger](
        s:         Session[F],
        maxQueued: Int
      ): Stream[F, E] =
        for
          up <- updates(s, maxQueued)
          us <- Stream.eval(selectProgramUsers(s, pid(up)))
          e   = element(up, us)
          _  <- Stream.eval(Logger[F].info(s"$name channel: $e"))
        yield e

      def create[F[_]](
        s:         Session[F],
        maxQueued: Int,
        sup:       Supervisor[F]
      )(using Concurrent[F], Logger[F]): F[Topic[F, E]] =
        for
          top <- Topic[F, E]
          es   = elements(s, maxQueued).through(top.publish)
          _   <- sup.supervise(
                   es
                     .compile
                     .drain
                     .onError:
                        case e => Logger[F].error(e)(s"$name Event Stream crashed!")
                     >> Logger[F].info(s"$name Event Stream terminated.")
                 )

          // Add a no-op subscriber to guarantee that there is at least one
          // subscriber consuming events at all times.
          _   <- sup.supervise(
                   top
                     .subscribe(1024)
                     .evalTap(e => Logger[F].debug(s"$name Event Consumer received $e"))
                     .compile
                     .drain
                     .onError:
                       case e => Logger[F].error(e)(s"$name Event Consumer crashed!")
                     >> Logger[F].info(s"$name Event Consumer terminated.")
                 )
                   
          _   <- Logger[F].info(s"Started topic for ${channel.value}")
        yield top