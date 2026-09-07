// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.topic

import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Temporal
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
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.data.Identifier
import skunk.implicits.*

import scala.concurrent.duration.*

trait OdbTopic[E]:

  /** Creates the topic (with a no-op consumer attached). Its feed is started separately via `OdbTopic.runFeeds`. */
  def create[F[_]: Concurrent: Logger](sup: Supervisor[F]): F[Topic[F, E]]

  /** A stream that LISTENs on the given session and publishes decoded events to the topic. */
  def feed[F[_]: Concurrent: Logger: Tracer](
    s:         Session[F],
    maxQueued: Int,
    top:       Topic[F, E]
  ): Stream[F, Unit]

object OdbTopic:

  private val InitialRetryDelay: FiniteDuration = 1.second
  private val MaxRetryDelay: FiniteDuration     = 1.minute

  // A feed that survives at least this long is considered healthy, resetting the retry backoff.
  private val HealthyRunTime: FiniteDuration    = 5.minutes

  // Ok for some reason the stream handling is broken; something may have changed
  // in fs2 or skunk that releases the portal too early and you get portal not found
  // asynchronously when doing other things. This is a workaround for now that just
  // interpolates the strings directly rather than preparing a statement.
  def selectProgramUsers[F[_]: Tracer](
    s:   Session[F],
    pid: Program.Id,
  ): F[List[User.Id]] =
    Tracer[F].span("topic.selectProgramUsers").surround:
      s.execute(
        sql"""
          select c_user_id from t_program_user where c_program_id = '#${pid.toString}' and c_user_id notnull
        """.query(user_id)
      )

  /**
   * Runs the given topic feeds together on a single session checked out from
   * `pool`. If any feed fails, or they somehow all terminate, the session is
   * released and all the feeds are restarted (with backoff) on a fresh
   * session. The topics themselves survive restarts, so existing subscribers
   * are unaffected beyond missing any events notified while disconnected.
   */
  def runFeeds[F[_]: Temporal: Logger](
    name:  String,
    pool:  Resource[F, Session[F]],
    sup:   Supervisor[F],
    feeds: Session[F] => List[Stream[F, Unit]]
  ): F[Unit] =
    def go(delay: FiniteDuration): F[Unit] =
      val next: F[FiniteDuration] =
        for
          start   <- Temporal[F].monotonic
          outcome <- pool
                      .use: s =>
                        Stream.emits(feeds(s)).parJoinUnbounded.compile.drain
                      .attempt
          end     <- Temporal[F].monotonic
          d        = if end - start >= HealthyRunTime then InitialRetryDelay else delay
          _       <- outcome match
                       case Left(e)  => Logger[F].error(e)(s"$name topic event streams crashed! Restarting in $d.")
                       case Right(_) => Logger[F].warn(s"$name topic event streams terminated. Restarting in $d.")
          _       <- Temporal[F].sleep(d)
        yield (d * 2).min(MaxRetryDelay)
      next.flatMap(go)

    sup.supervise(go(InitialRetryDelay)).void

  def define[U, E](
    name:    String,
    channel: Identifier,
    pid:     U => Program.Id,
    element: (U, List[User.Id]) => E
  )(update: PartialFunction[Array[String], Option[U]]): OdbTopic[E] =
    new OdbTopic[E]:

      def updates[F[_]: Logger](
        s:         Session[F],
        maxQueued: Int
      ): Stream[F, U] =
        s.channel(channel).listen(maxQueued).flatMap: n =>
          update
            .lift(n.value.split(","))
            .flatten
            .fold(Stream.exec(Logger[F].warn(s"Invalid $name event: $n")))(Stream(_))

      def elements[F[_]: Logger: Tracer](
        s:         Session[F],
        maxQueued: Int
      ): Stream[F, E] =
        for
          up <- updates(s, maxQueued)
          us <- Stream.eval(selectProgramUsers(s, pid(up)))
          e   = element(up, us)
          _  <- Stream.eval(Logger[F].info(s"$name channel: $e"))
        yield e

      // publish1 (unlike the publish pipe) never closes the topic, so the
      // feed can be restarted against the same topic after a failure.
      def feed[F[_]: Concurrent: Logger: Tracer](
        s:         Session[F],
        maxQueued: Int,
        top:       Topic[F, E]
      ): Stream[F, Unit] =
        elements(s, maxQueued).evalMap(e => top.publish1(e).void)

      def create[F[_]: Concurrent: Logger](sup: Supervisor[F]): F[Topic[F, E]] =
        for
          top <- Topic[F, E]

          // Add a no-op subscriber to guarantee that there is at least one
          // subscriber consuming events at all times.
          _   <- sup.supervise(
                   (top
                     .subscribe(1024)
                     .evalTap(e => Logger[F].debug(s"$name Event Consumer received $e"))
                     .compile
                     .drain
                     .onError:
                       case e => Logger[F].error(e)(s"$name Event Consumer crashed!")
                   ) >> Logger[F].info(s"$name Event Consumer terminated.")
                 )

          _   <- Logger[F].info(s"Created topic for ${channel.value}")
        yield top
