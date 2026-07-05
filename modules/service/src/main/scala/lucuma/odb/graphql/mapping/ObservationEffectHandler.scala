// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.traverse.*
import grackle.Cursor
import grackle.Env
import grackle.Query
import grackle.Query.EffectHandler
import grackle.Result
import grackle.ResultT
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

import table.ObservationView

trait ObservationEffectHandler[F[_]] extends ObservationView[F] {

  protected def T: Tracer[F]

  protected def effectHandler[E, R](
    readEnv:   Env => Result[E],
    calculate: (Program.Id, Observation.Id, E) => F[Result[R]]
  )(using Eq[E], io.circe.Encoder[R]): EffectHandler[F] =
    readQueryAndCursorEffectHander((_, c) => readEnv(c.fullEnv), calculate)

  /**
   * Batched effect handler keyed by observation id: `calculateAll` receives every observation id in
   * the result at once and returns a per-observation result map, replacing the
   * one-transaction-per-observation N+1 of `effectHandler`. The map must contain an entry for every
   * requested id; a missing key fails that observation's field. `spanName` names the tracing span so
   * each field remains individually attributable in traces.
   */
  protected def batchedEffectHandler[R](
    spanName: String
  )(
    calculateAll: List[Observation.Id] => F[Map[Observation.Id, Result[R]]]
  )(using io.circe.Encoder[R]): EffectHandler[F] =
    new EffectHandler[F] {

      private def oids(queries: List[(Query, Cursor)]): Result[List[Observation.Id]] =
        queries.traverse { case (_, cursor) => cursor.fieldAs[Observation.Id]("id") }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        T.span(spanName, Attribute("count", queries.size.toLong)).surround {
          (for {
            os  <- ResultT(oids(queries).pure[F])
            map <- ResultT.liftF(T.span(s"$spanName.calculateAll").surround(calculateAll(os)))
            res <- ResultT(os.zip(queries).traverse { case (oid, (query, parentCursor)) =>
                     for {
                       r            <- map.getOrElse(oid, Result.failure(s"No result computed for observation $oid"))
                       childContext <- Query.childContext(parentCursor.context, query)
                     } yield CirceCursor(childContext, r.asJson, Some(parentCursor), parentCursor.fullEnv)
                   }.pure[F])
          } yield res).value
        }
    }

  protected def readQueryAndCursorEffectHander[E, R](
    readQueryAndCursor:   (Query, Cursor) => Result[E],
    calculate: (Program.Id, Observation.Id, E) => F[Result[R]]
  )(using Eq[E], io.circe.Encoder[R]): EffectHandler[F] =

    new EffectHandler[F] {

      private def queryContext(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id, E)]] =
        queries.traverse { case (query, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
            e <- readQueryAndCursor(query, cursor)
          } yield (p, o, e)
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        T.span("effect.perObs", Attribute("count", queries.size.toLong)).surround {
          (for {
            ctx <- ResultT(queryContext(queries).pure[F])
            obs <- ctx.distinct.traverse { case (pid, oid, env) =>
                     ResultT(calculate(pid, oid, env)).map((oid, env, _))
                   }
            res <- ResultT(ctx
                     .flatMap { case (_, oid, env) => obs.find(r => r._1 === oid && r._2 === env).map(_._3).toList }
                     .zip(queries)
                     .traverse { case (result, (query, parentCursor)) =>
                       Query.childContext(parentCursor.context, query).map { childContext =>
                         CirceCursor(childContext, result.asJson, Some(parentCursor), parentCursor.fullEnv)
                       }
                     }.pure[F]
                   )
            } yield res
          ).value
        }
    }

}
