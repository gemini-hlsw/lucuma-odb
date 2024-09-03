// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import table.ObservationView

trait ObservationEffectHandler[F[_]] extends ObservationView[F] {

  protected def effectHandler[E, R](
    readEnv:   Env => Result[E],
    calculate: (Program.Id, Observation.Id, E) => F[Result[R]]
  )(using Eq[E], io.circe.Encoder[R]): EffectHandler[F] =
    readQueryAndCursorEffectHander((_, c) => readEnv(c.fullEnv), calculate)

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
