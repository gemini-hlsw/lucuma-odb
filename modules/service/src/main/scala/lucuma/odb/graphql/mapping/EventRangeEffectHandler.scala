// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Eq
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import grackle.Cursor
import grackle.Query
import grackle.Query.EffectHandler
import grackle.Result
import grackle.ResultT
import grackle.circe.CirceMappingLike
import grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

import scala.reflect.ClassTag

/**
 * An `EffectHandler` implementation for computing the `TimestampInterval` for
 * a series of execution events (see `ExecutionEventServices` `atomRange`,
 * `stepRange` and `visitRange`).
 */
trait EventRangeEffectHandler[F[_]: MonadCancelThrow] extends CirceMappingLike[F] {

  def eventRangeEffectHandler[T : ClassTag : Eq](
    fieldName: String,
    services:  Resource[F, Services[F]],
    range:     Services[F] ?=> T => Transaction[F] ?=> F[Option[TimestampInterval]]
  ): EffectHandler[F] =

    new EffectHandler[F] {
      def calculateInterval(id: T): F[Result[Json]] =
        services.useTransactionally {
          range(id).map(_.asJson.success)
        }

      override def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          ids  <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[T](fieldName)}.pure[F])
          jsns <- ids.distinct.traverse(id => ResultT(calculateInterval(id)).tupleLeft(id))
          res  <- ResultT(
                    ids
                      .flatMap { id => jsns.find(r => r._1 === id).map(_._2).toList }
                      .zip(queries)
                      .traverse { case (result, (query, parentCursor)) =>
                        Query.childContext(parentCursor.context, query).map { childContext =>
                          CirceCursor(childContext, result, Some(parentCursor), parentCursor.fullEnv)
                        }
                      }.pure[F]
                  )
        } yield res).value
    }

}
