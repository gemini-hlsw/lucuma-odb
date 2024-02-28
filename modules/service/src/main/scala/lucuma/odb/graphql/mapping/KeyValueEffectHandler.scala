// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Eq
import cats.effect.MonadCancelThrow
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
import io.circe.Encoder
import io.circe.syntax.*

import scala.reflect.ClassTag

/**
 * A simple `EffectHandler` implementation that extracts a key from the cursor
 * and uses it to compute a value via the provided function.
 */
trait KeyValueEffectHandler[F[_]: MonadCancelThrow] extends CirceMappingLike[F] {

  def keyValueEffectHandler[K : ClassTag : Eq, T: Encoder](keyField: String)(
    calculate: K => F[T]
  ):  EffectHandler[F] =

    new EffectHandler[F] {
      override def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          ctx <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[K](keyField) }.pure[F])
          all <- ctx.distinct.traverse { k => ResultT(calculate(k).map(Result.success)).tupleLeft(k) }
          res <- ResultT(ctx
                   .flatMap(k => all.find(r => r._1 === k).map(_._2).toList)
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