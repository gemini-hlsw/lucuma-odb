// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import grackle.syntax.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.User

import scala.reflect.ClassTag

/**
 * A simple `EffectHandler` implementation that extracts a key from the cursor
 * and uses it to compute a value via the provided function.
 */
trait KeyValueEffectHandler[F[_]: MonadCancelThrow] extends CirceMappingLike[F] {

  def keyValueEffectHandler[K : ClassTag : Eq, T: Encoder](keyField: String)(
    calculate: User ?=> K => F[T]
  ):  EffectHandler[F] =

    new EffectHandler[F] {
      override def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          usr <- ResultT.fromResult(UserEnv.fromQueries(queries))
          ctx <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[K](keyField) }.pure[F])
          all <- ctx.distinct.traverse { k => ResultT(calculate(using usr)(k).map(Result.success)).tupleLeft(k) }
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

  /**
   * An `EffectHandler` that computes the values for every key in the result at
   * once, so that work shared between keys (e.g., loading the program that
   * contains them) is done once rather than per key.
   *
   * Grackle hands a handler every query that uses it, across all the fields
   * it serves, so one handler may back several fields.  `calculateAll`
   * receives the distinct keys read from the cursors along with the names of
   * the fields requested, and `encode` turns a key's value into the JSON for
   * one of those fields.
   */
  def keyedBatchEffectHandler[K, T](
    readKey:      Cursor => Result[K],
    calculateAll: User ?=> (List[K], Set[String]) => F[Map[K, Result[T]]],
    encode:       (String, T) => Result[Json]
  ): EffectHandler[F] =

    new EffectHandler[F] {
      override def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
        (for {
          usr    <- ResultT.fromResult(UserEnv.fromQueries(queries))
          keys   <- ResultT(queries.traverse { case (_, cursor) => readKey(cursor) }.pure[F])
          fields <- ResultT(queries.traverse { case (query, _) =>
                      Query.rootName(query).map(_._1).toResultOrError("Query has the wrong shape")
                    }.pure[F])
          all    <- ResultT.liftF(calculateAll(using usr)(keys.distinct, fields.toSet))
          res    <- ResultT(keys
                      .zip(fields)
                      .zip(queries)
                      .traverse { case ((k, field), (query, parentCursor)) =>
                        for {
                          value        <- all.getOrElse(k, Result.failure(s"No result computed for $k"))
                          json         <- encode(field, value)
                          childContext <- Query.childContext(parentCursor.context, query)
                        } yield CirceCursor(childContext, json, Some(parentCursor), parentCursor.fullEnv)
                      }.pure[F]
                    )
        } yield res).value
    }

}