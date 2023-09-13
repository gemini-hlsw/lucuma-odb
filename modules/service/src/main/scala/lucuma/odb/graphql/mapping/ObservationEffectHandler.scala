// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.Eq
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.traverse.*
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.EffectHandler
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program

import table.ObservationView

trait ObservationEffectHandler[F[_]] extends ObservationView[F] {

  protected def effectHandler[E, R](
    fieldName: String,
    readEnv:   Env => Result[E],
    calculate: (Program.Id, Observation.Id, E) => F[Result[R]]
  )(using Eq[E], io.circe.Encoder[R]): EffectHandler[F] =

    new EffectHandler[F] {

      private def queryContext(queries: List[(Query, Cursor)]): Result[List[(Program.Id, Observation.Id, E)]] =
        queries.traverse { case (_, cursor) =>
          for {
            p <- cursor.fieldAs[Program.Id]("programId")
            o <- cursor.fieldAs[Observation.Id]("id")
            e <- readEnv(cursor.fullEnv)
          } yield (p, o, e)
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ctx <- ResultT(queryContext(queries).pure[F])
          obs <- ctx.distinct.traverse { case (pid, oid, env) =>
                   ResultT(calculate(pid, oid, env)).map((oid, env, _))
                 }
          res <- ResultT(ctx
                   .flatMap { case (_, oid, env) => obs.find(r => r._1 === oid && r._2 === env).map(_._3).toList }
                   .zip(queries)
                   .traverse { case (result, (child, childCursor)) =>
                     childCursor.context.parent.toResultOrError("No parent context").map { parentContext =>
                       val parentField    = childCursor.path.head
                       val json: Json     = Json.fromFields(List(parentField -> Json.fromFields(List(fieldName -> result.asJson))))
                       val cursor: Cursor = CirceCursor(parentContext, json, Some(childCursor), childCursor.fullEnv)
                       (Query.Select(parentField, None, child), cursor)
                     }
                   }.pure[F]
                 )
          } yield res
        ).value

    }


}
