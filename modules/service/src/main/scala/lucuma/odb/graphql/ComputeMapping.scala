// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Query
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import edu.gemini.grackle.circe.CirceMapping
import io.circe.{ Encoder, Json }
import org.tpolecat.sourcepos.SourcePos

trait ComputeMapping[F[_]] { this: CirceMapping[F] =>

  /** A RootMapping that computes a result directly. */
  sealed abstract case class ComputeRoot[A] private (
    fieldName: String,
    tpe: Type,
    pos: SourcePos,
    run: Cursor.Env => F[Result[Json]],
  ) extends RootMapping {
    def cursor(query: Query, env: Cursor.Env, resultName: Option[String]): fs2.Stream[F,Result[(Query, Cursor)]] =
        fs2.Stream.eval(run(env)).map { r => r.map(a => (query, CirceCursor(Cursor.Context(fieldName, resultName, tpe), a, None, env))) }
    def mutation: Mutation = Mutation.None
    def withParent(tpe: Type): RootMapping = this
  }

  object ComputeRoot {
    /** Construct a `ComputeRoot` for the specified field and type. */
    def apply[A](fieldName: String, tpe: Type, run: Cursor.Env => F[Result[A]])(
      implicit pos: SourcePos,
               enc: Encoder[A]
    ): ComputeRoot[A] =
      new ComputeRoot[A](fieldName, tpe, pos, e => run(e).map(_.map(enc(_)))) {}
  }

}

