// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.effect.Sync
import cats.syntax.all.*
import grackle.Env
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.Value.StringValue
import grackle.skunk.SkunkMapping

trait QueryMapping[F[_]: Sync] extends BaseMapping[F] {

  val QueryType: TypeRef = schema.ref("Query")

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(QueryType)(
      RootEffect.computeEncodable("hello")((_, env) => hello(env))
    )

  lazy val QueryElaborator: ElaboratorPF = List(
    helloElaborator
  ).combineAll

  def hello(env: Env): F[Result[String]] =
    env.get[String]("name") match {
      case Some(name) => Sync[F].pure(Result(s"Hello, $name!"))
      case None       => Sync[F].pure(Result("Hello, world!"))
    }

  def helloElaborator: ElaboratorPF = {
    case (QueryType, "hello", List(Binding("name", StringValue(name)))) =>
      Elab.env("name", name)
  }
}
