// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import grackle.*
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.Value.*
import io.circe.Encoder

trait MutationMapping[F[_]] extends BaseMapping[F] {
  val MutationType: TypeRef = schema.ref("Mutation")

  lazy val MutationMapping =
    ObjectMapping(MutationType)(
      RootEffect.computeEncodable[String]("sendHello")((_, env) => sendHello(env))
    )

  lazy val MutationElaborator: ElaboratorPF = List(
    sendHelloElaborator
  ).combineAll

  def sendHello(env: Env): F[Result[String]] =
    env
      .getR[String]("name")
      .map(name => s"Hello ${name}")
      .pure[F]

  def sendHelloElaborator: ElaboratorPF =
    case (MutationType, "sendHello", List(Binding("name", StringValue(name)))) =>
      Elab.env("name", name)

}
