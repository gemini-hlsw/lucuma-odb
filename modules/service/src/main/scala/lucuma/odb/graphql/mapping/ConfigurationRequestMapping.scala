// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.kernel.Resource
import cats.syntax.all.*
import grackle.Context
import grackle.Cursor
import grackle.Query
import grackle.Query.EffectHandler
import grackle.Result
import grackle.ResultT
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.model.ConfigurationRequest
import lucuma.odb.graphql.table.ConfigurationRequestView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.service.Services

import Services.Syntax.*

trait ConfigurationRequestMapping[F[_]] extends ConfigurationRequestView[F] with ProgramTable[F] {

  def services: Resource[F, Services[F]]

  lazy val ConfigurationRequestMapping =
    ObjectMapping(ConfigurationRequestType)(
      SqlField("id", ConfigurationRequestView.Id, key = true),
      SqlField("status", ConfigurationRequestView.Status),
      SqlObject("program", Join(ConfigurationRequestView.ProgramId, ProgramTable.Id)),
      SqlObject("configuration"),
      EffectField("applicableObservations", applicableObservationsHandler, List("id"))
    )

  val applicableObservationsHandler: EffectHandler[F] = pairs =>

    val sequence: ResultT[F, List[(ConfigurationRequest.Id, Cursor, Context)]] =
      ResultT.fromResult:
        pairs.traverse: (query, cursor) =>
          for {
            o <- cursor.fieldAs[ConfigurationRequest.Id]("id")
            c <- Query.childContext(cursor.context, query)
          } yield (o, cursor, c)

    def query(using Services[F]): ResultT[F, List[Cursor]] =
      sequence.flatMap: tuples =>
        ResultT(configurationService.selectObservations(tuples.map(_._1))).map: reqs =>
          tuples.map: (key, cursor, childContext) =>
            CirceCursor(childContext, reqs.get(key).orEmpty.asJson, Some(cursor), cursor.fullEnv)

    services.use: s =>
      query(using(s)).value

}

