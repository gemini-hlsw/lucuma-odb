// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.functor.*
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Result
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.table.AsterismTargetTable

import table._
import input._
import util.Bindings._


trait TargetEnvironmentMapping[F[_]]
  extends AsterismTargetTable[F]
     with ObservationView[F]
     with TargetView[F] { this: SkunkMapping[F] =>

  lazy val TargetEnvironmentType: TypeRef =
    schema.ref("TargetEnvironment")

  lazy val TargetEnvironmentMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetEnvironmentType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        SqlObject("asterism",
          Join(ObservationView.Id, AsterismTargetTable.ObservationId),
          Join(AsterismTargetTable.TargetId, TargetView.TargetId)
        ),
        SqlObject("explicitBase")
      )
    )

  lazy val TargetEnvironmentElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      TargetEnvironmentType -> {
        case Select("asterism", List(
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          rIncludeDeleted.map { includeDeleted =>
            Select("asterism", Nil,
              Filter(
                if (includeDeleted) True else Eql[Existence](UniquePath(List("existence")), Const(Existence.Present)),
                child
              )
            )
          }
      }
    )

}

