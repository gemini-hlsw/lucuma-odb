// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.functor.*
import cats.syntax.option.*
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

  private def asterismObject(name: String): SqlObject =
    SqlObject(
      name,
      Join(ObservationView.Id, AsterismTargetTable.ObservationId),
      Join(AsterismTargetTable.TargetId, TargetView.TargetId)
    )

  lazy val TargetEnvironmentMapping: ObjectMapping =
    ObjectMapping(
      tpe = TargetEnvironmentType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true, hidden = true),
        asterismObject("asterism"),
        asterismObject("firstScienceTarget"),
        SqlObject("explicitBase")
      )
    )

  private def asterismQuery(includeDeleted: Boolean, firstOnly: Boolean, child: Query): Query =
    FilterOrderByOffsetLimit(
      pred   = Option.unless(includeDeleted)(Eql[Existence](UniquePath(List("existence")), Const(Existence.Present))),
      oss    = List(OrderSelection(UniquePath[lucuma.core.model.Target.Id](List("id")))).some,
      offset = none,
      limit  = Option.when(firstOnly)(1),
      child  = child
    )

  lazy val TargetEnvironmentElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      TargetEnvironmentType -> {
        case Select("asterism", List(
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          rIncludeDeleted.map { includeDeleted =>
            Select("asterism", Nil, asterismQuery(includeDeleted, firstOnly = false, child))
          }

        // TODO: not yet working
        case Select("firstScienceTarget", List(
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          rIncludeDeleted.map { includeDeleted =>
            Select("firstScienceTarget", Nil, Unique(asterismQuery(includeDeleted, firstOnly = true, child)))
          }
      }
    )

}

