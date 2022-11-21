// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.binding.BooleanBinding

import table.ObservationView
import table.ProgramTable

trait ObservationMapping[F[_]]
  extends ObservationView[F]
     with ProgramTable[F]  {

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservationType,
      fieldMappings = List(
        SqlField("id", ObservationView.Id, key = true),
        SqlField("programId", ObservationView.ProgramId, hidden = true),
        SqlField("existence", ObservationView.Existence, hidden = true),
        SqlField("subtitle", ObservationView.Subtitle),
        SqlField("status", ObservationView.Status),
        SqlField("activeStatus", ObservationView.ActiveStatus),
        SqlField("visualizationTime", ObservationView.VisualizationTime),
        SqlObject("posAngleConstraint"),
        SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id)),
        SqlObject("targetEnvironment"),
        SqlObject("constraintSet"),
        SqlObject("scienceRequirements"),
        SqlObject("observingMode"),
        RootEffect.computeCursor("itc") { (_, tpe, env) =>
          val useCache = env.get[Boolean]("useCache").getOrElse(true)
          itcQuery(tpe, useCache)
        }
      )
    )

  def itcQuery(tpe: Type, useCache: Boolean): F[Result[Cursor]]

  lazy val ObservationElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ObservationType -> {
        case Select("itc", List(
          BooleanBinding("useCache", rUseCache)
        ), child) =>
          rUseCache.map { useCache =>
            Environment(
              Env("useCache" -> useCache),
              Select("itc", Nil, child)
            )
          }
      }
    )

}

