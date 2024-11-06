// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Predicate
import grackle.Predicate.and
import grackle.Predicate.True
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.ObservationIdBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ObservingModeGroupView
import lucuma.odb.graphql.table.ProgramTable

trait ObservingModeGroupMapping[F[_]]
  extends ObservingModeGroupView[F]
     with ObservationView[F]
     with Predicates[F]
     with ProgramTable[F]:

  lazy val ObservingModeGroupMapping =
    ObjectMapping(ObservingModeGroupType)(
      SqlField("key", ObservingModeGroupView.ObservingModeKey, key = true, hidden = true),
      SqlObject("program", Join(ObservingModeGroupView.ProgramId, ProgramTable.Id)),
      SqlObject("observations"),
      SqlObject("observingMode", Join(ObservingModeGroupView.ObservationId, ObservationView.Id))
    )

  lazy val ObservingModeGroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ObservingModeGroupType, "observations", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      Elab.transformChild: child =>
        (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap: (includeDeleted, OFFSET, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit): q =>
            FilterOrderByOffsetLimit(
              pred = and(List(
                Predicates.observation.existence.includeDeleted(includeDeleted),
                OFFSET.fold[Predicate](True)(Predicates.observation.id.gtEql)
              )).some,
              oss    = List(OrderSelection[Observation.Id](ObservationType / "id", true, true)).some,
              offset = none,
              limit  = (limit + 1).some,
              q
            )