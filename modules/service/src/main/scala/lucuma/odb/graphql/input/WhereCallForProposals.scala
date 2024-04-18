// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.CallForProposals
import lucuma.odb.data.CallForProposalsType
import lucuma.odb.graphql.binding.*

object WhereCallForProposals {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereIdBinding       = WhereOrder.binding(path / "id",               CallForProposalsIdBinding)
    val WhereTypeBinding     = WhereEq.binding(   path / "type",             CallForProposalsTypeBinding)
    val WhereSemesterBinding = WhereOrder.binding(path / "semester",         SemesterBinding)
    val WhereStartBinding    = WhereOrder.binding(path / "active" / "start", TimestampBinding)
    val WhereEndBinding      = WhereOrder.binding(path / "active" / "end",   TimestampBinding)

    lazy val WhereCfpBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereCfpBinding.List.Option("AND", rAND),
        WhereCfpBinding.List.Option("OR", rOR),
        WhereCfpBinding.Option("NOT", rNOT),

        WhereIdBinding.Option("id", rId),
        WhereTypeBinding.Option("type", rType),
        WhereSemesterBinding.Option("semester", rSemester),
        WhereStartBinding.Option("activeStart", rStart),
        WhereEndBinding.Option("activeEnd", rEnd)
      ) =>
        (rAND, rOR, rNOT, rId, rType, rSemester, rStart, rEnd).parMapN {
          (AND, OR, NOT, id, cfpType, semester, start, end) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              cfpType,
              semester,
              start,
              end
            ).flatten)
        }
    }
  }

}

