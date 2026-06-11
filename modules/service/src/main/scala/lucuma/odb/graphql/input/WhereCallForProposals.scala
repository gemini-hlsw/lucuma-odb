// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*
import org.typelevel.cats.time.given

object WhereCallForProposals:

  def binding(path: Path): Matcher[Predicate] =
    val WhereIdBinding       = WhereOrder.binding(path / "id",               CallForProposalsIdBinding)
    val WhereSemesterBinding = WhereOrder.binding(path / "semester",         SemesterBinding)
    val WhereStartBinding    = WhereOrder.binding(path / "active" / "start", DateBinding)
    val WhereEndBinding      = WhereOrder.binding(path / "active" / "end",   DateBinding)
    val WhereIsOpenBinding      = WhereBoolean.binding(path / "_isOpen",      BooleanBinding)
    val WhereObservatoryBinding = WhereEq.binding(path / "observatory",       ObservatoryBinding)
    val WhereGeminiBinding      = WhereGeminiCallProperties.binding(path / "gemini")

    lazy val WhereCfpBinding = binding(path)

    ObjectFieldsBinding.rmap:
      case List(
        WhereCfpBinding.List.Option("AND", rAND),
        WhereCfpBinding.List.Option("OR", rOR),
        WhereCfpBinding.Option("NOT", rNOT),

        WhereIdBinding.Option("id", rId),
        WhereSemesterBinding.Option("semester", rSemester),
        WhereStartBinding.Option("activeStart", rStart),
        WhereEndBinding.Option("activeEnd", rEnd),
        WhereIsOpenBinding.Option("isOpen", rIsOpen),
        WhereObservatoryBinding.Option("observatory", rObservatory),
        WhereGeminiBinding.Option("gemini", rGemini)
      ) =>
        (rAND, rOR, rNOT, rId, rSemester, rStart, rEnd, rIsOpen, rObservatory, rGemini).parMapN {
          (AND, OR, NOT, id, semester, start, end, isOpen, observatory, gemini) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              semester,
              start,
              end,
              isOpen,
              observatory,
              gemini
            ).flatten)
        }