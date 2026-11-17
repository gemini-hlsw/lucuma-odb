// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.model.Group
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.WhereOptionString
import lucuma.odb.graphql.binding.WhereOrder

object WhereGroup {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereOrderGroupIdBinding = WhereOrder.binding[Group.Id](path / "id", GroupIdBinding)
    val NameBinding = WhereOptionString.binding(path / "name")
    val DescriptionBinding = WhereOptionString.binding(path / "description")
    val ProgramBinding = WhereProgram.binding(path / "program")
    val StandingBinding = WhereCalculatedGroupStanding.binding(path / "standing")
    val WaiversBinding = WhereGroupConstraintWaiver.binding(path / "waivers")
    val HasWaiverBinding = WhereBoolean.binding(path / "hasWaiver", BooleanBinding)
    val HasActiveWaiverBinding = WhereBoolean.binding(path / "hasActiveWaiver", BooleanBinding)
    lazy val WhereGroupBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereGroupBinding.List.Option("AND", rAND),
        WhereGroupBinding.List.Option("OR", rOR),
        WhereGroupBinding.Option("NOT", rNOT),
        WhereOrderGroupIdBinding.Option("id", rId),
        NameBinding.Option("name", rName),
        DescriptionBinding.Option("description", rDescription),
        ProgramBinding.Option("program", rProgram),
        StandingBinding.Option("standing", rStanding),
        WaiversBinding.Option("waivers", rWaivers),
        HasWaiverBinding.Option("hasWaiver", rHasWaiver),
        HasActiveWaiverBinding.Option("hasActiveWaiver", rHasActiveWaiver),
      ) =>
        (rAND, rOR, rNOT, rId, rName, rDescription, rProgram, rStanding, rWaivers, rHasWaiver, rHasActiveWaiver).parMapN { (AND, OR, NOT, id, name, description, program, standing, waivers, hasWaiver, hasActiveWaiver) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            name,
            description,
            program,
            standing,
            waivers,
            hasWaiver,
            hasActiveWaiver,
          ).flatten)
        }
    }
  }

}

