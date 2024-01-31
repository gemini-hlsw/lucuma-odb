// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding._

object WhereObservation {

  def binding(path: Path): Matcher[Predicate] = {
    val SubtitleBinding = WhereOptionString.binding(path / "subtitle")
    val WhereOrderObservationIdBinding = WhereOrder.binding[Observation.Id](path / "id", ObservationIdBinding)
    val WhereProgramBinding = WhereProgram.binding(path / "program")
    val StatusBinding = WhereOrder.binding(path / "status", enumeratedBinding[ObsStatus])
    val ActiveStatusBinding = WhereOrder.binding(path / "activeStatus", enumeratedBinding[ObsActiveStatus])
    lazy val WhereObservationBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservationBinding.List.Option("AND", rAND),
        WhereObservationBinding.List.Option("OR", rOR),
        WhereObservationBinding.Option("NOT", rNOT),
        WhereOrderObservationIdBinding.Option("id", rId),
        WhereProgramBinding.Option("program", rProgram),
        SubtitleBinding.Option("subtitle", rSubtitle),
        StatusBinding.Option("status", rStatus),
        ActiveStatusBinding.Option("activeStatus", rActiveStatus)
      ) =>
        (rAND, rOR, rNOT, rId, rProgram, rSubtitle, rStatus, rActiveStatus).parMapN { (AND, OR, NOT, id, program, subtitle, status, activeStatus) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            program,
            subtitle,
            status,
            activeStatus
          ).flatten)
        }
    }
  }

}

