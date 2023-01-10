// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.mapping.ObservationMapping

object WhereObservation {

  def binding(path: Path): Matcher[Predicate] = {
    val SubtitleBinding = WhereOptionString.binding(path / "subtitle")
    val WhereOrderObservationIdBinding = WhereOrder.binding[Observation.Id](path / "id", ObservationIdBinding)
    val StatusBinding = WhereOrder.binding(path / "status", enumeratedBinding[ObsStatus])
    val ActiveStatusBinding = WhereOrder.binding(path / "activeStatus", enumeratedBinding[ObsActiveStatus])
    lazy val WhereObservationBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservationBinding.List.Option("AND", rAND),
        WhereObservationBinding.List.Option("OR", rOR),
        WhereObservationBinding.Option("NOT", rNOT),
        WhereOrderObservationIdBinding.Option("id", rId),
        SubtitleBinding.Option("subtitle", rSubtitle),
        StatusBinding.Option("status", rStatus),
        ActiveStatusBinding.Option("activeStatus", rActiveStatus)
      ) =>
        (rAND, rOR, rNOT, rId, rSubtitle, rStatus, rActiveStatus).parMapN { (AND, OR, NOT, id, subtitle, status, activeStatus) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            subtitle,
            status,
            activeStatus
          ).flatten)
        }
    }
  }

}

