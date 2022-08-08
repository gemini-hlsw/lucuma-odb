// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.parallel._
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import lucuma.core.enums.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.util.Bindings._

object WhereObservation {

  private val ProgramIdBinding: Matcher[Predicate] =
    WhereOrder.ProgramIdWithPath("programId")

  private val SubtitleBinding: Matcher[Predicate] =
    WhereOptionString.binding(UniquePath(List("subtitle")))

  private val StatusBinding: Matcher[Predicate] =
    WhereOrder.SimpleBinding[ObsStatus]("status", enumeratedBinding)

  private val ActiveStatusBinding: Matcher[Predicate] =
    WhereOrder.SimpleBinding[ObsActiveStatus]("activeStatus", enumeratedBinding)

  val Binding: Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservation.Binding.List.Option("AND", rAND),
        WhereObservation.Binding.List.Option("OR", rOR),
        WhereObservation.Binding.Option("NOT", rNOT),
        WhereOrder.ObservationId.Option("id", rId),
        ProgramIdBinding.Option("programId", rPid),
        SubtitleBinding.Option("subtitle", rSubtitle),
        StatusBinding.Option("status", rStatus),
        ActiveStatusBinding.Option("activeStatus", rActiveStatus)
      ) =>
        (rAND, rOR, rNOT, rId, rPid, rSubtitle, rStatus, rActiveStatus).parMapN { (AND, OR, NOT, id, pid, subtitle, status, activeStatus) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not),
            id,
            pid,
            subtitle,
            status,
            activeStatus
          ).flatten)
        }
    }

}
