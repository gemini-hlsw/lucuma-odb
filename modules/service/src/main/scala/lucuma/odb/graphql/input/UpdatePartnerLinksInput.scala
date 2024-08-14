// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.data.PartnerLink
import lucuma.odb.graphql.binding.*

case class UpdatePartnerLinksInput(
  SET: PartnerLink,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt]
)

object UpdatePartnerLinksInput {

  def binding(path: Path): Matcher[UpdatePartnerLinksInput] =
    val WhereProgramUsersBinding = WhereProgramUser.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        PartnerLinkInput.Binding("SET", rSET),
        WhereProgramUsersBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }

}
