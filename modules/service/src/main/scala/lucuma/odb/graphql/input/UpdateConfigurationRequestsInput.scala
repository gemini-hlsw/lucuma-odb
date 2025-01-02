// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import lucuma.odb.graphql.binding.*

case class UpdateConfigurationRequestsInput(
  SET:   ConfigurationRequestPropertiesInput.Update,
  WHERE: Option[Predicate],
  LIMIT: Option[NonNegInt]
)

object UpdateConfigurationRequestsInput {

  def binding(path: Path) = {
    val WhereConfigurationRequestsBinding = WhereConfigurationRequest.binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        ConfigurationRequestPropertiesInput.Update.Binding("SET", rSET),
        WhereConfigurationRequestsBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      ) =>
        (rSET, rWHERE, rLIMIT).parMapN(apply)
    }
  }

}
