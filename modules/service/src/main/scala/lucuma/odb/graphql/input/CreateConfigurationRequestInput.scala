// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

case class CreateConfigurationRequestInput(
  oid: Observation.Id,
  SET: ConfigurationRequestPropertiesInput.Create =
    ConfigurationRequestPropertiesInput.Create.Empty
)

object CreateConfigurationRequestInput {
  val Binding: Matcher[CreateConfigurationRequestInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObsId),
        ConfigurationRequestPropertiesInput.Create.Binding.Option("SET", rProps)
      ) => (rObsId, rProps).parMapN: (oid, props) =>
        CreateConfigurationRequestInput(oid, props.getOrElse(ConfigurationRequestPropertiesInput.Create.Empty))
    }

}
