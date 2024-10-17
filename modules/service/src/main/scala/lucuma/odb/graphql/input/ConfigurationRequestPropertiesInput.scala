// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.odb.graphql.binding.*
import lucuma.odb.data.ConfigurationRequest

case class ConfigurationRequestPropertiesInput(
  status: Option[ConfigurationRequest.Status]
)

object ConfigurationRequestPropertiesInput {

  val Binding: Matcher[ConfigurationRequestPropertiesInput] =
    val StatusBinding = enumeratedBinding[ConfigurationRequest.Status]
    ObjectFieldsBinding.rmap:
      case List(
        (StatusBinding.Option("status", rStatus))
      ) =>
        rStatus.map(ConfigurationRequestPropertiesInput.apply)

}
