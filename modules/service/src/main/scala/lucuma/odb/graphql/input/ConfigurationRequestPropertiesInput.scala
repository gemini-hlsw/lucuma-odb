// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.odb.graphql.binding.*

case class ConfigurationRequestPropertiesInput(
  status: Option[ConfigurationRequestStatus]
)

object ConfigurationRequestPropertiesInput {

  val Binding: Matcher[ConfigurationRequestPropertiesInput] =
    val StatusBinding = enumeratedBinding[ConfigurationRequestStatus]
    ObjectFieldsBinding.rmap:
      case List(
        (StatusBinding.Option("status", rStatus))
      ) =>
        rStatus.map(ConfigurationRequestPropertiesInput.apply)

}
