// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.*

object CreateConfigurationRequestInput {

 val Binding: Matcher[Observation.Id] =
    ObjectFieldsBinding.rmap {
      case List(
        ObservationIdBinding("observationId", rObsId)
      ) => rObsId
    }

}
