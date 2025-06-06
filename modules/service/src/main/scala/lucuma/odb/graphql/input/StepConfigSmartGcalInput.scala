// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.sequence.StepConfig.SmartGcal
import lucuma.odb.graphql.binding.*

object StepConfigSmartGcalInput:
  val Binding: Matcher[SmartGcal] =
    ObjectFieldsBinding.rmap {
      case List(
        SmartGcalTypeBinding("smartGcalType", rSmartGcalType)
      ) => rSmartGcalType.map(SmartGcal.apply)
    }