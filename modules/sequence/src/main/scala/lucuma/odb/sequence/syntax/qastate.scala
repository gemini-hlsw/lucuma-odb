// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.syntax

import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetQaState.*

trait ToDatasetQaStateOps {

  extension (self: DatasetQaState)
    def isPassing: Boolean =
      self match
        case Pass          => true
        case Usable | Fail => false

  extension (self: DatasetQaState.type)
    def orPassing(d: Option[DatasetQaState]): DatasetQaState =
      d.getOrElse(Pass)

}

object qastate extends ToDatasetQaStateOps