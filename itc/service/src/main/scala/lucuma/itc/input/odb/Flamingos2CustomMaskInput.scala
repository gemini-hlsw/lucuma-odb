// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import grackle.Result
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask.Custom
import lucuma.odb.graphql.binding.*

object Flamingos2CustomMaskInput:

  // As with GMOS, the ITC never uses the identity of a custom mask, only its slit width, so the
  // attachment id is parsed and discarded.
  val Binding: Matcher[Custom] =
    ObjectFieldsBinding.rmap:
      case List(
            AttachmentIdBinding.Option("attachmentId", _),
            Flamingos2CustomSlitWidthBinding("slitWidth", rSlitWidth)
          ) =>
        rSlitWidth.flatMap:
          case Flamingos2CustomSlitWidth.Other =>
            Result.failure(
              "Flamingos 2 custom slit width Other is not supported by the ITC, it has no defined width."
            )
          case w                               =>
            Result(Custom(ToBeDefined, w))
