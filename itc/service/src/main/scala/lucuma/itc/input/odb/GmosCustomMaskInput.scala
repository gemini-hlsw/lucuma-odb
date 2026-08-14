// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosFpuMask.Custom
import lucuma.odb.graphql.binding.*

object GmosCustomMaskInput:

  // The ITC never uses the identity of a custom mask we only use the stlit width
  // We can just ignore the param but we need to parse it anyway.
  val Binding: Matcher[Custom] =
    ObjectFieldsBinding.rmap:
      case List(
            AttachmentIdBinding.Option("attachmentId", _),
            GmosCustomSlitWidthBinding("slitWidth", rSlitWidth)
          ) =>
        rSlitWidth.map(Custom(ToBeDefined, _))
