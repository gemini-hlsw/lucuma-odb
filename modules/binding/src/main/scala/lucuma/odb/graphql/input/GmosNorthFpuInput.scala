// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.enums.GmosNorthFpu
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*

object GmosNorthFpuInput {

  val Binding: Matcher[GmosFpuMask[GmosNorthFpu]] =
    OneOfBinding(
      "customMask" -> GmosCustomMaskInput.Binding,
      "builtin"    -> GmosNorthFpuBinding.map(GmosFpuMask.Builtin(_))
    )

}
