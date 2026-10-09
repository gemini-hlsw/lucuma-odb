// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2FpuMaskInput:

  val Binding: Matcher[Flamingos2FpuMask] =
    OneOfOrDefaultBinding(Flamingos2FpuMask.Imaging)(
      "customMask" -> Flamingos2CustomMaskInput.Binding,
      "builtin"    -> Flamingos2FpuBinding.map(Flamingos2FpuMask.Builtin.apply)
    )
