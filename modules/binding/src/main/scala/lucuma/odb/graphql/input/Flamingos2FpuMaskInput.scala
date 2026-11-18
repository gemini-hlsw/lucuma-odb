// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import grackle.Result
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2FpuMaskInput:

  /** @see [[GmosCustomMaskInput.OptionalWidthBinding]] */
  val OptionalWidthBinding: Matcher[(Flamingos2FpuMask, Boolean)] =
    OneOfOrDefaultBinding[(Flamingos2FpuMask, Boolean)]((Flamingos2FpuMask.Imaging, false))(
      "customMask" -> Flamingos2CustomMaskInput.OptionalWidthBinding.map((c, widthless) => (c: Flamingos2FpuMask, widthless)),
      "builtin"    -> Flamingos2FpuBinding.map(f => (Flamingos2FpuMask.Builtin(f): Flamingos2FpuMask, false))
    )

  val Binding: Matcher[Flamingos2FpuMask] =
    OptionalWidthBinding.rmap:
      case (fpu, false) => Result(fpu)
      case (_, true)    => Result.failure(GmosCustomMaskInput.SlitWidthRequiredMessage)
