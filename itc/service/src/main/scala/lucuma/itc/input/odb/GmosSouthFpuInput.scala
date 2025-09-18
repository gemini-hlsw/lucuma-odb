// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*

object GmosSouthFpuInput {

  val Binding: Matcher[GmosFpuMask[GmosSouthFpu]] =
    ObjectFieldsBinding.rmap {
      case List(
            GmosCustomMaskInput.Binding.Option("customMask", rCustomMask),
            GmosSouthFpuBinding.Option("builtin", rBuiltin)
          ) =>
        (rCustomMask, rBuiltin).parTupled.flatMap { (custom, builtin) =>
          oneOrFail(
            custom.widen[GmosFpuMask[GmosSouthFpu]] -> "customMask",
            builtin.map(GmosFpuMask.Builtin(_))     -> "builtin"
          )
        }
    }

}
