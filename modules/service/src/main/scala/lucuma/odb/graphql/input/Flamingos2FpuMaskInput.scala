// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2FpuMaskInput:

  val Binding: Matcher[Flamingos2FpuMask] =
    ObjectFieldsBinding.rmap:
      case List(
        Flamingos2CustomMaskInput.Binding.Option("customMask", rCustomMask),
        Flamingos2FpuBinding.Option("builtin", rBuiltin)
      ) => (rCustomMask, rBuiltin).parTupled.flatMap: (custom, builtin) =>
        atMostOne(
          custom.widen[Flamingos2FpuMask]              -> "customMask",
          builtin.map(Flamingos2FpuMask.Builtin.apply) -> "builtin"
        ).map(_.getOrElse(Flamingos2FpuMask.Imaging))
