// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import lucuma.core.model.sequence.f2.F2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2FpuMaskInput:

  val Binding: Matcher[F2FpuMask] =
    ObjectFieldsBinding.rmap:
      case List(
        Flamingos2CustomMaskInput.Binding.Option("customMask", rCustomMask),
        F2FpuBinding.Option("builtin", rBuiltin)
      ) => (rCustomMask, rBuiltin).parTupled.flatMap: (custom, builtin) =>
        atMostOne(
          custom.widen[F2FpuMask]              -> "customMask",
          builtin.map(F2FpuMask.Builtin.apply) -> "builtin"
        ).map(_.getOrElse(F2FpuMask.Imaging))
