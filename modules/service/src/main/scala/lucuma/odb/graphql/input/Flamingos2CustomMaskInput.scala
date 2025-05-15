// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask.Custom
import lucuma.odb.graphql.binding.*

object Flamingos2CustomMaskInput:

  val Binding: Matcher[Custom] =
    ObjectFieldsBinding.rmap:
      case List(
        StringBinding("filename", rFilename),
        Flamingos2CustomSlitWidthBinding("slitWidth", rSlitWidth)
      ) => (rFilename, rSlitWidth).parTupled.flatMap: (filename, slitWidth) =>
        NonEmptyString.from(filename) match
          case Left(_)  => Matcher.validationFailure("The Flamingos 2 custom FPU mask 'filename' cannot be empty.")
          case Right(n) => Result(Custom(n, slitWidth))