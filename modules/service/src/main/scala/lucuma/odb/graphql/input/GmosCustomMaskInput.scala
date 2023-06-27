// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.sequence.gmos.GmosFpuMask.Custom
import lucuma.odb.graphql.binding.*

object GmosCustomMaskInput {

  val Binding: Matcher[Custom] =
    ObjectFieldsBinding.rmap {
      case List(
        StringBinding("filename", rFilename),
        GmosCustomSlitWidthBinding("slitWidth", rSlitWidth)
      ) => (rFilename, rSlitWidth).parTupled.flatMap { (filename, slitWidth) =>
        NonEmptyString.from(filename) match {
          case Left(_)  => Result.failure("The GMOS custom FPU mask 'filename' cannot be empty.")
          case Right(n) => Result(Custom(n, slitWidth))
        }
      }
    }

}
