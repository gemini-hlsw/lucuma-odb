// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosFpuMask.Custom
import lucuma.odb.graphql.binding.*

object GmosCustomMaskInput {

  val SlitWidthRequiredMessage: String =
    "A custom mask needs a `slitWidth`."

  /** Refuses a value whose custom mask slit width was left out. */
  def requireSlitWidth[A](m: Matcher[(A, Boolean)]): Matcher[A] =
    m.rmap:
      case (a, false) => Result(a)
      case (_, true)  => Result.failure(SlitWidthRequiredMessage)

  /**
   * The mask, and whether its slit width was left out.  The width of a mask
   * given without one is a stand-in, to be replaced before it is used.
   */
  val OptionalWidthBinding: Matcher[(Custom, Boolean)] =
    ObjectFieldsBinding.rmap {
      case List(
        AttachmentIdBinding.Option("attachmentId", rAttachmentId),
        GmosCustomSlitWidthBinding.Option("slitWidth", rSlitWidth)
      ) => (rAttachmentId, rSlitWidth).parMapN { (attachmentId, slitWidth) =>
        val mask = attachmentId.fold[MaskDefinition](ToBeDefined)(Defined(_))
        (Custom(mask, slitWidth.getOrElse(GmosCustomSlitWidth.CustomWidth_1_00)), slitWidth.isEmpty)
      }
    }

  val Binding: Matcher[Custom] =
    requireSlitWidth(OptionalWidthBinding)

}
