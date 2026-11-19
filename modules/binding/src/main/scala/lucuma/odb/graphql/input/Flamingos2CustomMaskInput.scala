// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask.Custom
import lucuma.odb.graphql.binding.*

object Flamingos2CustomMaskInput:

  /** @see [[GmosCustomMaskInput.OptionalWidthBinding]] */
  val OptionalWidthBinding: Matcher[(Custom, Boolean)] =
    ObjectFieldsBinding.rmap:
      case List(
        AttachmentIdBinding.Option("attachmentId", rAttachmentId),
        Flamingos2CustomSlitWidthBinding.Option("slitWidth", rSlitWidth)
      ) => (rAttachmentId, rSlitWidth).parMapN: (attachmentId, slitWidth) =>
        val mask = attachmentId.fold[MaskDefinition](ToBeDefined)(Defined(_))
        (Custom(mask, slitWidth.getOrElse(Flamingos2CustomSlitWidth.CustomWidth_2_pix)), slitWidth.isEmpty)

  val Binding: Matcher[Custom] =
    GmosCustomMaskInput.requireSlitWidth(OptionalWidthBinding)
