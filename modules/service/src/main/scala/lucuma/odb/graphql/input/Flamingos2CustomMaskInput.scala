// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.Defined
import lucuma.core.model.MaskDefinition
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask.Custom
import lucuma.odb.graphql.binding.*

object Flamingos2CustomMaskInput:

  val Binding: Matcher[Custom] =
    ObjectFieldsBinding.rmap:
      case List(
        AttachmentIdBinding.Option("attachmentId", rAttachmentId),
        Flamingos2CustomSlitWidthBinding("slitWidth", rSlitWidth)
      ) => (rAttachmentId, rSlitWidth).parTupled.flatMap: (attachmentId, slitWidth) =>
        Result(Custom(attachmentId.fold[MaskDefinition](ToBeDefined)(Defined(_)), slitWidth))
