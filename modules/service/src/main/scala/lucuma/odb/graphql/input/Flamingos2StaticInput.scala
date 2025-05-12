// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.odb.graphql.binding.*

object Flamingos2StaticInput:

  val Binding: Matcher[F2StaticConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        MosPreImagingBinding.Option("mosPreImaging", rPreImaging),
        BooleanBinding.Option("useElectronicOffsetting", rEOffsetting)
      ) =>
        (rPreImaging, rEOffsetting)
          .parMapN: (preImage, eOffset) =>
            F2StaticConfig(
              preImage.getOrElse(MosPreImaging.IsNotMosPreImaging),
              eOffset.getOrElse(false)
            )