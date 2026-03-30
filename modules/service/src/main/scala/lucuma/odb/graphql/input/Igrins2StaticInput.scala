// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.graphql.binding.*

object Igrins2StaticInput:

  val Binding: Matcher[Igrins2StaticConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("saveSVCImages", rSVC),
        Igrins2OffsetModeBinding.Option("offsetMode", rOffsetMode)
      ) =>
        (rSVC, rOffsetMode)
          .parMapN: (svc, offsetMode) =>
            Igrins2StaticConfig(
              Igrins2SVCImages(svc.getOrElse(false)),
              offsetMode.getOrElse(Igrins2OffsetMode.NodAlongSlit)
            )
