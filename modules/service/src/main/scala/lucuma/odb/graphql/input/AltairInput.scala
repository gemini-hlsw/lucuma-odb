// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.graphql.binding.*

object AltairInput:

  val Binding: Matcher[AltairConfiguration] =
    ObjectFieldsBinding.rmap {
      case List(
        AltairModeBinding("mode", rMode),
        FieldLensBinding.Option("fieldLens", rFieldLens),
        CassRotatorBinding("cassRotator", rCassRotator),
        AltairNdFilterBinding("ndFilter", rNdFilter)
      ) => (rMode, rFieldLens, rCassRotator, rNdFilter).parMapN(AltairConfiguration.apply)
    }
