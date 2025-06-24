// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.odb.graphql.binding.*

object RegionInput:

  case class Create(raArc: Arc[RightAscension], decArc: Arc[Declination])
  case class Edit(raArc: Option[Arc[RightAscension]], decArc: Option[Arc[Declination]])

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap:
      case List(
        RightAscensionArcInput.Binding("rightAscensionArc", rRA),
        DeclinationArcInput.Binding("declinationArc", rDec)
      ) => (rRA, rDec).mapN(Create.apply)

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap:
      case List(
        RightAscensionArcInput.Binding.Option("rightAscensionArc", rRA),
        DeclinationArcInput.Binding.Option("declinationArc", rDec)
      ) => (rRA, rDec).mapN(Edit.apply)
