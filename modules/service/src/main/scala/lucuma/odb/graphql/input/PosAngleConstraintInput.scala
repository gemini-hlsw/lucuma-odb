// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.math.Angle
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosAngleConstraintModeBinding

final case class PosAngleConstraintInput(
  mode:  Option[PosAngleConstraintMode],
  angle: Option[Angle]
)

object PosAngleConstraintInput {

  val Binding: Matcher[PosAngleConstraintInput] =
    ObjectFieldsBinding.rmap {
      case List(
        PosAngleConstraintModeBinding.Option("mode", rMode),
        AngleInput.Binding.Option("angle", rAngle)
      ) =>
        (rMode, rAngle).parMapN(apply)
    }

}
