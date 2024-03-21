// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Target
import lucuma.odb.graphql.binding.*


//# Add or delete targets in an asterism
//input EditAsterismsPatchInput {
//  ADD: [TargetId!]
//  DELETE: [TargetId!]
//}

final case class EditAsterismsPatchInput(
  ADD:    Option[List[Target.Id]],
  DELETE: Option[List[Target.Id]]
)

object EditAsterismsPatchInput {

  val Binding: Matcher[EditAsterismsPatchInput] =
    ObjectFieldsBinding.rmap {
      case List(
        TargetIdBinding.List.Option("ADD", rADD),
        TargetIdBinding.List.Option("DELETE", rDELETE)
      ) =>
        (rADD, rDELETE).parMapN(apply)

    }
}