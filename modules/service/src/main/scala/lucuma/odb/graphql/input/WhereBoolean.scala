// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereBoolean {

  def binding(path: Path, binding: Matcher[Boolean]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(binding.Option("EQ", rEQ)) =>
        rEQ.map(_.fold(Predicate.True)(b => Eql(path, Const(b))))
    }

}
