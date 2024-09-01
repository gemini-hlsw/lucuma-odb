// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereOptionBoolean {

  def binding(path: Path, binding: Matcher[Boolean]): Matcher[Predicate] =
    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        binding.Option("EQ", rEq)
      ) => (rIsNull, rEq).mapN {
        (isNull, eq) =>
          and(List(
            isNull.map(IsNull(path, _)),
            eq.map(a => Eql(path, Const(a.some)))
          ).flatten)
    }
  }

}
