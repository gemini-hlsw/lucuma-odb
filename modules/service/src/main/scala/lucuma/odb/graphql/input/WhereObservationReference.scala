// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PosIntBinding

object WhereObservationReference {

  def binding(path: Path): Matcher[Predicate] = {
    // match observation reference labels on the String value so we can do
    // comparisons like 'LIKE: "G-2024A-0001-Q-000?"'.
    val WhereLabel       = WhereString.binding(path / "labelString")
    val WhereProgramRef  = WhereProgramReference.binding(path / "program")
    val WhereIndex       = WhereOrder.binding[PosInt](path / "index", PosIntBinding)

    ObjectFieldsBinding.rmap {
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereLabel.Option("label", rRef),
        WhereProgramRef.Option("program", rProgram),
        WhereIndex.Option("index", rIndex),
      ) => (rIsNull, rRef, rProgram, rIndex).parMapN {
        (isNull, ref, program, index) =>
          and(List(
            isNull.map(IsNull(path / "id", _)),
            ref,
            program,
            index
          ).flatten)
      }
    }
  }

}
