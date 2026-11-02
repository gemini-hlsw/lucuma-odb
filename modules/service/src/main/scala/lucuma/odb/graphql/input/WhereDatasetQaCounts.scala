// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.WhereOrder

object WhereDatasetQaCounts:

  def binding(path: Path): Matcher[Predicate] =
    def count(name: String): Matcher[Predicate] =
      WhereOrder.binding[NonNegInt](path / name, NonNegIntBinding)

    val WhereTotal       = count("total")
    val WherePass        = count("pass")
    val WhereUsable      = count("usable")
    val WhereFail        = count("fail")
    val WhereNotAssessed = count("notAssessed")

    ObjectFieldsBinding.rmap:
      case List(
        WhereTotal.Option("total", rTotal),
        WherePass.Option("pass", rPass),
        WhereUsable.Option("usable", rUsable),
        WhereFail.Option("fail", rFail),
        WhereNotAssessed.Option("notAssessed", rNotAssessed)
      ) => (rTotal, rPass, rUsable, rFail, rNotAssessed).parMapN: (total, pass, usable, fail, notAssessed) =>
        and(List(total, pass, usable, fail, notAssessed).flatten)
