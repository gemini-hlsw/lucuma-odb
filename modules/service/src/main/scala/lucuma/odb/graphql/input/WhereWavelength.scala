// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*

object WhereWavelength {

  def binding(path: Path): Matcher[Predicate] = {
    val PicometersBinding  = WhereOrder.binding(path / "picometers",  PosIntBinding)
    val AngstromsBinding   = WhereOrder.binding(path / "angstroms",   PosBigDecimalBinding)
    val NanometersBinding  = WhereOrder.binding(path / "nanometers",  PosBigDecimalBinding)
    val MicrometersBinding = WhereOrder.binding(path / "micrometers", PosBigDecimalBinding)

    lazy val WhereWavelengthBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereWavelengthBinding.List.Option("AND", rAND),
        WhereWavelengthBinding.List.Option("OR", rOR),
        WhereWavelengthBinding.Option("NOT", rNOT),

        PicometersBinding.Option("picometers", rPicometers),
        AngstromsBinding.Option("angstroms", rAngstroms),
        NanometersBinding.Option("nanometers", rNanometers),
        MicrometersBinding.Option("micrometers", rMicrometers)
      ) =>
        (rAND, rOR, rNOT, rPicometers, rAngstroms, rNanometers, rMicrometers).parMapN {
          (AND, OR, NOT, picometers, angstroms, nanometers, micrometers) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              picometers,
              angstroms,
              nanometers,
              micrometers
            ).flatten)
        }
    }
  }

}
