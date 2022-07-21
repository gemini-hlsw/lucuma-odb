// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile._
import lucuma.odb.graphql.util.Bindings._

object SourceProfileInput {

  // convenience projections
  implicit class SourceProfileOps(self: SourceProfile) {
    def point:    Result[Point]    = self match { case a: Point    => Result(a); case _ => Result.failure("Not a point source.") }
    def uniform:  Result[Uniform]  = self match { case a: Uniform  => Result(a); case _ => Result.failure("Not a uniform source.") }
    def gaussian: Result[Gaussian] = self match { case a: Gaussian => Result(a); case _ => Result.failure("Not a gaussian source.") }
  }

  val CreateBinding: Matcher[SourceProfile] =
    ObjectFieldsBinding.rmap {
      case List(
        SpectralDefinitionInput.Integrated.CreateBinding.Option("point", rPoint),
        SpectralDefinitionInput.Surface.CreateBinding.Option("uniform", rUniform),
        GaussianInput.CreateBinding.Option("gaussian", rGaussian),
      ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {
          case (Some(point), None, None)     => Result(Point(point))
          case (None, Some(uniform), None)   => Result(Uniform(uniform))
          case (None, None, Some(gaussian))  => Result(gaussian)
          case _                             => Result.failure("Expected exactly one of point, uniform, or guassian.")
        }
    }

  val EditBinding: Matcher[SourceProfile => Result[SourceProfile]] =
    ObjectFieldsBinding.rmap {
      case List(
        SpectralDefinitionInput.Integrated.EditBinding.Option("point", rPoint),
        SpectralDefinitionInput.Surface.EditBinding.Option("uniform", rUniform),
        GaussianInput.EditBinding.Option("gaussian", rGaussian),
      ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {
          case (Some(f), None, None) => Result(sp => sp.point.flatMap(ps => f(ps.spectralDefinition)).map(Point(_)))
          case (None, Some(f), None) => Result(sp => sp.uniform.flatMap(us => f(us.spectralDefinition)).map(Uniform(_)))
          case (None, None, Some(f)) => Result(sp => sp.gaussian.flatMap(gs => f(gs)))
          case _ => Result.failure("Expected exactly one of point, uniform, or guassian.")
        }
    }

}
