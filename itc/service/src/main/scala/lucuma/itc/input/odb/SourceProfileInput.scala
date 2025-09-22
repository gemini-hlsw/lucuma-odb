// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile.*
import lucuma.odb.graphql.binding.*

object SourceProfileInput {

  // convenience projections
  implicit class SourceProfileOps(self: SourceProfile) {
    def point: Result[Point] = self match
      case a: Point => Result(a);
      case _        =>
        Result.failure:
          "Not a point source. To change profile type, please provide a full definition."

    def uniform: Result[Uniform] = self match
      case a: Uniform => Result(a);
      case _          =>
        Result.failure:
          "Not a uniform source. To change profile type, please provide a full definition."

    def gaussian: Result[Gaussian] = self match
      case a: Gaussian => Result(a);
      case _           =>
        Result.failure:
          "Not a gaussian source.  To change profile type, please provide a full definition."
  }

  val Binding: Matcher[SourceProfile] =
    ObjectFieldsBinding.rmap:
      case List(
            SpectralDefinitionInput.Integrated.Binding.Option("point", rPoint),
            SpectralDefinitionInput.Surface.Binding.Option("uniform", rUniform),
            GaussianInput.Binding.Option("gaussian", rGaussian)
          ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {
          case (Some(point), None, None)    => Result(Point(point))
          case (None, Some(uniform), None)  => Result(Uniform(uniform))
          case (None, None, Some(gaussian)) => Result(gaussian)
          case _                            => Result.failure("Expected exactly one of point, uniform, or gaussian.")
        }
}
