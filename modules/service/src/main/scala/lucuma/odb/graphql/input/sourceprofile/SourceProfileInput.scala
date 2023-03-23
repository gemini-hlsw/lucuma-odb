// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.Ior
import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile._
import lucuma.odb.graphql.binding._

object SourceProfileInput {

  // convenience projections
  implicit class SourceProfileOps(self: SourceProfile) {
    def point:    Result[Point]    = self match { case a: Point    => Result(a); case _ => Result.failure("Not a point source. To change profile type, please provide a full definition.") }
    def uniform:  Result[Uniform]  = self match { case a: Uniform  => Result(a); case _ => Result.failure("Not a uniform source. To change profile type, please provide a full definition.") }
    def gaussian: Result[Gaussian] = self match { case a: Gaussian => Result(a); case _ => Result.failure("Not a gaussian source.  To change profile type, please provide a full definition.") }
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
          case _                             => Result.failure("Expected exactly one of point, uniform, or gaussian.")
        }
    }

  val EditBinding: Matcher[SourceProfile => Result[SourceProfile]] = {
    ObjectFieldsBinding.rmap {
      case List(
        SpectralDefinitionInput.Integrated.CreateOrEditBinding.Option("point", rPoint),
        SpectralDefinitionInput.Surface.CreateOrEditBinding.Option("uniform", rUniform),
        GaussianInput.CreateOrEditBinding.Option("gaussian", rGaussian),
      ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {

          // If the user provides an input that can be used for editing or replacement, apply the edit if the source profile types match,
          // otherwise interpret it as a replacement.
          case (Some(Ior.Both(c, e)), None, None) => Result(sp => sp.point.toOption.map(_.spectralDefinition).fold(Result(c))(e).map(Point(_)))
          case (None, Some(Ior.Both(c, e)), None) => Result(sp => sp.uniform.toOption.map(_.spectralDefinition).fold(Result(c))(e).map(Uniform(_)))
          case (None, None, Some(Ior.Both(c, e))) => Result(sp => sp.gaussian.toOption.fold(Result(c))(e))

          // If the user provides a full definition then we will replace the source profile
          case (Some(Ior.Left(p)), None, None) => Result(_ => Result(Point(p)))
          case (None, Some(Ior.Left(u)), None) => Result(_ => Result(Uniform(u)))
          case (None, None, Some(Ior.Left(g))) => Result(_ => Result(g))

          // Otherwise we will try to apply an edit, which may fail.
          case (Some(Ior.Right(f)), None, None) => Result(sp => sp.point.flatMap(ps => f(ps.spectralDefinition)).map(Point(_)))
          case (None, Some(Ior.Right(f)), None) => Result(sp => sp.uniform.flatMap(us => f(us.spectralDefinition)).map(Uniform(_)))
          case (None, None, Some(Ior.Right(f))) => Result(sp => sp.gaussian.flatMap(gs => f(gs)))

          // Otherwise we definitely fail.
          case _ => Result.failure("Expected exactly one of point, uniform, or gaussian.")

        }
    }
  }

}

