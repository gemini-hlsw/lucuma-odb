// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input
package sourceprofile

import cats.data.Ior
import grackle.Result
import grackle.syntax.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SourceProfile.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.sourceprofile.SpectralDefinitionInput.matches

object SourceProfileInput {

  // convenience projections
  implicit class SourceProfileOps(self: SourceProfile) {
    def point:    Result[Point]    = self match { case a: Point    => Result(a); case _ => Matcher.validationFailure("Not a point source. To change profile type, please provide a full definition.") }
    def uniform:  Result[Uniform]  = self match { case a: Uniform  => Result(a); case _ => Matcher.validationFailure("Not a uniform source. To change profile type, please provide a full definition.") }
    def gaussian: Result[Gaussian] = self match { case a: Gaussian => Result(a); case _ => Matcher.validationFailure("Not a gaussian source.  To change profile type, please provide a full definition.") }
  }

  val CreateBinding: Matcher[SourceProfile] =
    OneOfBinding(
      "point"    -> SpectralDefinitionInput.Integrated.CreateBinding.map(Point(_)),
      "uniform"  -> SpectralDefinitionInput.Surface.CreateBinding.map(Uniform(_)),
      "gaussian" -> GaussianInput.CreateBinding
    )

  val EditBinding: Matcher[SourceProfile => Result[SourceProfile]] = {
    OneOfBinding(
      "point" -> SpectralDefinitionInput.Integrated.CreateOrEditBinding.map[SourceProfile => Result[SourceProfile]] {
        // If the user provides an input that can be used for editing or replacement, apply the edit if the source profile types match,
        // otherwise interpret it as a replacement.
        case Ior.Both(c, e) =>
          sp =>
            sp.point.toOption.map(_.spectralDefinition)
              // do a replace if the original is bandNormalized and the new is emissionLines, or vice verse
              .filter(_.matches(c))
              .fold(c.success)(e)
              .map(Point(_))
        // If the user provides a full definition then we will replace the source profile
        case Ior.Left(p)    => _ => Point(p).success
        // Otherwise we will try to apply an edit, which may fail.
        case Ior.Right(f)   => sp => sp.point.flatMap(ps => f(ps.spectralDefinition)).map(Point(_))
      },
      "uniform" -> SpectralDefinitionInput.Surface.CreateOrEditBinding.map[SourceProfile => Result[SourceProfile]] {
        case Ior.Both(c, e) =>
          sp =>
            sp.uniform.toOption.map(_.spectralDefinition)
              .filter(_.matches(c))
              .fold(c.success)(e)
              .map(Uniform(_))
        case Ior.Left(u)    => _ => Uniform(u).success
        case Ior.Right(f)   => sp => sp.uniform.flatMap(us => f(us.spectralDefinition)).map(Uniform(_))
      },
      "gaussian" -> GaussianInput.CreateOrEditBinding.map[SourceProfile => Result[SourceProfile]] {
        case Ior.Both(c, e) => sp => sp.gaussian.toOption.fold(c.success)(e)
        case Ior.Left(g)    => _ => g.success
        case Ior.Right(f)   => sp => sp.gaussian.flatMap(f)
      }
    )
  }

}
