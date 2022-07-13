package lucuma.odb.graphql.snippet
package input
package sourceprofile

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.util.Bindings._

object SourceProfileInput {

  val CreateBinding: Matcher[SourceProfile] = {
    ObjectFieldsBinding.rmap {
      case List(
        SpectralDefinitionInput.Integrated.CreateBinding.Option("point", rPoint),
        SpectralDefinitionInput.Surface.CreateBinding.Option("uniform", rUniform),
        GaussianInput.CreateBinding.Option("gaussian", rGaussian),
      ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {
          case (Some(point), None, None)     => Result(SourceProfile.Point(point))
          case (None, Some(uniform), None)   => Result(SourceProfile.Uniform(uniform))
          case (None, None, Some(gaussian))  => Result(gaussian)
          case _                             => Result.failure("Expected exactly one of point, uniform, or guassian.")
        }
    }
  }

}
