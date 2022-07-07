package lucuma.odb.graphql.snippet
package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.odb.graphql.util.Bindings._

import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines

import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessUnits.FluxDensityContinuum
import lucuma.core.math.BrightnessUnits.LineFlux
import lucuma.core.model.SpectralDefinition
import lucuma.core.math.BrightnessUnits.Brightness
import lucuma.core.math.BrightnessUnits.BrightnessMeasure
import lucuma.core.math.dimensional.Of
import lucuma.core.enum.Band
import lucuma.core.model.UnnormalizedSED
import scala.collection.immutable.SortedMap
import lucuma.core.math.Wavelength
import lucuma.core.model.EmissionLine
import lucuma.core.math.dimensional.Measure
import eu.timepit.refined.types.numeric.PosBigDecimal
import coulomb.Quantity
import lucuma.core.math.units._
import scala.collection.immutable.TreeMap
import cats.kernel.Order

object SourceProfileInput {

  val CreateBinding: Matcher[SourceProfile] = {
    ObjectFieldsBinding.rmap {
      case List(
        SpectralDefinitionInput.Integrated.CreateBinding.Option("point", rPoint),
        SpectralDefinitionSurfaceInput.CreateBinding.Option("uniform", rUniform),
        GaussianInput.CreateBinding.Option("gaussian", rGaussian),
      ) =>
        (rPoint, rUniform, rGaussian).parTupled.flatMap {
          case (Some(point), None, None)     => Result(SourceProfile.Point(point))
          case (None, Some(uniform), None)   => Result(uniform)
          case (None, None, Some(gaussian))  => Result(gaussian)
          case _                             => Result.failure("Expected exactly one of point, uniform, or guassian.")
        }
    }
  }

}

object SpectralDefinitionInput {

  object Integrated {
    val CreateBinding = createBinding(
      BandNormalizedInput.Integrated.CreateBinding,
      EmissionLinesInput.Integrated.CreateBinding,
    )
  }

  def createBinding[A](
    bandNormalized: Matcher[BandNormalized[A]],
    emissionLines: Matcher[EmissionLines[A]],
  ): Matcher[SpectralDefinition[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        bandNormalized.Option("bandNormalized", rBandNormalized),
        emissionLines.Option("emissionLines", rEmissionLines),
      ) =>
        (rBandNormalized, rEmissionLines).parTupled.flatMap {
          case (Some(bandNormalized), None) => Result(bandNormalized)
          case (None, Some(emissionLines))  => Result(emissionLines)
          case _                            => Result.failure("Expected exactly one of bandNormalized or emissionLines.")
        }
    }

}

object SpectralDefinitionSurfaceInput {

  val CreateBinding: Matcher[SourceProfile.Uniform] = ???

}

object GaussianInput {

  val CreateBinding: Matcher[SourceProfile.Gaussian] = ???

}

object BandNormalizedInput {

  object Integrated {
    val CreateBinding = ???
  }

  def createBinding[A](
    brightnesses: Matcher[SortedMap[Band, BrightnessMeasure[A]]]
  ): Matcher[BandNormalized[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        UnnormalizedSedInput.Binding.Option("sed", rSed),
        brightnesses.Option("brightnesses", rBrightnesses),
        brightnesses.Option("editBrightnesses", rEditBrightnesses),
        BandBinding.List.Option("deleteBrightnesses", rDeleteBrightnesses),
      ) =>
        (rSed, rBrightnesses, rEditBrightnesses, rDeleteBrightnesses).parTupled.flatMap {
          case (Some(sed), Some(brightnesses), None, None) => Result(BandNormalized(sed, brightnesses))
          case (Some(sed), Some(brightnesses), _, _)       => Result.warning("editBrightness and deleteBrightness are ignored on creation.", BandNormalized(sed, brightnesses))
          case _                                           => Result.failure("Both sed and brightness are required.")
        }
    }

}

object EmissionLinesInput {

  implicit val WavelengthOrdering: Ordering[Wavelength] =
    Order[Wavelength].toOrdering

  object Integrated {
    val CreateBinding = createBinding[Integrated](
      EmissionLineInput.Integrated.CreateBinding,
      FluxDensityContinuumInput.Integrated.Binding,
    )
  }

  def createBinding[A](
    line: Matcher[(Wavelength, EmissionLine[A])],
    fluxDensityContinuum:  Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]],
  ): Matcher[EmissionLines[A]] =
    ObjectFieldsBinding.rmap {
      case List(
        line.List.Option("lines", rLines),
        line.List.Option("editLines", rEditLines),
        WavelengthInput.Binding.List.Option("deleteLines", rDeleteLines),
        fluxDensityContinuum.Option("fluxDensityContinuum", rFluxDensityContinuum),
      ) =>
        (rLines, rEditLines, rDeleteLines, rFluxDensityContinuum).parTupled.flatMap {
          case (Some(lines), None, None, Some(fluxDensityContinuum)) => Result(EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case (Some(lines), _, _, Some(fluxDensityContinuum))       => Result.warning("editLines and deleteLines are ignored on creation.", EmissionLines(lines.to(TreeMap), fluxDensityContinuum))
          case _                                                     => Result.failure("Both lines and fluxDensityContinuum are required on creation.")
        }
    }

}

object EmissionLineInput {

  object Integrated {
    val CreateBinding = createBinding[Integrated](
      LineFluxInput.Integrated.Binding
    )
  }

  def createBinding[A](
    lineFlux: Matcher[Measure[PosBigDecimal] Of LineFlux[A]]
  ): Matcher[(Wavelength, EmissionLine[A])] =
    ObjectFieldsBinding.rmap {
      case List(
        WavelengthInput.Binding("wavelength", rWavelength),
        LineWidthInput.Binding.Option("lineWidth", rLineWidth),
        lineFlux.Option("lineFlux", rLineFlux),
      ) =>
        (rWavelength, rLineWidth, rLineFlux).parTupled.flatMap {
          case (wavelength, Some(lineWidth), Some(lineFlux)) => Result((wavelength, EmissionLine(lineWidth, lineFlux)))
          // TODO
        }
    }

}

object LineWidthInput {
  val Binding: Matcher[Quantity[PosBigDecimal, KilometersPerSecond]] =
    ???
}

object LineFluxInput {

  object Integrated {
    val Binding = create[Integrated]
  }

  def create[A]: Matcher[Measure[PosBigDecimal] Of LineFlux[A]] =
    ???

}

object BandBrightnessInput {

  def create[A](
    units: Matcher[Unit Of Brightness[A]]
  ): Matcher[(Band, BrightnessMeasure[A])] =
    ???

}

object UnnormalizedSedInput {
  val Binding: Matcher[UnnormalizedSED] =
    ???
}

object WavelengthInput {
  val Binding: Matcher[Wavelength] =
    ???
}

object FluxDensityContinuumInput {

  object Integrated {
    val Binding = binding[Integrated]
  }

  def binding[A]: Matcher[Measure[PosBigDecimal] Of FluxDensityContinuum[A]] =
    ???

}