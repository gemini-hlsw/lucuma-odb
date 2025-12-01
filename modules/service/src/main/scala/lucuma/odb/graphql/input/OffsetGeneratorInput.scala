// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.OffsetGeneratorType
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

sealed trait OffsetGeneratorInput:

  def offsetGeneratorType: OffsetGeneratorType =
    this match
      case OffsetGeneratorInput.NoGenerator   => OffsetGeneratorType.NoGenerator
      case OffsetGeneratorInput.Enumerated(_) => OffsetGeneratorType.Enumerated
      case OffsetGeneratorInput.Random(_, _)  => OffsetGeneratorType.Random
      case OffsetGeneratorInput.Spiral(_, _)  => OffsetGeneratorType.Spiral
      case OffsetGeneratorInput.Uniform(_, _) => OffsetGeneratorType.Uniform

object OffsetGeneratorInput:

  case object NoGenerator extends OffsetGeneratorInput

  case class Enumerated(
    values: NonEmptyList[TelescopeConfig]
  ) extends OffsetGeneratorInput

//  object Enumerated:
//
//    val values: Lens[Enumerated, NonEmptyList[TelescopeConfig]] =
//      GenLens[Enumerated](_.values)

  case class Random(
    size:   Angle,
    center: Offset
  ) extends OffsetGeneratorInput

  case class Spiral(
    size:   Angle,
    center: Offset
  ) extends OffsetGeneratorInput

  case class Uniform(
    cornerA: Offset,
    cornerB: Offset
  ) extends OffsetGeneratorInput

  object Uniform:
    val cornerA: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerA)
    val cornerB: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerB)

//  val enumerated: Prism[OffsetGeneratorInput, Enumerated] = GenPrism[OffsetGeneratorInput, Enumerated]
//
//  val offsets: Optional[OffsetGeneratorInput, NonEmptyList[TelescopeConfig]] =
//    enumerated.andThen(Enumerated.values)

  val uniform: Prism[OffsetGeneratorInput, Uniform]   = GenPrism[OffsetGeneratorInput, Uniform]
  val cornerA: Optional[OffsetGeneratorInput, Offset] = uniform.andThen(Uniform.cornerA)
  val cornerB: Optional[OffsetGeneratorInput, Offset] = uniform.andThen(Uniform.cornerB)

  val size: Optional[OffsetGeneratorInput, Angle] =
    Optional.apply[OffsetGeneratorInput, Angle] {
      case OffsetGeneratorInput.Random(s, _) => s.some
      case OffsetGeneratorInput.Spiral(s, _) => s.some
      case _                                 => none
    } { s => o =>
      o match
        case OffsetGeneratorInput.Random(_, c) => OffsetGeneratorInput.Random(s, c)
        case OffsetGeneratorInput.Spiral(_, c) => OffsetGeneratorInput.Spiral(s, c)
        case i                                 => i
    }

  val center: Optional[OffsetGeneratorInput, Offset] =
    Optional.apply[OffsetGeneratorInput, Offset] {
      case OffsetGeneratorInput.Random(_, c) => c.some
      case OffsetGeneratorInput.Spiral(_, c) => c.some
      case _                                 => none
    } { c => o =>
      o match
        case OffsetGeneratorInput.Random(s, _) => OffsetGeneratorInput.Random(s, c)
        case OffsetGeneratorInput.Spiral(s, _) => OffsetGeneratorInput.Spiral(s, c)
        case i                                 => i
    }

  private val EnumeratedBinding: Matcher[Enumerated] =
    ObjectFieldsBinding.rmap:
      case List(
        TelescopeConfigInput.Binding.List("values", rValues)
      ) => rValues.flatMap: lst =>
        NonEmptyList
          .fromList(lst)
          .fold(OdbError.InvalidArgument("'enumerated' offsets must have at least one offset position".some).asFailure): nel =>
            Enumerated(nel).success

  private val RandomBinding: Matcher[Random] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        Random(size, center.getOrElse(Offset.Zero))

  private val SpiralBinding: Matcher[Spiral] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        Spiral(size, center.getOrElse(Offset.Zero))

  private val UniformBinding: Matcher[Uniform] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("cornerA", rCornerA),
        OffsetInput.Binding("cornerB", rCornerB)
      ) => (rCornerA, rCornerB).parMapN: (a, b) =>
        Uniform(a, b)

  val Binding: Matcher[OffsetGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        EnumeratedBinding.Option("enumerated", rEnumerated),
        RandomBinding.Option("random", rRandom),
        SpiralBinding.Option("spiral", rSpiral),
        UniformBinding.Option("uniform", rUniform)
      ) => (rEnumerated, rRandom, rSpiral, rUniform).parTupled.flatMap:
        case (None,    None,    None,    None   ) => NoGenerator.success
        case (Some(e), None,    None,    None   ) => e.success
        case (None,    Some(r), None,    None   ) => r.success
        case (None,    None,    Some(s), None   ) => s.success
        case (None,    None,    None,    Some(u)) => u.success
        case _                                    => Matcher.validationFailure("At most one offset generator may be specified.")