// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.enums.TelescopeConfigGeneratorType
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

sealed trait TelescopeConfigGeneratorInput:
  def generatorType: TelescopeConfigGeneratorType
  def _center:        Offset       = Offset.Zero
  def _size:          Angle        = Angle.Angle0
  def _cornerA:       Offset       = Offset.Zero
  def _cornerB:       Offset       = Offset.Zero
  def _seed:          Option[Long] = None

object TelescopeConfigGeneratorInput:

  case object NoGeneratorInput extends TelescopeConfigGeneratorInput:
    override def generatorType: TelescopeConfigGeneratorType =
      TelescopeConfigGeneratorType.NoGenerator

  case class EnumeratedInput(
    values: NonEmptyList[TelescopeConfig]
  ) extends TelescopeConfigGeneratorInput:
    override def generatorType: TelescopeConfigGeneratorType =
      TelescopeConfigGeneratorType.Enumerated

  private val EnumeratedBinding: Matcher[TelescopeConfigGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        TelescopeConfigInput.Binding.List("values", rValues)
      ) => rValues.flatMap: lst =>
        NonEmptyList
          .fromList(lst)
          .fold(OdbError.InvalidArgument("'enumerated' offsets must have at least one offset position".some).asFailure): nel =>
            EnumeratedInput(nel).success

  case class RandomInput(
    size:   Angle,
    center: Option[Offset],
    seed:   Option[Long]
  ) extends TelescopeConfigGeneratorInput:
    override def generatorType: TelescopeConfigGeneratorType =
      TelescopeConfigGeneratorType.Random

    override def _size: Angle        = size
    override def _center: Offset     = center.getOrElse(Offset.Zero)
    override def _seed: Option[Long] = seed

  private val RandomBinding: Matcher[TelescopeConfigGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter),
        LongBinding.Option("seed", rSeed)
      ) => (rSize, rCenter, rSeed).parMapN: (size, center, seed) =>
        RandomInput(size, center, seed)

  case class SpiralInput(
    size:   Angle,
    center: Option[Offset],
    seed:   Option[Long]
  ) extends TelescopeConfigGeneratorInput:
    override def generatorType: TelescopeConfigGeneratorType =
      TelescopeConfigGeneratorType.Spiral

    override def _size: Angle        = size
    override def _center: Offset     = center.getOrElse(Offset.Zero)
    override def _seed: Option[Long] = seed

  private val SpiralBinding: Matcher[TelescopeConfigGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter),
        LongBinding.Option("seed", rSeed)
      ) => (rSize, rCenter, rSeed).parMapN: (size, center, seed) =>
        SpiralInput(size, center, seed)

  case class UniformInput(
    cornerA: Offset,
    cornerB: Offset
  ) extends TelescopeConfigGeneratorInput:
    override def generatorType: TelescopeConfigGeneratorType =
      TelescopeConfigGeneratorType.Uniform

    override def _cornerA: Offset = cornerA
    override def _cornerB: Offset = cornerB

  private val UniformBinding: Matcher[TelescopeConfigGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("cornerA", rCornerA),
        OffsetInput.Binding("cornerB", rCornerB)
      ) => (rCornerA, rCornerB).parMapN: (a, b) =>
        UniformInput(a, b)

  val Binding: Matcher[TelescopeConfigGeneratorInput] =
    ObjectFieldsBinding.rmap:
      case List(
        EnumeratedBinding.Option("enumerated", rEnumerated),
        RandomBinding.Option("random", rRandom),
        SpiralBinding.Option("spiral", rSpiral),
        UniformBinding.Option("uniform", rUniform)
      ) => (rEnumerated, rRandom, rSpiral, rUniform).parTupled.flatMap:
        case (None,    None,    None,    None   ) => NoGeneratorInput.success
        case (Some(e), None,    None,    None   ) => e.success
        case (None,    Some(r), None,    None   ) => r.success
        case (None,    None,    Some(s), None   ) => s.success
        case (None,    None,    None,    Some(u)) => u.success
        case _                                    => Matcher.validationFailure("At most one offset generator may be specified.")