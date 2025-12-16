// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.math.Offset
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.core.geom.OffsetGenerator

object TelescopeConfigGeneratorInput:

  private val EnumeratedBinding: Matcher[TelescopeConfigGenerator.Enumerated] =
    ObjectFieldsBinding.rmap:
      case List(
        TelescopeConfigInput.Binding.List("values", rValues)
      ) => rValues.flatMap: lst =>
        NonEmptyList
          .fromList(lst)
          .fold(OdbError.InvalidArgument("'enumerated' offsets must have at least one offset position".some).asFailure): nel =>
            TelescopeConfigGenerator.Enumerated(nel).success

  private val RandomBinding: Matcher[TelescopeConfigGenerator.FromOffsetGenerator] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Random(size, center.getOrElse(Offset.Zero)))

  private val SpiralBinding: Matcher[TelescopeConfigGenerator.FromOffsetGenerator] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Spiral(size, center.getOrElse(Offset.Zero)))

  private val UniformBinding: Matcher[TelescopeConfigGenerator.FromOffsetGenerator] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("cornerA", rCornerA),
        OffsetInput.Binding("cornerB", rCornerB)
      ) => (rCornerA, rCornerB).parMapN: (a, b) =>
        TelescopeConfigGenerator.FromOffsetGenerator(OffsetGenerator.Uniform(a, b))

  val Binding: Matcher[TelescopeConfigGenerator] =
    ObjectFieldsBinding.rmap:
      case List(
        EnumeratedBinding.Option("enumerated", rEnumerated),
        RandomBinding.Option("random", rRandom),
        SpiralBinding.Option("spiral", rSpiral),
        UniformBinding.Option("uniform", rUniform)
      ) => (rEnumerated, rRandom, rSpiral, rUniform).parTupled.flatMap:
        case (None,    None,    None,    None   ) => TelescopeConfigGenerator.NoGenerator.success
        case (Some(e), None,    None,    None   ) => e.success
        case (None,    Some(r), None,    None   ) => r.success
        case (None,    None,    Some(s), None   ) => s.success
        case (None,    None,    None,    Some(u)) => u.success
        case _                                    => Matcher.validationFailure("At most one offset generator may be specified.")