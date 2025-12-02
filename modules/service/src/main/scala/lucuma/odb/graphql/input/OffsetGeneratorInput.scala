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
import lucuma.odb.sequence.data.OffsetGenerator

object OffsetGeneratorInput:

  private val EnumeratedBinding: Matcher[OffsetGenerator.Enumerated] =
    ObjectFieldsBinding.rmap:
      case List(
        TelescopeConfigInput.Binding.List("values", rValues)
      ) => rValues.flatMap: lst =>
        NonEmptyList
          .fromList(lst)
          .fold(OdbError.InvalidArgument("'enumerated' offsets must have at least one offset position".some).asFailure): nel =>
            OffsetGenerator.Enumerated(nel).success

  private val RandomBinding: Matcher[OffsetGenerator.Random] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        OffsetGenerator.Random(size, center.getOrElse(Offset.Zero))

  private val SpiralBinding: Matcher[OffsetGenerator.Spiral] =
    ObjectFieldsBinding.rmap:
      case List(
        AngleInput.Binding("size", rSize),
        OffsetInput.Binding.Option("center", rCenter)
      ) => (rSize, rCenter).parMapN: (size, center) =>
        OffsetGenerator.Spiral(size, center.getOrElse(Offset.Zero))

  private val UniformBinding: Matcher[OffsetGenerator.Uniform] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("cornerA", rCornerA),
        OffsetInput.Binding("cornerB", rCornerB)
      ) => (rCornerA, rCornerB).parMapN: (a, b) =>
        OffsetGenerator.Uniform(a, b)

  val Binding: Matcher[OffsetGenerator] =
    ObjectFieldsBinding.rmap:
      case List(
        EnumeratedBinding.Option("enumerated", rEnumerated),
        RandomBinding.Option("random", rRandom),
        SpiralBinding.Option("spiral", rSpiral),
        UniformBinding.Option("uniform", rUniform)
      ) => (rEnumerated, rRandom, rSpiral, rUniform).parTupled.flatMap:
        case (None,    None,    None,    None   ) => OffsetGenerator.NoGenerator.success
        case (Some(e), None,    None,    None   ) => e.success
        case (None,    Some(r), None,    None   ) => r.success
        case (None,    None,    Some(s), None   ) => s.success
        case (None,    None,    None,    Some(u)) => u.success
        case _                                    => Matcher.validationFailure("At most one offset generator may be specified.")