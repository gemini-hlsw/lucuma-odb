// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.Eq
import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.eq.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Offset
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.sequence.data.OffsetGenerator
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream

sealed trait Variant[A]:

  import Variant.*

  def variantType: VariantType =
    this match
      case Grouped(_, _, _, _, _)    => VariantType.Grouped
      case Interleaved(_)            => VariantType.Interleaved
      case PreImaging(_, _, _, _, _) => VariantType.PreImaging

  def fold[B](
    fg: Grouped[A]     => B,
    fi: Interleaved[A] => B,
    fp: PreImaging[A]  => B
  ): B =
    this match
      case g @ Grouped(_, _, _, _, _)    => fg(g)
      case i @ Interleaved(_)            => fi(i)
      case p @ PreImaging(_, _, _, _, _) => fp(p)

  def filters: NonEmptyList[A]

  def toGrouped: Grouped[A] =
    fold(identity, i => Grouped.default(i.filters), p => Grouped.default(p.filters))

  def toInterleaved: Interleaved[A] =
    fold(g => Interleaved.default(g.filters), identity, p => Interleaved.default(p.filters))

  def toPreImaging: PreImaging[A] =
    fold(g => PreImaging.default(g.filters), i => PreImaging.default(i.filters), identity)

object Variant:

  case class Grouped[A](
    filters:    NonEmptyList[A],
    order:      WavelengthOrder,
    offsets:    OffsetGenerator,
    skyCount:   NonNegInt,
    skyOffsets: OffsetGenerator
  ) extends Variant[A]

  object Grouped:

    def default[A](filters: NonEmptyList[A]): Grouped[A] =
      Grouped(
        filters    = filters,
        order      = WavelengthOrder.Decreasing,
        offsets    = OffsetGenerator.NoGenerator,
        skyCount   = NonNegInt.MinValue,
        skyOffsets = OffsetGenerator.NoGenerator
      )

    def offsets[A]: Lens[Grouped[A], OffsetGenerator] =
      GenLens[Grouped[A]](_.offsets)

    def skyOffsets[A]: Lens[Grouped[A], OffsetGenerator] =
      GenLens[Grouped[A]](_.skyOffsets)

    given [A: HashBytes]: HashBytes[Grouped[A]] with
      def hashBytes(g: Grouped[A]): Array[Byte] =
        val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
        val out: DataOutputStream      = new DataOutputStream(bao)

        out.write(g.filters.hashBytes)
        out.write(g.order.hashBytes)
        out.write(g.offsets.hashBytes)
        out.write(g.skyCount.hashBytes)
        out.write(g.skyOffsets.hashBytes)

        out.close()
        bao.toByteArray

    given [A: Eq]: Eq[Grouped[A]] =
      Eq.by: g =>
        (
          g.filters,
          g.order,
          g.offsets,
          g.skyCount,
          g.skyOffsets
        )

  def grouped[A]: Prism[Variant[A], Grouped[A]] =
    GenPrism[Variant[A], Grouped[A]]

  def offsets[A]: Optional[Variant[A], OffsetGenerator] =
    grouped.andThen(Grouped.offsets)

  def skyOffsets[A]: Optional[Variant[A], OffsetGenerator] =
    grouped.andThen(Grouped.skyOffsets)

  case class Interleaved[A](
    filters: NonEmptyList[A],
  ) extends Variant[A]

  object Interleaved:
    def default[A](filters: NonEmptyList[A]): Interleaved[A] =
      Interleaved(filters)

    given [A: HashBytes]: HashBytes[Interleaved[A]] with
      def hashBytes(i: Interleaved[A]): Array[Byte] =
        i.filters.hashBytes

    given [A: Eq]: Eq[Interleaved[A]] =
      Eq.by(_.filters)

  case class PreImaging[A](
    filters: NonEmptyList[A],
    offset1: Offset,
    offset2: Offset,
    offset3: Offset,
    offset4: Offset
  ) extends Variant[A]

  object PreImaging:
    def default[A](filters: NonEmptyList[A]): PreImaging[A] =
      PreImaging(
        filters = filters,
        offset1 = Offset.Zero,
        offset2 = Offset.Zero,
        offset3 = Offset.Zero,
        offset4 = Offset.Zero
      )

    given [A: HashBytes]: HashBytes[PreImaging[A]] with
      def hashBytes(p: PreImaging[A]): Array[Byte] =
        Array.concat(
          p.filters.hashBytes,
          p.offset1.hashBytes,
          p.offset2.hashBytes,
          p.offset3.hashBytes,
          p.offset4.hashBytes
        )

    given [A: Eq]: Eq[PreImaging[A]] =
      Eq.by: p =>
        (
          p.filters,
          p.offset1,
          p.offset2,
          p.offset3,
          p.offset4
        )

  case class Fields[A](
    variantType: VariantType,
    filters:     NonEmptyList[A],
    order:       WavelengthOrder,
    skyCount:    NonNegInt,
    offset1:     Offset,
    offset2:     Offset,
    offset3:     Offset,
    offset4:     Offset
  ):
    def toVariant(
      objectGen: OffsetGenerator,
      skyGen:    OffsetGenerator
    ): Variant[A] =
      variantType match
        case VariantType.Grouped     => Grouped(filters, order, objectGen, skyCount, skyGen)
        case VariantType.Interleaved => Interleaved(filters)
        case VariantType.PreImaging  => PreImaging(filters, offset1, offset2, offset3, offset4)


  given [A: HashBytes]: HashBytes[Variant[A]] with
    def hashBytes(v: Variant[A]): Array[Byte] =
      v.fold(_.hashBytes, _.hashBytes, _.hashBytes)

  given [A: Eq]: Eq[Variant[A]] with
    def eqv(x: Variant[A], y: Variant[A]): Boolean =
      (x, y) match
        case (a @ Grouped(_, _, _, _, _), b @ Grouped(_, _, _, _, _))       => a === b
        case (a @ Interleaved(_), b @ Interleaved(_))                       => a === b
        case (a @ PreImaging(_, _, _, _, _), b @ PreImaging(_, _, _, _, _)) => a === b
        case _                                                              => false

  given [A]: Functor[Variant] with
    def map[A, B](fa: Variant[A])(f: A => B): Variant[B] =
      fa match
        case Grouped(filters, order, offsets, skyCount, skyOffsets)  =>
          Grouped(filters.map(f), order, offsets, skyCount, skyOffsets)
        case Interleaved(filters)                                    =>
          Interleaved(filters.map(f))
        case PreImaging(filters, offset1, offset2, offset3, offset4) =>
          PreImaging(filters.map(f), offset1, offset2, offset3, offset4)