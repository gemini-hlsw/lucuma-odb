// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
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

sealed trait Variant:

  import Variant.*

  def variantType: VariantType =
    this match
      case Grouped(_, _, _, _)    => VariantType.Grouped
      case Interleaved(_, _, _)   => VariantType.Interleaved
      case PreImaging(_, _, _, _) => VariantType.PreImaging

  def fold[A](
    fg: Grouped     => A,
    fi: Interleaved => A,
    fp: PreImaging  => A
  ): A =
    this match
      case g @ Grouped(_, _, _, _)    => fg(g)
      case i @ Interleaved(_, _, _)   => fi(i)
      case p @ PreImaging(_, _, _, _) => fp(p)

  def grouped: Option[Grouped] =
    fold(_.some, _ => none, _ => none)

  def interleaved: Option[Interleaved] =
    fold(_ => none, _.some, _ => none)

  def preImaging: Option[PreImaging] =
    fold(_ => none, _ => none, _.some)

object Variant:

  case class Grouped(
    order:      WavelengthOrder,
    offsets:    OffsetGenerator,
    skyCount:   NonNegInt,
    skyOffsets: OffsetGenerator
  ) extends Variant

  object Grouped:

    val Default: Grouped =
      Grouped(
        order      = WavelengthOrder.Increasing,
        offsets    = OffsetGenerator.NoGenerator,
        skyCount   = NonNegInt.MinValue,
        skyOffsets = OffsetGenerator.NoGenerator
      )

    val offsets: Lens[Grouped, OffsetGenerator] =
      GenLens[Grouped](_.offsets)

    def skyOffsets: Lens[Grouped, OffsetGenerator] =
      GenLens[Grouped](_.skyOffsets)

    given HashBytes[Grouped] with
      def hashBytes(g: Grouped): Array[Byte] =
        val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
        val out: DataOutputStream      = new DataOutputStream(bao)

        out.write(g.order.hashBytes)
        out.write(g.offsets.hashBytes)
        out.write(g.skyCount.hashBytes)
        out.write(g.skyOffsets.hashBytes)

        out.close()
        bao.toByteArray

    given Eq[Grouped] =
      Eq.by: g =>
       (
          g.order,
          g.offsets,
          g.skyCount,
          g.skyOffsets
        )

  val grouped: Prism[Variant, Grouped] =
    GenPrism[Variant, Grouped]

  val offsets: Optional[Variant, OffsetGenerator] =
    grouped.andThen(Grouped.offsets)

  val skyOffsets: Optional[Variant, OffsetGenerator] =
    grouped.andThen(Grouped.skyOffsets)

  case class Interleaved(
    offsets:    OffsetGenerator,
    skyCount:   NonNegInt,
    skyOffsets: OffsetGenerator
  ) extends Variant

  object Interleaved:
    val Default: Interleaved =
      Interleaved(
        offsets    = OffsetGenerator.NoGenerator,
        skyCount   = NonNegInt.MinValue,
        skyOffsets = OffsetGenerator.NoGenerator
      )

    given HashBytes[Interleaved] with
      def hashBytes(i: Interleaved): Array[Byte] =
        val bao: ByteArrayOutputStream = new ByteArrayOutputStream(256)
        val out: DataOutputStream      = new DataOutputStream(bao)

        out.write(i.offsets.hashBytes)
        out.write(i.skyCount.hashBytes)
        out.write(i.skyOffsets.hashBytes)

        out.close()
        bao.toByteArray

    given Eq[Interleaved] =
      Eq.by: g =>
       (
          g.offsets,
          g.skyCount,
          g.skyOffsets
        )

  case class PreImaging(
    offset1: Offset,
    offset2: Offset,
    offset3: Offset,
    offset4: Offset
  ) extends Variant

  object PreImaging:
    val Default: PreImaging =
      PreImaging(
        offset1 = Offset.Zero,
        offset2 = Offset.Zero,
        offset3 = Offset.Zero,
        offset4 = Offset.Zero
      )

    given HashBytes[PreImaging] with
      def hashBytes(p: PreImaging): Array[Byte] =
        Array.concat(
          p.offset1.hashBytes,
          p.offset2.hashBytes,
          p.offset3.hashBytes,
          p.offset4.hashBytes
        )

    given Eq[PreImaging] =
      Eq.by: p =>
        (
          p.offset1,
          p.offset2,
          p.offset3,
          p.offset4
        )

  case class Fields(
    variantType: VariantType,
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
    ): Variant =
      variantType match
        case VariantType.Grouped     => Grouped(order, objectGen, skyCount, skyGen)
        case VariantType.Interleaved => Interleaved(objectGen, skyCount, skyGen)
        case VariantType.PreImaging  => PreImaging(offset1, offset2, offset3, offset4)


  given HashBytes[Variant] with
    def hashBytes(v: Variant): Array[Byte] =
      v.fold(_.hashBytes, _.hashBytes, _.hashBytes)

  given Eq[Variant] with
    def eqv(x: Variant, y: Variant): Boolean =
      (x, y) match
        case (a @ Grouped(_, _, _, _),    b @ Grouped(_, _, _, _))    => a === b
        case (a @ Interleaved(_, _, _),   b @ Interleaved(_, _, _))   => a === b
        case (a @ PreImaging(_, _, _, _), b @ PreImaging(_, _, _, _)) => a === b
        case _                                                        => false