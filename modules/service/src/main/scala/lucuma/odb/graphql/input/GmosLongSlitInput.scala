// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import coulomb.Quantity
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Picometer
import lucuma.core.model.SourceProfile
import lucuma.core.optics.Format
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.gmos.longslit.GmosLongSlitConfig

import scala.util.Try
import scala.util.control.Exception.*

object GmosLongSlitInput {

  val WavelengthDithersFormat: Format[String, List[Quantity[Int, Picometer]]] =
    Format(
      s => Try(
        s.split(",").toList.map { d =>
          Quantity[Picometer](BigDecimal(d).bigDecimal.movePointRight(3).intValueExact)
        }
      ).toOption,
      _.map(w => BigDecimal(w.value).bigDecimal.movePointLeft(3).toPlainString).intercalate(",")
    )

  val SpatialOffsetsFormat: Format[String, List[Q]] =
    Format(
      s => Try(
        s.split(",").toList.map { q =>
          Q.signedDecimalArcseconds.reverseGet(BigDecimal(q))
        }
      ).toOption,
      _.map(q => Angle.signedDecimalArcseconds.get(q.toAngle).bigDecimal.toPlainString).intercalate(",")
    )

  sealed trait Create[G, F, U] {
    def grating: G
    def filter:  Option[F]
    def fpu:     U
    def common:  Create.Common
  }

  object Create {

    final case class Common(
      centralWavelength:      Wavelength,
      explicitXBin:           Option[GmosXBinning],
      explicitYBin:           Option[GmosYBinning],
      explicitAmpReadMode:    Option[GmosAmpReadMode],
      explicitAmpGain:        Option[GmosAmpGain],
      explicitRoi:            Option[GmosRoi],
      explicitλDithers:       Option[List[Quantity[Int, Picometer]]],
      explicitSpatialOffsets: Option[List[Q]]
    ) {

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Option[String] =
        explicitλDithers.map(WavelengthDithersFormat.reverseGet)

      val formattedSpatialOffsets: Option[String] =
        explicitSpatialOffsets.map(SpatialOffsetsFormat.reverseGet)

    }

    private abstract class AbstractLongSlitConfig[G, F, U](c: Create[G, F, U]) extends GmosLongSlitConfig[G, F, U] {
      override def grating: G = c.grating
      override def filter: Option[F] = c.filter
      override def fpu: U = c.fpu
      override def centralWavelength: Wavelength = c.common.centralWavelength

      override def explicitXBin: Option[GmosXBinning] = c.common.explicitXBin
      override def explicitYBin: Option[GmosYBinning] = c.common.explicitYBin
      override def explicitAmpReadMode: Option[GmosAmpReadMode] = c.common.explicitAmpReadMode
      override def explicitAmpGain: Option[GmosAmpGain] = c.common.explicitAmpGain
      override def explicitRoi: Option[GmosRoi] = c.common.explicitRoi
      override def explicitWavelengthDithers: Option[List[WavelengthDither]] =
        // TODO: change this in Common to WavelengthDither
        c.common.explicitλDithers.map(_.map(WavelengthDither.picometers.get))

      override def explicitSpatialOffsets: Option[List[Q]] = c.common.explicitSpatialOffsets
    }


    final case class North(
      grating: GmosNorthGrating,
      filter:  Option[GmosNorthFilter],
      fpu:     GmosNorthFpu,
      common:  Common
    ) extends Create[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

      def toGmosLongSlit: GmosLongSlitConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
        new AbstractLongSlitConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu](this) {

          override def defaultXBin(
            sourceProfile: SourceProfile,
            imageQuality:  ImageQuality,
            sampling:      PosDouble
          ): GmosXBinning =
            GmosLongSlitConfig.xbinNorth(this.fpu, sourceProfile, imageQuality, sampling)

          override def defaultWavelengthDithers: List[WavelengthDither] =
            GmosLongSlitConfig.defaultWavelengthDithersNorth(this.grating)
        }  

    }

    object North {

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(North(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exSpatialOffsets.toOption
              )
            ))
          case _ =>
            Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
        }

    }

    final case class South(
      grating: GmosSouthGrating,
      filter:  Option[GmosSouthFilter],
      fpu:     GmosSouthFpu,
      common:  Common
    ) extends Create[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

      def toGmosLongSlit: GmosLongSlitConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
        new AbstractLongSlitConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu](this) {

          override def defaultXBin(
            sourceProfile: SourceProfile,
            imageQuality:  ImageQuality,
            sampling:      PosDouble
          ): GmosXBinning =
            GmosLongSlitConfig.xbinSouth(this.fpu, sourceProfile, imageQuality, sampling)

          override def defaultWavelengthDithers: List[WavelengthDither] =
            GmosLongSlitConfig.defaultWavelengthDithersSouth(this.grating)
        }  
    }

    object South {

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(South(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exSpatialOffsets.toOption
              )
            ))
          case _ =>
            Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS South Long Slit observing mode.")
        }

    }

  }


  object Edit {

    final case class Common(
      centralWavelength:      Option[Wavelength],
      explicitXBin:           Nullable[GmosXBinning],
      explicitYBin:           Nullable[GmosYBinning],
      explicitAmpReadMode:    Nullable[GmosAmpReadMode],
      explicitAmpGain:        Nullable[GmosAmpGain],
      explicitRoi:            Nullable[GmosRoi],
      explicitλDithers:       Nullable[List[Quantity[Int, Picometer]]],
      explicitSpatialOffsets: Nullable[List[Q]]
    ) {

      def toCreate(site: Site): Result[Create.Common] =
        required(site, centralWavelength, "centralWavelength").map { w =>
          Create.Common(
            w,
            explicitXBin.toOption,
            explicitYBin.toOption,
            explicitAmpReadMode.toOption,
            explicitAmpGain.toOption,
            explicitRoi.toOption,
            explicitλDithers.toOption,
            explicitSpatialOffsets.toOption
          )
        }

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Nullable[String] =
        explicitλDithers.map(WavelengthDithersFormat.reverseGet)

      val formattedSpatialOffsets: Nullable[String] =
        explicitSpatialOffsets.map(SpatialOffsetsFormat.reverseGet)

    }

    private def required[A](site: Site, oa: Option[A], itemName: String): Result[A] = {
      val siteName = site match {
        case Site.GN => "North"
        case Site.GS => "South"
      }
          
      Result.fromOption(oa, s"A $itemName is required in order to create a GMOS ${siteName} Long Slit observing mode.")
    }

    final case class North(
      grating: Option[GmosNorthGrating],
      filter:  Nullable[GmosNorthFilter],
      fpu:     Option[GmosNorthFpu],
      common:  Edit.Common
    ) {

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

      val toCreate: Result[Create.North] =
        for {
          g <- required(Site.GN, grating, "grating")
          u <- required(Site.GN, fpu, "fpu")
          c <- common.toCreate(Site.GN)
        } yield Create.North(g, filter.toOption, u, c)

    }

    object North {

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(North(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exSpatialOffsets
              )
            ))

        }
    }
    
    final case class South(
      grating: Option[GmosSouthGrating],
      filter:  Nullable[GmosSouthFilter],
      fpu:     Option[GmosSouthFpu],
      common:  Edit.Common
    ) {

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

      val toCreate: Result[Create.South] =
        for {
          g <- required(Site.GS, grating, "grating")
          u <- required(Site.GS, fpu, "fpu")
          c <- common.toCreate(Site.GS)
        } yield Create.South(g, filter.toOption, u, c)

    }

    object South {

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(South(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exSpatialOffsets
              )
            ))

        }
    }    
  }


  private val NorthData: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength],
    Nullable[GmosXBinning],
    Nullable[GmosYBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
    Nullable[List[Quantity[Int, Picometer]]],
    Nullable[List[Q]]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        GmosXBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosYBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.Binding.List.Nullable("explicitSpatialOffsets", rSpatialOffsets)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rCentralWavelength,
        rExplicitXBin,
        rExplicitYBin,
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
        rWavelengthDithers,
        rSpatialOffsets
      ).parTupled
    }

  private val SouthData: Matcher[(
    Option[GmosSouthGrating],
      Nullable[GmosSouthFilter],
      Option[GmosSouthFpu],
      Option[Wavelength],
      Nullable[GmosXBinning],
      Nullable[GmosYBinning],
      Nullable[GmosAmpReadMode],
      Nullable[GmosAmpGain],
      Nullable[GmosRoi],
      Nullable[List[Quantity[Int, Picometer]]],
      Nullable[List[Q]]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosSouthGratingBinding.Option("grating", rGrating),
          GmosSouthFilterBinding.Nullable("filter", rFilter),
          GmosSouthFpuBinding.Option("fpu", rFpu),
          WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
          GmosXBinningBinding.Nullable("explicitXBin", rExplicitXBin),
          GmosYBinningBinding.Nullable("explicitYBin", rExplicitYBin),
          GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
          WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
          OffsetComponentInput.Binding.List.Nullable("explicitSpatialOffsets", rSpatialOffsets)
        ) => (
          rGrating,
          rFilter,
          rFpu,
          rCentralWavelength,
          rExplicitXBin,
          rExplicitYBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rSpatialOffsets
        ).parTupled
      }
}

