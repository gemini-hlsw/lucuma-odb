// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.data.ZipperCodec
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ItcResult
import lucuma.odb.data.ItcScience

trait ItcCodec:

  import time.decoder.given
  import wavelength.decoder.given
  import ZipperCodec.given

  given Decoder[SignalToNoiseAt] = c =>
    for
      w <- c.downField("wavelength").as[Wavelength]
      s <- c.downField("single").as[SignalToNoise]
      t <- c.downField("total").as[SignalToNoise]
    yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

  // N.B. lucuma.itc.SignalToNoiseAt defines its own encoder consistent with
  // this decoder.  Perhaps we should move the decoder there as well.

  given Decoder[ItcResult] =
    Decoder.instance: c =>
      for
        targetId        <- c.downField("targetId").as[Target.Id]
        exposureTime    <- c.downField("exposureTime").as[TimeSpan]
        exposureCount   <- c.downField("exposureCount").as[PosInt]
        signalToNoiseAt <- c.downField("signalToNoiseAt").as[Option[SignalToNoiseAt]]
      yield ItcResult(targetId, IntegrationTime(exposureTime, exposureCount), signalToNoiseAt)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcResult] =
    Encoder.instance: a =>
      Json.obj(
        "targetId"        -> a.targetId.asJson,
        "exposureTime"    -> a.value.exposureTime.asJson,
        "exposureCount"   -> a.value.exposureCount.value.asJson,
        "signalToNoiseAt" -> a.signalToNoise.asJson
      )

  // GnirsAcquisitionType is a plain Enumerated; encode by tag.  Round-trips
  // within this codec only (the value is stored in the acquisition jsonb blob).
  private given Encoder[GnirsAcquisitionType] =
    Encoder[String].contramap(Enumerated[GnirsAcquisitionType].tag)

  private given Decoder[GnirsAcquisitionType] =
    Decoder[String].emap: s =>
      Enumerated[GnirsAcquisitionType].fromTag(s).toRight(s"Invalid GnirsAcquisitionType: $s")

  // Only the Available case is stored as JSON (in c_acquisition_results); the
  // NotApplicable and Failed cases are represented by the c_acquisition_* columns
  // directly (both null / c_acquisition_error), so the codec is for the payload.
  given Decoder[ItcAcquisition.Available] =
    Decoder.instance: c =>
      for
        times   <- c.downField("times").as[Zipper[ItcResult]]
        acqType <- c.downField("gnirsAcqType").as[Option[GnirsAcquisitionType]]
      yield ItcAcquisition.Available(times, acqType)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcAcquisition.Available] =
    Encoder.instance: a =>
      Json.obj(
        "times" -> a.times.asJson
      // Emitted only when set (only GNIRS pins an acquisition type).
      ).deepMerge(a.gnirsAcqType.fold(Json.obj())(t => Json.obj("gnirsAcqType" -> t.asJson)))

  private def imagingScienceNemDecoder[A: Decoder: Order](
    fieldName: String
  ): Decoder[NonEmptyMap[A, Zipper[ItcResult]]] =
    Decoder.instance: c =>
      c.downField(fieldName)
       .values
       .flatMap(it => NonEmptyList.fromList(it.toList))
       .toRight(DecodingFailure("Expecting at least one ITC result set.", c.history))
       .flatMap: nel =>
         val res = nel.traverse: json =>
           val c = json.hcursor
           for
             filter  <- c.downField("filter").as[A]
             results <- c.downField("results").as[Zipper[ItcResult]]
           yield filter -> results
         res.map(_.toNem)

  given Decoder[ItcScience.GhostIfu] =
    Decoder.instance: c =>
      for
        red  <- c.downField("red").as[Zipper[ItcResult]]
        blue <- c.downField("blue").as[Zipper[ItcResult]]
      yield ItcScience.GhostIfu(red, blue)

  given Decoder[ItcScience.Flamingos2Imaging] =
    imagingScienceNemDecoder[Flamingos2Filter]("flamingos2ImagingScience").map(ItcScience.Flamingos2Imaging.apply)

  given Decoder[ItcScience.GmosNorthImaging] =
    imagingScienceNemDecoder[GmosNorthFilter]("gmosNorthImagingScience").map(ItcScience.GmosNorthImaging.apply)

  given Decoder[ItcScience.GmosSouthImaging] =
    imagingScienceNemDecoder[GmosSouthFilter]("gmosSouthImagingScience").map(ItcScience.GmosSouthImaging.apply)

  given Decoder[ItcScience.GnirsImaging] =
    imagingScienceNemDecoder[GnirsFilter]("gnirsImagingScience").map(ItcScience.GnirsImaging.apply)

  given Decoder[ItcScience.Spectroscopy] =
    Decoder.instance:
      _.downField("spectroscopyScience").as[Zipper[ItcResult]].map(ItcScience.Spectroscopy.apply)

  private def imagingScienceNemEncoder[A: Encoder](
    using Encoder[TimeSpan], Encoder[Wavelength]
  ): Encoder[NonEmptyMap[A, Zipper[ItcResult]]] =
    Encoder.instance: a =>
      Json.fromValues:
        a.toNel.toList.map: (filter, results) =>
          Json.obj(
            "filter"  -> filter.asJson,
            "results" -> results.asJson
          )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.GhostIfu] =
    Encoder.instance: a =>
      Json.obj(
        "itcType" -> ItcScience.Type.GhostIfu.asJson,
        "red"     -> a.red.asJson,
        "blue"    -> a.blue.asJson
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.Flamingos2Imaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"                  -> ItcScience.Type.Flamingos2Imaging.asJson,
        "flamingos2ImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[Flamingos2Filter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.GmosNorthImaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"                 -> ItcScience.Type.GmosNorthImaging.asJson,
        "gmosNorthImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[GmosNorthFilter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.GmosSouthImaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"                 -> ItcScience.Type.GmosSouthImaging.asJson,
        "gmosSouthImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[GmosSouthFilter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.GnirsImaging] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"             -> ItcScience.Type.GnirsImaging.asJson,
        "gnirsImagingScience" -> a.science.asJson(using imagingScienceNemEncoder[GnirsFilter])
      )

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience.Spectroscopy] =
    Encoder.instance: a =>
      Json.obj(
        "itcType"             -> ItcScience.Type.Spectroscopy.asJson,
        "spectroscopyScience" -> a.science.asJson
      )

  given Decoder[ItcScience] =
    Decoder.instance: c =>
      c.downField("itcType")
       .as[ItcScience.Type]
       .flatMap:
         case ItcScience.Type.Flamingos2Imaging   => Decoder[ItcScience.Flamingos2Imaging].apply(c)
         case ItcScience.Type.GhostIfu            => Decoder[ItcScience.GhostIfu].apply(c)
         case ItcScience.Type.GmosNorthImaging    => Decoder[ItcScience.GmosNorthImaging].apply(c)
         case ItcScience.Type.GmosSouthImaging    => Decoder[ItcScience.GmosSouthImaging].apply(c)
         case ItcScience.Type.GnirsImaging        => Decoder[ItcScience.GnirsImaging].apply(c)
         case ItcScience.Type.Spectroscopy        => Decoder[ItcScience.Spectroscopy].apply(c)

  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[ItcScience] =
    Encoder.instance:
      case a @ ItcScience.Flamingos2Imaging(_)   => Encoder[ItcScience.Flamingos2Imaging].apply(a)
      case a @ ItcScience.GhostIfu(_, _)         => Encoder[ItcScience.GhostIfu].apply(a)
      case a @ ItcScience.GmosNorthImaging(_)    => Encoder[ItcScience.GmosNorthImaging].apply(a)
      case a @ ItcScience.GmosSouthImaging(_)    => Encoder[ItcScience.GmosSouthImaging].apply(a)
      case a @ ItcScience.GnirsImaging(_)        => Encoder[ItcScience.GnirsImaging].apply(a)
      case a @ ItcScience.Spectroscopy(_)        => Encoder[ItcScience.Spectroscopy].apply(a)

  // The GraphQL `Itc` union predates the acquisition/science split, so its output
  // JSON reassembles the two parts into the original fused shape.  There is no
  // corresponding Decoder: the composite is assembled from separate storage
  // columns (see ItcService), never decoded from a single blob.  Imaging and
  // GHOST encode straight from their science result (they have no acquisition).
  // A spectroscopy science with an acquisition result is an `ItcSpectroscopy`
  // (acquisition merged in); one without is an `ItcIgrins2Spectroscopy` (distinct
  // `itcType`).  A `Failed` acquisition is not represented here — the caller
  // surfaces it as an error, as before the split an acquisition ITC failure
  // failed the whole lookup.  `gnirsAcqType` is an internal detail with no
  // GraphQL field, so it is omitted.
  given (using Encoder[TimeSpan], Encoder[Wavelength]): Encoder[Itc] =
    Encoder.instance: itc =>
      val science = itc.science.asJson
      itc.science match
        case _: ItcScience.Spectroscopy =>
          itc.acquisition match
            case ItcAcquisition.Available(times, _) =>
              science.deepMerge(Json.obj("acquisition" -> times.asJson))
            case _                                  =>
              science.deepMerge(Json.obj("itcType" -> "igrins_2_spectroscopy".asJson))
        case _                          =>
          science

object itc extends ItcCodec
