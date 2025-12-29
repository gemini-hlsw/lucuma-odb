// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

object all {

  trait UniversalCodecs
    extends CalculatedValueCodec
       with CatalogInfoCodec
       with EpochCodec
       with Flamingos2Codec
       with GmosCodec
       with ItcCodec
       with NumericCodec
       with PartnerLinkCodec
       with SequenceCodec
       with SourceProfileCodec
       with StepConfigCodec
       with TimeAccountingCodec

  object query
    extends angle.QueryCodec
       with offset.QueryCodec
       with time.QueryCodec
       with wavelength.QueryCodec
       with rightascension.QueryCodec
       with declination.QueryCodec
       with propermotion.QueryCodec
       with radialvelocity.QueryCodec
       with parallax.QueryCodec
       with target.QueryCodec
       with configurationrequest.QueryCodec
       with tellurictype.QueryCodec
       with UniversalCodecs

  object transport
    extends angle.TransportCodec
       with offset.TransportCodec
       with time.TransportCodec
       with wavelength.TransportCodec
       with rightascension.TransportCodec
       with declination.TransportCodec
       with propermotion.TransportCodec
       with radialvelocity.TransportCodec
       with parallax.TransportCodec
       with target.TransportCodec
       with tellurictype.TransportCodec
       with UniversalCodecs

}
