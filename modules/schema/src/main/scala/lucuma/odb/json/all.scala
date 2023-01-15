// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

object all {

  trait UniversalCodecs
    extends GmosCodec
       with NumericCodec
       with SourceProfileCodec
       with StepConfigCodec

  object query
    extends angle.QueryCodec
       with offset.QueryCodec
       with time.QueryCodec
       with wavelength.QueryCodec
       with UniversalCodecs

  object transport
    extends angle.TransportCodec
       with offset.TransportCodec
       with time.TransportCodec
       with wavelength.TransportCodec
       with UniversalCodecs

}
