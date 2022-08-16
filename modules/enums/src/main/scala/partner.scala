// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.enums

import lucuma.core.model.Partner

object PartnerQuery extends EnumeratedQuery[Partner](_.tag) {

    val document =
      """
        query {
          partnerMeta {
            tag
            shortName
            longName
            active
          }
        }
      """

    val ElemDecoder = hc =>
      for {
        tag <- hc.downField("tag").as[String]
        nam <- hc.downField("name").as[String]
        shn <- hc.downField("shortName").as[String]
        act <- hc.downField("active").as[Boolean]
      } yield ??? // TODO: Partner(tag, tag, nam, shn, act)

  }

