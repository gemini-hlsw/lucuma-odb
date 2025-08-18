// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import io.circe.parser.parse
import lucuma.sso.service.SsoSuite

object PersonCodecSuite extends SsoSuite {

  pureTest("deserialize a record with no credit name") {

    val json = parse {
      s"""{
            "last-modified-date" : null,
            "name" : {
              "created-date" : {
                "value" : 1593610871037
              },
              "last-modified-date" : {
                "value" : 1593610871037
              },
              "given-names" : {
                "value" : "Bob"
              },
              "family-name" : {
                "value" : "Dole"
              },
              "credit-name" : null,
              "source" : null,
              "visibility" : "public",
              "path" : "0000-0000-0000-0000"
            },
            "other-names" : {
              "last-modified-date" : null,
              "other-name" : [
              ],
              "path" : "/0000-0000-0000-0000/other-names"
            },
            "biography" : null,
            "researcher-urls" : {
              "last-modified-date" : null,
              "researcher-url" : [
              ],
              "path" : "/0000-0000-0000-0000/researcher-urls"
            },
            "emails" : {
              "last-modified-date" : null,
              "email" : [
              ],
              "path" : "/0000-0000-0000-0000/email"
            },
            "addresses" : {
              "last-modified-date" : null,
              "address" : [
              ],
              "path" : "/0000-0000-0000-0000/address"
            },
            "keywords" : {
              "last-modified-date" : null,
              "keyword" : [
              ],
              "path" : "/0000-0000-0000-0000/keywords"
            },
            "external-identifiers" : {
              "last-modified-date" : null,
              "external-identifier" : [
              ],
              "path" : "/0000-0000-0000-0000/external-identifiers"
            },
            "path" : "/0000-0000-0000-0000/person"
          }
          """
      } .toOption.get // yolo

    expect(OrcidPerson.DecoderOrcidPerson.decodeJson(json).isRight)

  }

  pureTest("deserialize a person with no public information at all") {

    val json = parse {
      s"""{
            "last-modified-date" : null,
            "name" : null,
            "other-names" : {
              "last-modified-date" : null,
              "other-name" : [
              ],
              "path" : "/0000-0000-0000-0000/other-names"
            },
            "biography" : null,
            "researcher-urls" : {
              "last-modified-date" : null,
              "researcher-url" : [
              ],
              "path" : "/0000-0000-0000-0000/researcher-urls"
            },
            "emails" : {
              "last-modified-date" : null,
              "email" : [
              ],
              "path" : "/0000-0000-0000-0000/email"
            },
            "addresses" : {
              "last-modified-date" : null,
              "address" : [
              ],
              "path" : "/0000-0000-0000-0000/address"
            },
            "keywords" : {
              "last-modified-date" : null,
              "keyword" : [
              ],
              "path" : "/0000-0000-0000-0000/keywords"
            },
            "external-identifiers" : {
              "last-modified-date" : null,
              "external-identifier" : [
              ],
              "path" : "/0000-0000-0000-0000/external-identifiers"
            },
            "path" : "/0000-0000-0000-0000/person"
          }
        """

    }.toOption.get // yolo

    expect(OrcidPerson.DecoderOrcidPerson.decodeJson(json).isRight)

  }

}
