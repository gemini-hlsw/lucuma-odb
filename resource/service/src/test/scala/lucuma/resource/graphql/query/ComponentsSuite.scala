// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource.graphql.query

import io.circe.literal.*

class ComponentsSuite extends ComponentFixture:

  test(
    "components: returns present components with blocks at the site, excludes deleted and blockless, ordered instrument/type/code"
  ):
    insertCatalog >> expectSuccess(
      query = """
        query {
          components(site: GN) {
            id instrument componentType code name barcode aliases existence
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-1", "instrument": "GMOS", "componentType": "FILTER", "code": "G_PRIME", "name": "g'", "barcode": null, "aliases": ["g_G0301"], "existence": "PRESENT" },
          { "id": "c-2", "instrument": "GMOS", "componentType": "DISPERSER", "code": "R831_G5302", "name": "R831", "barcode": null, "aliases": [], "existence": "PRESENT" }
        ]
      }"""
    )

  test("components: includeDeleted adds the deleted component"):
    expectSuccess(
      query = """
        query {
          components(site: GN, includeDeleted: true) {
            id existence
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-1", "existence": "PRESENT" },
          { "id": "c-2", "existence": "PRESENT" },
          { "id": "c-3", "existence": "DELETED" }
        ]
      }"""
    )

  test(
    "components: componentTypes narrows to matching types; instruments combined with includeDeleted narrows to the deleted GNIRS piece"
  ):
    expectSuccess(
      query = """
        query {
          components(site: GN, componentTypes: [DISPERSER]) {
            id
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-2" }
        ]
      }"""
    ) >> expectSuccess(
      query = """
        query {
          components(site: GN, instruments: [GNIRS], includeDeleted: true) {
            id
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-3" }
        ]
      }"""
    )

  test("components: search matches an alias, and a code, case-insensitively"):
    expectSuccess(
      query = """
        query {
          components(site: GN, search: "g0301") {
            id
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-1" }
        ]
      }"""
    ) >> expectSuccess(
      query = """
        query {
          components(site: GN, search: "r831") {
            id
          }
        }
      """,
      expected = json"""{
        "components": [
          { "id": "c-2" }
        ]
      }"""
    )

  test("components: no records at GS returns an empty list"):
    expectSuccess(
      query = """
        query {
          components(site: GS) {
            id
          }
        }
      """,
      expected = json"""{
        "components": []
      }"""
    )
