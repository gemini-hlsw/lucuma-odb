// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import munit.FunSuite

class GraphQLRoutesSuite extends FunSuite:

  test("capDocument keeps a document under the cap untouched"):
    val doc = "query { observations { id } }"
    assertEquals(GraphQLRoutes.capDocument(doc), doc)

  test("capDocument keeps a document exactly at the cap untouched"):
    val doc = "x" * GraphQLRoutes.MaxFullDocumentLength
    assertEquals(GraphQLRoutes.capDocument(doc), doc)

  test("capDocument truncates and labels a document over the cap"):
    val max = GraphQLRoutes.MaxFullDocumentLength
    val doc = "x" * (max + 10)
    val out = GraphQLRoutes.capDocument(doc)
    assert(out.startsWith("x" * max))
    assert(out.endsWith(s"... (truncated at $max of ${max + 10} chars)"))
