// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

// This just does a big introspection query to ensure that the schema doesn't have any undefined types or
// other loose ends.
class introspection extends OdbSuite:
  val pi         = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("introspection query"):
    query(
      user = pi,
      // N.B. this is the query https://github.com/graphql/graphiql uses
      query = """
        query IntrospectionQuery {
          __schema {
            queryType {
              name
            }
            mutationType {
              name
            }
            subscriptionType {
              name
            }
            types {
              ...FullType
            }
            directives {
              name
              description
              locations
              args {
                ...InputValue
              }
            }
          }
        }

        fragment FullType on __Type {
          kind
          name
          description
          fields(includeDeprecated: true) {
            name
            description
            args {
              ...InputValue
            }
            type {
              ...TypeRef
            }
            isDeprecated
            deprecationReason
          }
          inputFields {
            ...InputValue
          }
          interfaces {
            ...TypeRef
          }
          enumValues(includeDeprecated: true) {
            name
            description
            isDeprecated
            deprecationReason
          }
          possibleTypes {
            ...TypeRef
          }
        }

        fragment InputValue on __InputValue {
          name
          description
          type {
            ...TypeRef
          }
          defaultValue
        }

        fragment TypeRef on __Type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                      ofType {
                        kind
                        name
                      }
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).onError { case t =>
      fail("\n🐞🐞🐞\n🐞🐞🐞 Schema introspection failed!\n🐞🐞🐞\n", t)
    }

  test("introspection query works without authentication"):
    unauthenticatedQuery(
      query = """
        query IntrospectionQuery {
          __schema {
            queryType {
              name
            }
          }
        }
      """
    ).onError { case t =>
      fail("Unauthenticated schema introspection failed!", t)
    }
