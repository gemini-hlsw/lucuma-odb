#!/usr/bin/env node
import { writeFile } from 'fs/promises';
import { buildClientSchema, getIntrospectionQuery, printSchema } from 'graphql';

//
// fetchLocalODBSchema.mjs
//
// Fetches the ODB schema from a locally running ODB instance and writes it
// to the schema resources directory for use in ITC schema stitching.
//
// Prerequisites: ODB server must be running on localhost:8082
//

const url = 'http://localhost:8082/odb';

console.log('Fetching schema from local ODB server...');

const response = await fetch(new URL(url), {
  headers: {
    'Content-Type': 'application/json',
  },
  method: 'POST',
  body: JSON.stringify({
    query: getIntrospectionQuery({
      specifiedByUrl: true,
      oneOf: true,
      inputValueDeprecation: true,
      directiveIsRepeatable: true,
    }),
  }),
});

if (!response.ok) {
  throw new Error(
    `Failed to fetch introspection query: ${response.statusText}`
  );
}

console.log('Fetched ODB schema from local server.');

const json = await response.json();

if (json.errors) {
  throw new Error('Introspection query returned errors', { cause: json.errors });
}

const schema = printSchema(buildClientSchema(json.data));

await writeFile(
  'service/src/main/resources/graphql/ObservationDB.graphql',
  schema
);

console.log('Wrote ODB schema to service/src/main/resources/graphql/ObservationDB.graphql');
