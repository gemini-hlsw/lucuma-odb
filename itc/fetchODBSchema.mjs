#!/usr/bin/env node
import { writeFile } from 'fs/promises';
import { buildClientSchema, getIntrospectionQuery, printSchema } from 'graphql';

const [url, outputPath] = process.argv.slice(2);

if (!url || !outputPath) {
  console.error('Usage: fetchODBSchema.mjs <url> <outputPath>');
  process.exit(1);
}

console.log(`Fetching ODB schema from ${url}...`);

const response = await fetch(new URL(url), {
  headers: { 'Content-Type': 'application/json' },
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
  throw new Error(`Failed to fetch introspection query: ${response.statusText}`);
}

const json = await response.json();

if (json.errors) {
  throw new Error('Introspection query returned errors', { cause: json.errors });
}

const schema = printSchema(buildClientSchema(json.data));
await writeFile(outputPath, schema);

console.log(`Wrote ODB schema to ${outputPath}`);
