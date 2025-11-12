#!/usr/bin/env bash

# Check if ODB is running
echo "Checking if ODB server is running..."
if ! curl -s http://localhost:8082/odb > /dev/null 2>&1; then
  echo "ODB server is not running on localhost:8082"
  exit 1
fi

echo "Fetching ODB schema from local server..."
npm ci --silent
./fetchLocalODBSchema.mjs

echo "Stitching"
node schemastitcher.mjs \
  service/src/main/resources/graphql/ObservationDB.graphql \
  service/src/main/resources/graphql/itc_base.graphql \
  service/src/main/resources/graphql/itc.graphql

rm service/src/main/resources/graphql/ObservationDB.graphql

echo "Schema stitching complete"
