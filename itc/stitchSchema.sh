#!/usr/bin/env bash
set -e

ITC_BASE=itc/service/src/main/resources/graphql/itc_base.graphql
ITC_OUT=itc/service/src/main/resources/graphql/itc.graphql
TEMP_SCHEMA=/tmp/OdbSchema.graphql

function usage {
  echo "Usage: $0 <local|dev|staging|production>"
  echo "  local      — fetch from http://localhost:8082/odb"
  echo "  dev        — fetch from https://lucuma-postgres-odb-dev.herokuapp.com/odb"
  echo "  staging    — fetch from https://lucuma-postgres-odb-staging.herokuapp.com/odb"
  echo "  production — fetch from https://lucuma-postgres-odb-production.herokuapp.com/odb"
  exit 1
}

if [ -z "${1:-}" ]; then
  usage
fi

ENV=$1

case "$ENV" in
  local)
    ODB_URL=http://localhost:8082/odb
    if ! curl -sf "$ODB_URL" > /dev/null 2>&1; then
      echo "ODB server is not running on localhost:8082"
      exit 1
    fi
    ODB_SCHEMA=$TEMP_SCHEMA
    ;;
  dev|staging|production)
    ODB_URL=https://lucuma-postgres-odb-${ENV}.herokuapp.com/odb
    ODB_SCHEMA=$TEMP_SCHEMA
    ;;
  *)
    usage
    ;;
esac

npm ci --silent

if [ -n "$ODB_URL" ]; then
  trap 'rm -f "$TEMP_SCHEMA"' EXIT
  echo "Fetching ODB schema from $ODB_URL..."
  node itc/fetchODBSchema.mjs "$ODB_URL" "$TEMP_SCHEMA"
fi

echo "Stitching schema..."
node itc/schemastitcher.mjs "$ODB_SCHEMA" "$ITC_BASE" "$ITC_OUT"

echo "Done: $ITC_OUT"
