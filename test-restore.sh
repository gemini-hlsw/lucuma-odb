#!/bin/bash
set -e

PG_HOST=localhost
PG_USER=jimmy
PG_DATABASE=lucuma-odb
export PGPASSWORD=banana

# Clean up on exit
function clean_up {
  if [ $? -eq 0 ]; then
    echo "🍏 Success!"
  else
    echo "🍎 Fail."
  fi
}
trap clean_up EXIT

echo "🍏 Recreating the database."
docker-compose down > /dev/null 2>&1
docker-compose up -d > /dev/null 2>&1

echo "🍏 Waiting for postgres."
RETRIES=100
until psql -w -h $PG_HOST -U $PG_USER -d postgres -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  sleep 1
done

echo "🍏 Dumping $PG_DATABASE."

pg_dump -h $PG_HOST -U $PG_USER -d $PG_DATABASE > /tmp/test-backup.sql

echo "🍏 Dropping and re-creating $PG_DATABASE."
psql -h $PG_HOST -U $PG_USER -d postgres -c "drop database \"$PG_DATABASE\"" > /dev/null
psql -h $PG_HOST -U $PG_USER -d postgres -c "create database \"$PG_DATABASE\"" > /dev/null

echo "🍏 Restoring $PG_DATABASE."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE < /tmp/test-backup.sql 


