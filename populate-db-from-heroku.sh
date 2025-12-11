#!/bin/bash
set -e
shopt -s extglob

function usage {
  echo -e "Usage: $0 [dev|staging|production]"
  exit 1
}

if [[ $1 != @(dev|staging|production) ]]; then
  usage
fi

HEROKU_APP=lucuma-postgres-odb-$1
SCRIPT_DIR=modules/service/src/main/resources/db/migration/
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

echo "🍏 Populating local database from $HEROKU_APP."

echo "🍏 Starting the database."
docker-compose up -d > /dev/null 2>&1

echo "🍏 Waiting for postgres."
RETRIES=100
until psql -w -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  sleep 1
done

echo "🍏 Dropping and re-creating the database with an empty schema."
psql -h $PG_HOST -U $PG_USER -d postgres -c "drop database \"$PG_DATABASE\"" > /dev/null
psql -h $PG_HOST -U $PG_USER -d postgres -c "create database \"$PG_DATABASE\"" > /dev/null

echo "🍏 Capturing a Heroku backup."
heroku pg:backups:capture --app $HEROKU_APP

echo "🍏 Downloading Heroku backup."
heroku pg:backups:download --app $HEROKU_APP --output /tmp/$HEROKU_APP.dump

echo "🍏 Translating binary dump to SQL."
pg_restore --verbose --clean --if-exists --no-acl --no-owner -f /tmp/$HEROKU_APP.temp /tmp/$HEROKU_APP.dump > /dev/null 2>&1

echo "🍏 Removing pg_catalog.set_config (temporary, hopefully)."
grep -v pg_catalog.set_config /tmp/$HEROKU_APP.temp > /tmp/$HEROKU_APP.sql

echo "🍏 Restoring dump to local database."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE < /tmp/$HEROKU_APP.sql > /dev/null 2>&1

echo "🍏 Fixing program references (temporary, hopefully)."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c 'update t_program set c_program_id = c_program_id' > /dev/null 2>&1
