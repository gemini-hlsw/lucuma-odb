#!/bin/bash
set -e

PG_HOST=localhost
PG_PORT=5432
PG_USER=jimmy
PG_DATABASE=lucuma-odb
PG_PASS=banana
PG_BACKUP=backup

# Clean up on exit
function clean_up {
  if [ $? -eq 0 ]; then
    echo "🍏 Success!"
  else
    echo "🍎 Fail."
  fi
  if [ -f $PG_BACKUP ]; then rm $PG_BACKUP; fi
}
trap clean_up EXIT

# Restart the database
echo "🍏 Re-initializing database and applying migrations."
docker-compose down > /dev/null 2>&1
docker-compose up -d > /dev/null 2>&1


# Wait for PG to restart
echo "🍏 Waiting for postgres to start."
RETRIES=100
until docker-compose run -v `pwd`:/local -e PGPASSWORD=$PG_PASS db psql -w -h db -U $PG_USER -d $PG_DATABASE -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  sleep 1
done

# Dump our database
echo "🍏 Performing pg_dump."
docker-compose run -v `pwd`:/local db bash -c "PGPASSWORD=$PG_PASS pg_dump -w -h db -U $PG_USER -Fc $PG_DATABASE > /local/$PG_BACKUP" 

# Attempt a restore
echo "🍏 Performing pg_restore."
docker-compose run -v `pwd`:/local -e PGPASSWORD=$PG_PASS db pg_restore -w -e -n public --clean --if-exists --no-acl --no-owner -h db -U $PG_USER -d $PG_DATABASE /local/$PG_BACKUP

