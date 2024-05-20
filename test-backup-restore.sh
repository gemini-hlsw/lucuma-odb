#!/bin/bash
set -e

PG_HOST=localhost
PG_PORT=5432
PG_USER=jimmy
PG_DATABASE=lucuma-odb
PG_PASS=banana

PG_BACKUP=./backup
export PGPASSFILE=./pgpass

# Clean up on error
function clean_up {
  if [ $? -eq 0 ]; then
    echo "🍏 Success!"
  else
    echo "🍎 Fail."
  fi
  if [ -f $PGPASSFILE ]; then rm $PGPASSFILE; fi
  if [ -f $PG_BACKUP ]; then rm $PG_BACKUP; fi
}
trap clean_up EXIT

# Set up password file
echo $PG_HOST:$PG_PORT:$PG_DATABASE:$PG_USER:$PG_PASS > $PGPASSFILE
chmod 0600 $PGPASSFILE

# Restart the database
echo "🍏 Re-initializing database and applying migrations."
docker-compose down > /dev/null 2>&1
docker-compose up -d > /dev/null 2>&1

# Wait for PG to restart
echo "🍏 Waiting for postgres to start."
RETRIES=100
until psql -w -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  sleep 1
done

# Dump our database
echo "🍏 Performing pg_dump."
pg_dump -w -h $PG_HOST -U $PG_USER -Fc $PG_DATABASE > $PG_BACKUP

# Attempt a restore
echo "🍏 Performing pg_restore."
pg_restore -w -e -n public --clean --if-exists --no-acl --no-owner -h $PG_HOST -U $PG_USER -d $PG_DATABASE $PG_BACKUP

