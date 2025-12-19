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

ENVIRONMENT=$1
HEROKU_APP=lucuma-postgres-odb-$ENVIRONMENT
PG_HOST=${PG_HOST:-localhost}
PG_USER=${PG_USER:-jimmy}
PG_DATABASE=${PG_DATABASE:-lucuma-odb}
export PGPASSWORD=${PGPASSWORD:-banana}

SERVICE_LOG=/tmp/service-$ENVIRONMENT.log
RESTORE_LOG=/tmp/restore-$ENVIRONMENT.log

# Clean up on exit
function clean_up {
  echo "Cleaning up..."
  if [ -n "$SERVICE_PID" ]; then
    kill $SERVICE_PID 2>/dev/null || true
    wait $SERVICE_PID 2>/dev/null || true
  fi
  rm -f /tmp/$HEROKU_APP.dump /tmp/$HEROKU_APP.temp /tmp/$HEROKU_APP.sql

  if [ $? -eq 0 ]; then
    echo "Success testing migrations against $ENVIRONMENT"
  else
    echo "Failed testing migrations against $ENVIRONMENT"
  fi
}
trap clean_up EXIT

echo "Testing migrations against $HEROKU_APP"

echo "Waiting for PostgreSQL..."
RETRIES=100
until psql -w -h $PG_HOST -U $PG_USER -d postgres -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  echo "Waiting for postgres... $RETRIES attempts remaining"
  sleep 1
  RETRIES=$((RETRIES - 1))
done

if [ $RETRIES -eq 0 ]; then
  echo "PostgreSQL failed to start"
  exit 1
fi
echo "PostgreSQL is ready"

echo "Capturing Heroku backup from $HEROKU_APP..."
heroku pg:backups:capture --app $HEROKU_APP

echo "Downloading Heroku backup..."
heroku pg:backups:download --app $HEROKU_APP --output /tmp/$HEROKU_APP.dump
ls -lh /tmp/$HEROKU_APP.dump

echo "Dropping and recreating database..."
psql -h $PG_HOST -U $PG_USER -d postgres -c "DROP DATABASE IF EXISTS \"$PG_DATABASE\"" > /dev/null
psql -h $PG_HOST -U $PG_USER -d postgres -c "CREATE DATABASE \"$PG_DATABASE\"" > /dev/null

echo "Converting binary dump to SQL..."
pg_restore --verbose --clean --if-exists --no-acl --no-owner \
  -f /tmp/$HEROKU_APP.temp /tmp/$HEROKU_APP.dump > /dev/null 2>&1 || true

echo "Removing pg_catalog.set_config calls..."
grep -v pg_catalog.set_config /tmp/$HEROKU_APP.temp > /tmp/$HEROKU_APP.sql || true

echo "Restoring to local database..."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE < /tmp/$HEROKU_APP.sql > $RESTORE_LOG 2>&1

echo "Fixing program references..."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c 'UPDATE t_program SET c_program_id = c_program_id' > /dev/null 2>&1

echo "Database restored successfully"

echo "Starting service to run migrations..."
export DATABASE_URL="postgres://$PG_USER:$PGPASSWORD@$PG_HOST:5432/$PG_DATABASE"

# Start the service in background
sbt -v -J-Xmx6g "service/run serve" > $SERVICE_LOG 2>&1 &
SERVICE_PID=$!

echo "Service PID: $SERVICE_PID"

# Monitor service startup for success indicators
SUCCESS=false
TIMEOUT=300  # 5 minutes
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
  # Check if process is still running
  if ! kill -0 $SERVICE_PID 2>/dev/null; then
    echo "Service process died unexpectedly"
    echo "Last 100 lines of service log:"
    tail -100 $SERVICE_LOG
    exit 1
  fi

  # Check logs for success indicators
  if grep -q "Startup diagnostics passed" $SERVICE_LOG 2>/dev/null; then
    echo "Migrations completed and startup diagnostics passed"
    SUCCESS=true
    break
  fi

  # Check for failure indicators
  if grep -q "Startup diagnostics failed" $SERVICE_LOG 2>/dev/null; then
    echo "Startup diagnostics failed"
    echo "Service log:"
    cat $SERVICE_LOG
    exit 1
  fi

  if grep -Eqi "error.*migration|migration.*failed" $SERVICE_LOG 2>/dev/null; then
    echo "Migration error detected"
    echo "Service log:"
    cat $SERVICE_LOG
    exit 1
  fi

  sleep 2
  ELAPSED=$((ELAPSED + 2))
done

# Kill the service
if [ -n "$SERVICE_PID" ]; then
  kill $SERVICE_PID 2>/dev/null || true
  wait $SERVICE_PID 2>/dev/null || true
  SERVICE_PID=""  # Prevent cleanup from trying to kill again
fi

if [ "$SUCCESS" = "true" ]; then
  echo "Migration test passed for $ENVIRONMENT"
  exit 0
else
  echo "Timeout waiting for service startup"
  echo "Last 100 lines of service log:"
  tail -100 $SERVICE_LOG
  exit 1
fi
