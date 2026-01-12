#!/bin/bash
set -e
shopt -s extglob

function usage {
  echo -e "Usage: $0 [dev|staging|production] [--test]"
  echo -e "  --test  Start service to verify migrations work (for CI)"
  exit 1
}

TEST_MIGRATION=false
ENVIRONMENT=""

for arg in "$@"; do
  case $arg in
    dev|staging|production)
      ENVIRONMENT=$arg
      ;;
    --test)
      TEST_MIGRATION=true
      ;;
    *)
      usage
      ;;
  esac
done

if [ -z "$ENVIRONMENT" ]; then
  usage
fi

HEROKU_APP=lucuma-postgres-odb-$ENVIRONMENT
PG_HOST=localhost
PG_USER=jimmy
PG_DATABASE=lucuma-odb
export PGPASSWORD=banana

SERVICE_LOG=/tmp/service-$ENVIRONMENT.log

# Clean up on exit
function clean_up {
  if [ -n "$SERVICE_PID" ]; then
    kill $SERVICE_PID 2>/dev/null || true
    wait $SERVICE_PID 2>/dev/null || true
  fi
  if [ -n "$TAIL_PID" ]; then
    kill $TAIL_PID 2>/dev/null || true
  fi
  rm -f /tmp/$HEROKU_APP.dump /tmp/$HEROKU_APP.temp /tmp/$HEROKU_APP.sql
}
trap clean_up EXIT

echo "🍏 Populating local database from $HEROKU_APP."

echo "🍏 Starting the database."
docker compose up -d > /dev/null 2>&1

echo "🍏 Waiting for postgres."
RETRIES=100
until psql -w -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c "select 1" > /dev/null 2>&1 || [ $RETRIES -eq 0 ]; do
  sleep 1
  RETRIES=$((RETRIES - 1))
done

if [ $RETRIES -eq 0 ]; then
  echo "PostgreSQL failed to start"
  exit 1
fi

echo "🍏 Dropping and re-creating the database with an empty schema."
psql -h $PG_HOST -U $PG_USER -d postgres -c "drop database \"$PG_DATABASE\"" > /dev/null
psql -h $PG_HOST -U $PG_USER -d postgres -c "create database \"$PG_DATABASE\"" > /dev/null

echo "🍏 Capturing a Heroku backup."
heroku pg:backups:capture --app $HEROKU_APP

echo "🍏 Downloading Heroku backup."
heroku pg:backups:download --app $HEROKU_APP --output /tmp/$HEROKU_APP.dump

echo "🍏 Translating binary dump to SQL."
pg_restore --verbose --clean --if-exists --no-acl --no-owner -f /tmp/$HEROKU_APP.temp /tmp/$HEROKU_APP.dump > /dev/null 2>&1

echo "🍏 Removing pg_catalog.set_config."
grep -v pg_catalog.set_config /tmp/$HEROKU_APP.temp > /tmp/$HEROKU_APP.sql

echo "🍏 Restoring dump to local database."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE < /tmp/$HEROKU_APP.sql > /dev/null 2>&1

echo "🍏 Fixing program references (temporary, hopefully)."
psql -h $PG_HOST -U $PG_USER -d $PG_DATABASE -c 'update t_program SET c_program_id = c_program_id' > /dev/null 2>&1

if [ "$TEST_MIGRATION" = "false" ]; then
  exit 0
fi

echo "🍏 Starting service to test migrations..."
# We need these environment variables, for the most part they are not used
export DATABASE_URL="postgres://$PG_USER:$PGPASSWORD@$PG_HOST:5432/$PG_DATABASE"
export PORT=8080
export ODB_ITC_ROOT="http://localhost:9090"
export ODB_SSO_ROOT="http://localhost:9091"
export ODB_SSO_PUBLIC_KEY="-----BEGIN PGP PUBLIC KEY BLOCK-----

mQINBGQ1w9IBEAC8Td3AlypimgzF2/isMZSD3z63sUKpd/Lc3XZHjpKwbgNHA7/3
7ddE7VB8526Cn8DJwArL39DlKdCV5VB1VguLjnSfYD1C6GEHMmhGB5T2QiVBFVZD
3/XvMTF/9akrwPp4Y6CxUNOWej9Bube+pvUQZ4e5gz4yCduIMwU/zODpy4BJVc1u
86l3Xrt1FmCIgRzpD4coVrhtjAtsuXVH8eZvgMfgFY2c8whBBv8upTHxCLKfxbCN
pS9nOaZE+3ujI/+xoVw6RiOwrMR683Rs46TZGOo7IfPmpLwxtQt+XwZUHeEC5bMT
7wG9jebPPc0Ro0wrkwf9N6J0Fnp+gdcIT2AruxtR5hjVcwckORM26RYnCJ+sirpU
Tu0kw754d7Uvwrr15cSMjvSA/qlvdmqaquOGXS+aqM/OPecAVpcUJADG4H2KAXGq
d79OuspC/CCBoA6HJb+TBneP6UflKRVnZrdlhKc001yGiHS4X19HaJCu5Co6PNbN
G7H2Z0+NVBHR/GIYGZ2DS/yjE0R07WhC4mCbehC01InWARNzDqmF5zcVZUi0Kmb7
YHlJPURCG4+9qi1SBgYhVmPmPASy/vjsBVadPp5aGQFjYupv8gW3LTeq/uW+CZUw
gbPA5SKTk0VIUxwH9qqkbod98S67fuTP9ryFRJEo5wZrWsPx7pgE7E2V8QARAQAB
tCdMdWN1bWEgU1NPIERldiA8cm9iLm5vcnJpc0Bub2lybGFiLmVkdT6JAlcEEwEI
AEEWIQS0yfZiKQanqInSO1pcW28wo0EWRAUCZDXD0gIbAwUJA8JnAAULCQgHAgIi
AgYVCgkICwIEFgIDAQIeBwIXgAAKCRBcW28wo0EWRLBPEAC3T2c5BhgJ++RahDbt
f3gPpq2nAbVJyn2VI37uFXIfNFKkYdFTQh/wlb+qprNqQrbPNnRWKebq9qzcubTd
sADSwrM4imbBeyOEJsceyPeP6yAfaWcSpFXwtuLTVMb+eB9u985qNmu7kIC7gnak
SjkBdbKsM3HQvr3PrsNCZsy9ysGRBdIfDc/DDwoGhCU0Bqd5ORjzsS4u7SNiRrkc
+Dw3siX4cskwiDbCr21Bz4XJxpU86cx+idhSS7naiX6rN6KqZRhAO2FZOhL8/11u
sQPshz45m1mvQ4367fams8N2gtpX+1RKuVY6xcSvfa7rK6aWpjGC7u0tr2hcK0G5
NCiI6DPYllC2lyZPonycHHRaGLIQWIipJkP9cdu8ph+O/7qshEtb7nX3JlyRIxcW
kxQnqROrVqJALogmzmF+4OP8gTjY2ph8OmaPU8ATjdql5da1iHlDT5M/0oatZ6J2
lmYdT0LxnSMlMGFb9xOo1xeYK0/a5kR4fRET4m4g+x5N9UUPSJjfFhDa6iO89X0V
d/EKiM3//ukkw7RcwGLWw4hnqqxPdHvLM0yTKajc79pAQR3rOEcW1SrV5PECFSxD
HMeMka0SYzCqqtl0XWI1dlC0JXKnVfuDHOKVY523EKnEAcHqZ8oAZB//2Puj4qfO
yMvjw3Rl9GQnMoTGYsNsunNy4Q==
=8OhQ
-----END PGP PUBLIC KEY BLOCK-----"
export ODB_SERVICE_JWT="dummy-jwt"
export CLOUDCUBE_ACCESS_KEY_ID="dummy"
export CLOUDCUBE_SECRET_ACCESS_KEY="dummy"
export CLOUDCUBE_URL="https://dummy.cloudcube.net/bucket"
export FILE_UPLOAD_MAX_MB="100"
export MAILGUN_API_KEY="dummy"
export MAILGUN_DOMAIN="dummy.mailgun.org"
export MAILGUN_WEBHOOK_SIGNING_KEY="dummy"
export INVITATION_SENDER_EMAIL="noreply@example.com"
export EXPLORE_URL="http://localhost:3000"
export ODB_DOMAIN="localhost"

# Start the odb, it will attempt to migrate
sbt -v -J-Xmx6g "service/run serve" > $SERVICE_LOG 2>&1 &
SERVICE_PID=$!

echo "🍏 Service PID: $SERVICE_PID"

tail -f $SERVICE_LOG &
TAIL_PID=$!

SUCCESS=false
TIMEOUT=300
ELAPSED=0

while [ $ELAPSED -lt $TIMEOUT ]; do
  if ! kill -0 $SERVICE_PID 2>/dev/null; then
    echo "🍎 Service process died"
    tail -100 $SERVICE_LOG
    exit 1
  fi

  if grep -q "Startup diagnostics passed" $SERVICE_LOG 2>/dev/null; then
    echo "🍏 Migrations completed and startup diagnostics passed"
    SUCCESS=true
    break
  fi

  if grep -q "Startup diagnostics failed" $SERVICE_LOG 2>/dev/null; then
    echo "🍎 Startup diagnostics failed"
    cat $SERVICE_LOG
    exit 1
  fi

  if grep -Eqi "error.*migration|migration.*failed" $SERVICE_LOG 2>/dev/null; then
    echo "🍎 Migration error detected"
    cat $SERVICE_LOG
    exit 1
  fi

  sleep 2
  ELAPSED=$((ELAPSED + 2))
done

kill $TAIL_PID 2>/dev/null || true
TAIL_PID=""
kill $SERVICE_PID 2>/dev/null || true
wait $SERVICE_PID 2>/dev/null || true
SERVICE_PID=""

if [ "$SUCCESS" = "true" ]; then
  echo "🍏 Migration test passed for $ENVIRONMENT"
  exit 0
else
  echo "🍎 Timeout waiting for service startup"
  tail -100 $SERVICE_LOG
  exit 1
fi
