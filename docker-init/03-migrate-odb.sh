#!/bin/bash
set -e

echo "Applying ODB migrations to lucuma-odb database..."

# Apply ODB migrations in order
ODB_MIGRATIONS_DIR="/docker-entrypoint-initdb.d/odb-migrations"

if [ -d "$ODB_MIGRATIONS_DIR" ]; then
    # Apply migrations in version order
    for migration_file in $(ls "$ODB_MIGRATIONS_DIR"/V*.sql | sort -V); do
        if [ -f "$migration_file" ]; then
            psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "lucuma-odb" < "$migration_file"
        fi
    done
else
    echo "Warning: ODB migrations directory $ODB_MIGRATIONS_DIR does not exist"
fi
