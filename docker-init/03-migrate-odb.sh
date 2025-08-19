#!/bin/bash
set -e

echo "Applying ODB migrations to lucuma-odb database..."

# Apply ODB migrations in order
ODB_MIGRATIONS_DIR="/docker-entrypoint-initdb.d/odb-migrations"

if [ -d "$ODB_MIGRATIONS_DIR" ]; then
    for migration_file in "$ODB_MIGRATIONS_DIR"/*.sql; do
        if [ -f "$migration_file" ]; then
            echo "Applying $(basename "$migration_file") to lucuma-odb"
            psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "lucuma-odb" < "$migration_file"
        fi
    done
else
    echo "Warning: ODB migrations directory $ODB_MIGRATIONS_DIR does not exist"
fi

echo "ODB migration complete."