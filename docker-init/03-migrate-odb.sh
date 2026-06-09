#!/bin/bash
set -e

echo "Applying ODB migrations to lucuma-odb database..."

# Apply ODB migrations in order
ODB_MIGRATIONS_DIR="/docker-entrypoint-initdb.d/odb-migrations"

if [ -d "$ODB_MIGRATIONS_DIR" ]; then
    # Apply migrations in version order
    while IFS= read -r migration_file; do
        if [ -f "$migration_file" ]; then
            psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "lucuma-odb" < "$migration_file"
        fi
    done < <(ls "$ODB_MIGRATIONS_DIR"/V*.sql | sort -V)
else
    echo "Warning: ODB migrations directory $ODB_MIGRATIONS_DIR does not exist"
fi
