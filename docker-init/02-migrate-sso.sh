#!/bin/bash
set -e

echo "Applying SSO migrations to lucuma-sso database..."

# Apply SSO migrations in order
SSO_MIGRATIONS_DIR="/docker-entrypoint-initdb.d/sso-migrations"

if [ -d "$SSO_MIGRATIONS_DIR" ]; then
    # Apply migrations in version order
    for migration_file in $(ls "$SSO_MIGRATIONS_DIR"/V*.sql | sort -V); do
        if [ -f "$migration_file" ]; then
            psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "lucuma-sso" < "$migration_file"
        fi
    done
else
    echo "Warning: SSO migrations directory $SSO_MIGRATIONS_DIR does not exist"
fi

