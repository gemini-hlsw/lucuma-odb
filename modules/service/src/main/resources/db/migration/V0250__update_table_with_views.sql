
-- Procedure for updating a table for which one or more views have been defined.
-- Records the definition of each view, drops it, then does the table update
-- and finally recreates the views.

-- All credit to the robots for writing this one.

CREATE OR REPLACE PROCEDURE update_table_with_views(
    table_name text,
    update_statement text,
    VARIADIC view_names text[]
) LANGUAGE plpgsql
AS $$
DECLARE
    view_definitions text[];
    view_definition text;
BEGIN
    -- Get the current view definitions
    FOR i IN 1..array_length(view_names, 1) LOOP
        view_definition := pg_get_viewdef(view_names[i], true);
        view_definitions := array_append(view_definitions, view_definition);
    END LOOP;

    -- Drop the views
    FOR i IN 1..array_length(view_names, 1) LOOP
        EXECUTE format('DROP VIEW IF EXISTS %I', view_names[i]);
    END LOOP;

    -- Update the table
    EXECUTE update_statement;

    -- Recreate the views
    FOR i IN 1..array_length(view_names, 1) LOOP
        view_definition := view_definitions[i];
        EXECUTE format('CREATE VIEW %I AS %s', view_names[i], view_definition);
    END LOOP;
END;
$$;