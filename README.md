# Lucuma Observing Database

This is the Postgres-backed observing database for GPP, under construction.

## Working on the Database Schema

The highlights:

- To start up a local database and a simple web UI: `docker-compose up -d`
- The database will be created and initialized if it doesn't yet exist.
- The Schema is defined by `.sql` files in `modules/service/src/main/resources/db/migration/`.
- To inspect the database using a web UI, go [here](http://localhost:8686)
- The password for user `jimmy` is `banana`.
- To connect using `psql` (if you have it): `psql -h localhost -U jimmy -d lucuma-odb`
- To connect using `psql` (if you don't have it): `docker-compose run db psql -h db -U jimmy -d lucuma-odb`
- To build a nice documentation website, look in `schemaapy/`
- To stop the database (and the web UI): `docker-compose stop`
- To stop **and delete** the database (and the web UI): `docker-compose down`

To work on the schema I just do `docker-compose down; docker-compose up` to wipe it and rebuild from the schema. Any errors will be reported on the console. For now we're not worried about migrations so files can be edited arbitrarily. Once we're deployed migrations will need to be append-only.

## Running the App

The application assumes that you have an empty database initially because it wants to run migrations. To set this up, start the docker container as above, then do this:

```
psql -h localhost -U jimmy -d postgres -e -c 'drop database "lucuma-odb"'
psql -h localhost -U jimmy -d postgres -e -c 'create database "lucuma-odb"'
```

You can now run the app, and you can do `docker-compose stop`, but if you do `down` and then `up` you'll need to clear out the db again.


## Access Control

The SSO server defines three types of users:

- **Guest** users, who have no identifying information;
- **Service** users, used internally when services need to communicate (not on behalf of a specific user); and
- **Standard** users, who are authenticated and have an ORCID id.

**Standard** users always wear a **hat**, which identifies their point of view.

  | Hat       | Access            | Notes                                           |
  |-----------|-------------------|-------------------------------------------------|
  | **Pi**    | My&nbsp;programs.      |                                                 |
  | **Ngo**   | Partner&nbsp;programs. | Hat is labeled with a **partner** and an **admin** bit. When wearing this hat I can see all programs to which my hat's partner has allocated time. If my hat's admit bit is set I can also edit them. |
  | **Staff** | All&nbsp;programs.[^0]     |                                                 |
  | **Admin** | All&nbsp;programs.     | Allows more operations than **Staff**, such as giving out hats. |

The ODB associates users with programs via **roles**.

| Role         | Count | Allowed User Types | Access     | Notes                             |
|--------------|:-----:|--------------------|------------|-----------------------------------|
| **Pi**       | 0..1  | Standard, Guest    | Read/Write |                                   |
| **Coi**      | 0..n  | Standard           | Read/Write | Some operations restricted.       |
| **Observer** | 0..n  | Standard           | Read       |                                   |
| **Support**  | 0..n  | Standard           | Read/Write | Labeled with **Gemini** or a with a **partner**. |

In summary:

| User Type     | Hat         | Pi  | Coi | Observer  | Support | None | Allocation |
|---------------|-------------|-----|-----|-----------|---------|------|------------|
| **Guest**     | --          | RW  |--   | --        | --      | --   | --         |
| **Standard**  | **Pi**      | RW  | RW  | R         |         |      | --         |
|               | **Ngo**     |     |     |           | RW[^1]  |      | R[W][^2]   |
|               | **Staff**   | RW  | RW  | RW        | RW      | RW   | --         |
|               | **Admin**   | RW  | RW  | RW        | RW      | RW   | --         |
| **Service**   | --          | RW  | RW  | RW        | RW      | RW   | --         |

[^0]: The UI will allow filtering by role, for example if a staff users only wants to see programs where they are in a support role.
[^1]: When in matching partner support role.
[^2]: When matching partner has allocated time [and **admin** bit is set].

