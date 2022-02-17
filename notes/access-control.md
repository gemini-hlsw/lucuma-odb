# Access Control

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

