# Invitations to Collaborate

We need a way for collaborators to invite others to participate in a science program.

- The Pi can invite Cois and Spectators
- A Coi can invite Spectators

An invitation is an id-prefixed token (same as we do with API keys) that we mail to a person. We retain:

  - an id
  - a hash of the token
  - the name and email address of the recipient (arbitrary)
  - the id of the user who created the invitation
  - the target program
  - the target role

Program owners have the ability to view, re-send, and cancel outstanding invitations.

When the invited user logs in they can enter the invitation code to gain access to the target program.

