#!/usr/bin/env -S uv run --quiet --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["cryptography>=42", "pyjwt>=2.8"]
# ///
"""Local stand-in for lucuma-sso: an RSA key the ODB trusts, and JWTs for any user/role.

  mint_token.py keygen <dir>                 writes <dir>/sso-key.pem and <dir>/sso-key.asc
  mint_token.py token <dir> <who> [--id N]   prints a JWT signed by <dir>/sso-key.pem

<who> is one of: pi, staff, admin, ngo:<PARTNER>, service, guest, optionally
suffixed @<id> (e.g. pi@0x2001) for a distinct user of that kind.
"""

import argparse
import base64
import json
import sys
import time
from pathlib import Path

import jwt
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import rsa

DEFAULT_IDS = {"pi": 0x1001, "staff": 0x1002, "admin": 0x1003, "ngo": 0x1004, "service": 0x1005, "guest": 0x1006}


def mpi(n: int) -> bytes:
    raw = n.to_bytes((n.bit_length() + 7) // 8, "big")
    return n.bit_length().to_bytes(2, "big") + raw


def crc24(data: bytes) -> int:
    crc = 0xB704CE
    for b in data:
        crc ^= b << 16
        for _ in range(8):
            crc <<= 1
            if crc & 0x1000000:
                crc ^= 0x1864CFB
    return crc & 0xFFFFFF


def pgp_armor(pub: rsa.RSAPublicKey) -> str:
    # A bare v4 RSA public-key packet, the same shape GpgPublicKeyReader.armorText produces.
    nums = pub.public_numbers()
    body = b"\x04" + int(time.time()).to_bytes(4, "big") + b"\x01" + mpi(nums.n) + mpi(nums.e)
    packet = bytes([0x99]) + len(body).to_bytes(2, "big") + body
    b64 = base64.b64encode(packet).decode()
    lines = [b64[i : i + 64] for i in range(0, len(b64), 64)]
    crc = base64.b64encode(crc24(packet).to_bytes(3, "big")).decode()
    return "-----BEGIN PGP PUBLIC KEY BLOCK-----\n\n" + "\n".join(lines) + f"\n={crc}\n-----END PGP PUBLIC KEY BLOCK-----\n"


def orcid(id_: int) -> str:
    ds = f"{id_:015d}"
    total = 0
    for c in ds:
        total = (total + int(c)) * 2
    r = (12 - total % 11) % 11
    x = "X" if r == 10 else str(r)
    return f"{ds[0:4]}-{ds[4:8]}-{ds[8:12]}-{ds[12:15]}{x}"


def user_json(who: str, id_: int) -> dict:
    uid = f"u-{id_:x}"
    if who == "service":
        return {"type": "service", "id": uid, "name": f"verify-service-{id_:x}"}
    if who == "guest":
        return {"type": "guest", "id": uid}
    kind, _, partner = who.partition(":")
    if kind not in ("pi", "staff", "admin", "ngo"):
        sys.exit(f"unknown user kind: {who}")
    role = {"type": kind, "id": f"r-{id_:x}"}
    if kind == "ngo":
        if not partner:
            sys.exit("ngo needs a partner, e.g. ngo:US")
        role["partner"] = partner.upper()
    return {
        "type": "standard",
        "id": uid,
        "role": role,
        "otherRoles": [],
        "profile": {
            "orcidId": orcid(id_),
            "profile": {
                "givenName": f"Verify{kind.capitalize()}",
                "familyName": f"User{id_:x}",
                "creditName": None,
                "email": f"verify-{kind}-{id_:x}@example.com",
            },
        },
    }


def keygen(d: Path) -> None:
    d.mkdir(parents=True, exist_ok=True)
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    pem = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    (d / "sso-key.pem").write_bytes(pem)
    (d / "sso-key.pem").chmod(0o600)
    (d / "sso-key.asc").write_text(pgp_armor(key.public_key()))


def token(d: Path, who: str, id_: int | None) -> str:
    key = serialization.load_pem_private_key((d / "sso-key.pem").read_bytes(), password=None)
    who, _, at = who.partition("@")
    if at:
        id_ = int(at, 0)
    id_ = id_ if id_ is not None else DEFAULT_IDS[who.partition(":")[0]]
    user = user_json(who, id_)
    now = int(time.time())
    claims = {
        "lucuma-user": user,
        "iss": "lucuma-sso",
        "sub": user["id"],
        "aud": ["lucuma"],
        "exp": now + 12 * 3600,
        "nbf": now - 10,
        "iat": now - 10,
    }
    return jwt.encode(claims, key, algorithm="RS512")


def main() -> None:
    p = argparse.ArgumentParser()
    sub = p.add_subparsers(dest="cmd", required=True)
    k = sub.add_parser("keygen")
    k.add_argument("dir", type=Path)
    t = sub.add_parser("token")
    t.add_argument("dir", type=Path)
    t.add_argument("who")
    t.add_argument("--id", type=lambda s: int(s, 0))
    a = p.parse_args()
    if a.cmd == "keygen":
        keygen(a.dir)
    else:
        print(token(a.dir, a.who, a.id))


if __name__ == "__main__":
    main()
