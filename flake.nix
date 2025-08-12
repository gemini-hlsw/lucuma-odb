{
  inputs = {
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    nixpkgs.follows = "typelevel-nix/nixpkgs";
    flake-utils.follows = "typelevel-nix/flake-utils";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, typelevel-nix, sops-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs-x86_64 = import nixpkgs { system = "x86_64-darwin"; };
        scala-cli-overlay = final: prev: { scala-cli = pkgs-x86_64.scala-cli; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlay scala-cli-overlay ];
        };

        # SOPS secrets
        secretValues = {
          DATABASE_URL = "postgres://user:SOPS_ENCRYPTED_PASSWORD@localhost:5432/lucuma-odb";
          ODB_SERVICE_JWT = "SOPS_ENCRYPTED_JWT_TOKEN";
          CLOUDCUBE_ACCESS_KEY_ID = "SOPS_ENCRYPTED_CLOUDCUBE_ACCESS_KEY_ID";
          CLOUDCUBE_SECRET_ACCESS_KEY = "SOPS_ENCRYPTED_CLOUDCUBE_SECRET";
          MAILGUN_API_KEY = "SOPS_ENCRYPTED_MAILGUN_KEY";
          MAILGUN_WEBHOOK_SIGNING_KEY = "SOPS_ENCRYPTED_WEBHOOK_KEY";
        };

      in {
        devShell = pkgs.devshell.mkShell {
          imports = [ typelevel-nix.typelevelShell ];
          packages = [
            pkgs.nodePackages.graphqurl
            pkgs.docker
            pkgs.docker-compose
            pkgs.postgresql_15
            pkgs.sops
            pkgs.age
            pkgs.yq
            pkgs.direnv
          ];
          typelevelShell = {
            nodejs.enable = true;
            jdk.package = pkgs.jdk17;
          };
          env = [
            {
              name = "ODB_ITC_ROOT";
              value = "https://itc-dev.lucuma.xyz/itc";
            }
            {
              name = "ODB_SSO_ROOT";
              value = "https://sso-dev.gpp.lucuma.xyz";
            }
            {
              # For odb-dev
              name = "ODB_SSO_PUBLIC_KEY";
              value = "-----BEGIN PGP PUBLIC KEY BLOCK-----

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
-----END PGP PUBLIC KEY BLOCK----- ";
            }
            {
              name = "FILE_UPLOAD_MAX_MB";
              value = "10";
            }
            {
              name = "ODB_DOMAIN";
              value = "lucuma.xyz, gemini.edu";
            }
            {
              name = "CLOUDCUBE_URL";
              value = "https://cloud-cube-us2.s3.amazonaws.com/lucuma-staging";
            }
            {
              name = "INVITATION_SENDER_EMAIL";
              value = "noreply@gemini.edu";
            }
            {
              name = "EXPLORE_URL";
              value = "https://explore-dev.lucuma.xyz";
            }
            {
              name = "MAILGUN_DOMAIN";
              value = "odb-dev.lucuma.xyz";
            }
            {
              name = "DATABASE_URL";
              value = secretValues.DATABASE_URL;
            }
            {
              name = "ODB_HONEYCOMB_WRITE_KEY";
              value = "";
            }
            {
              name = "ODB_HONEYCOMB_DATASET";
              value = "";
            }
            {
              name = "ODB_SERVICE_JWT";
              value = secretValues.ODB_SERVICE_JWT;
            }
            {
              name = "CLOUDCUBE_SECRET_ACCESS_KEY";
              value = secretValues.CLOUDCUBE_SECRET_ACCESS_KEY;
            }
            {
              name = "MAILGUN_API_KEY";
              value = secretValues.MAILGUN_API_KEY;
            }
            {
              name = "CLOUDCUBE_ACCESS_KEY_ID";
              value = secretValues.CLOUDCUBE_ACCESS_KEY_ID;
            }
            {
              name = "MAILGUN_WEBHOOK_SIGNING_KEY";
              value = secretValues.MAILGUN_WEBHOOK_SIGNING_KEY;
            }
          ];
        };
      }

    );
}
