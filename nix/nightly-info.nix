# Rolling nightly channel pointer.
#
# THIS FILE IS AUTO-UPDATED by .github/workflows/nightly.yml on the `nightly`
# git ref immediately before the nightly tag is force-moved. On `main` it is
# only a seed: the hash below is a placeholder and will NOT fetch successfully
# from main.
#
# Consume the nightly through the pinned ref:
#
#   nix run github:Ignis-lang/ignis/nightly#ignis-nightly
#
# From-source nightly (no hash needed):
#
#   nix run github:Ignis-lang/ignis/nightly#ignis-source
{
  version = "nightly";

  artifacts = {
    "x86_64-linux" = {
      url = "https://github.com/Ignis-lang/ignis/releases/download/nightly/ignis-nightly-linux-amd64.tar.gz";
      hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    };
  };
}
