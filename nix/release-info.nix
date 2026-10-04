{
  version = "0.4.0";

  # SHA256 SRI hash of each prebuilt artifact published in the matching GitHub
  # Release. This file is the stable channel pointer: it tracks the newest
  # published release tag. The rolling nightly channel is separate (see
  # nix/nightly-info.nix).
  #
  # To refresh after a new release:
  #
  #   ver=X.Y.Z
  #   curl -fsSL -o /tmp/ignis-linux-amd64.tar.gz \
  #     "https://github.com/Ignis-lang/ignis/releases/download/v$ver/ignis-linux-amd64.tar.gz"
  #   nix-hash --to-sri --type sha256 \
  #     "$(sha256sum /tmp/ignis-linux-amd64.tar.gz | cut -d' ' -f1)"
  #
  # Then update `version`, the `url` and the `hash` below.
  artifacts = {
    "x86_64-linux" = {
      url = "https://github.com/Ignis-lang/ignis/releases/download/v0.4.0/ignis-linux-amd64.tar.gz";
      hash = "sha256-8Y1OqzA7i0w/+9dldY/weAVGWTQ7jN4IXtn9n1bP/Jw=";
    };
  };
}
