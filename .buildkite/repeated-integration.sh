#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

echo "+++ cardano-wallet:integration"
nix-build -A checks.cardano-wallet.integration --no-out-link
