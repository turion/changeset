# changeset

This repository uses the `auto-merge-trusted` workflow to enable squash auto-merge for same-repository Dependabot pull requests after required checks pass. Flake-lock update pull requests are not included: existing update PRs are authored by `turion`, and branch names are not a trustworthy identity. Add flake-lock auto-merge only after switching the updater to a dedicated bot or GitHub App identity and confirming that identity. Branch protection must require the CI `success` check, and **Allow auto-merge** must be enabled in repository settings.
