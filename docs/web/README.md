# Web documentation sources

The guides published at the Ignis website. They live here rather than in the website repository so
that a release carries its own documentation: checking out a tag gives both the prose for that
version and the compiler that generates its API reference.

## Contract

Every file is Markdown with frontmatter:

```yaml
---
title: Records
description: One sentence, used as the page lede and in search results.
section: getting-started | language | std | tooling
order: 2
status: stable | experimental
---
```

`section` and `order` decide where the page sits in the sidebar. `status: experimental` renders a
warning banner on the page.

Code blocks tagged `ignis` are parsed with the tree-sitter grammar when the site builds, and a block
that fails to parse fails the build. An example here is expected to compile.

## Releases

`.github/workflows/release.yml` bundles this directory together with the API reference produced by
`ignis doc` and attaches the result to the release as `ignis-docs-<version>.tar.gz`. The website
reads those assets, so a version's documentation is frozen at the moment it was released and is
never regenerated from a later compiler.
