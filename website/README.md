# seqproc documentation site

This directory contains the Astro Starlight site published at
<https://combine-lab.github.io/seqproc/>.

Node.js 22.12 or newer is required. Install the locked dependencies and start a
local development server with:

```console
npm ci
npm run dev
```

Build the same static artifact used by GitHub Pages with:

```console
npm run build
```

Documentation pages live under `src/content/docs/`. Navigation is explicit in
`astro.config.mjs`; add a new page there as well as in the content tree.

The deployment workflow builds pull requests that modify `website/` and
deploys changes merged to `main` through GitHub Pages.
