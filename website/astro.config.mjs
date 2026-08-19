import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
  site: 'https://combine-lab.github.io',
  base: '/seqproc',
  integrations: [
    starlight({
      title: 'seqproc',
      description:
        'Geometry-driven, high-performance preprocessing for structured sequencing reads.',
      logo: {
        dark: './src/assets/seqproc_logo_oblique_wordmark_dark.svg',
        light: './src/assets/seqproc_logo_oblique_wordmark.svg',
        alt: 'seqproc',
        replacesTitle: true,
      },
      customCss: ['./src/styles/custom.css'],
      social: [
        {
          icon: 'github',
          label: 'GitHub',
          href: 'https://github.com/COMBINE-lab/seqproc',
        },
      ],
      editLink: {
        baseUrl: 'https://github.com/COMBINE-lab/seqproc/edit/main/website/',
      },
      sidebar: [
        {
          label: 'Getting started',
          items: [
            { label: 'Introduction', slug: 'getting-started/introduction' },
            { label: 'Installation', slug: 'getting-started/installation' },
            { label: 'Quick start', slug: 'getting-started/quick-start' },
            { label: 'Command line', slug: 'getting-started/command-line' },
          ],
        },
        {
          label: 'EFGDL',
          items: [
            { label: 'Language overview', slug: 'efgdl/overview' },
            { label: 'Intervals and layouts', slug: 'efgdl/intervals' },
            {
              label: 'Matching and transformations',
              slug: 'efgdl/matching-and-transformations',
            },
            {
              label: 'Annotations and ambiguity',
              slug: 'efgdl/annotations-and-ambiguity',
            },
          ],
        },
        {
          label: 'Guides',
          items: [
            { label: 'Protocol recipes', slug: 'guides/protocol-recipes' },
            { label: 'Compressed I/O', slug: 'guides/compressed-io' },
            { label: 'Demultiplexing', slug: 'guides/demultiplexing' },
            { label: 'Run summaries', slug: 'guides/summaries' },
            { label: 'Performance', slug: 'guides/performance' },
          ],
        },
        {
          label: 'Reference',
          items: [
            { label: 'Rust API', slug: 'reference/rust-api' },
            { label: 'Troubleshooting', slug: 'reference/troubleshooting' },
          ],
        },
        {
          label: 'Project',
          items: [
            { label: 'Citation and support', slug: 'about/citation' },
          ],
        },
      ],
    }),
  ],
});
