# Liminal Desiderata

A blog of technical essays and personal philosophy, exploring thresholds between systems design and human judgment.

## Overview

This blog is built with [org-static-blog](https://github.com/bastibe/org-static-blog), a minimal static site generator for Emacs org-mode. It emphasizes:

- **Clarity over cleverness** - Precise, reflective writing
- **Minimal dependencies** - Just Emacs and org-mode
- **Semantic HTML** - Clean, accessible markup
- **Timeless design** - Typography-focused CSS

## Directory Structure

```
.
├── .github/workflows/
│   └── deploy.yml           # GitHub Actions deployment
├── build/
│   └── publish.el           # Emacs Lisp build script
├── posts/                   # Published org-mode posts (source)
├── drafts/                  # Unpublished drafts
├── static/
│   └── style.css           # Minimal semantic CSS
├── public/                 # Generated HTML (git-ignored)
├── context.md              # Blog vision and philosophy
└── README.md               # This file
```

## Quick Start with `just`

This repository includes a [`justfile`](https://github.com/casey/just) for common tasks:

```bash
# Build and serve locally
just serve

# Create a new draft
just new-draft "Post Title"

# Publish a draft
just publish 2026-02-11-post-title.org

# Deploy to GitHub Pages
just deploy

# List all commands
just
```

See the [Manual Commands](#manual-commands) section below for alternatives without `just`.

## Writing Posts

### Creating a New Post

**With `just`:**
```bash
just new-draft "Your Post Title"
```

**In Emacs:**
```elisp
M-x org-static-blog-create-new-post
```

Both methods create a new org file with the required metadata.

### Post Structure

Each post requires:

```org
#+TITLE: Your Post Title
#+DATE: <2026-02-11 Tue>
#+FILETAGS: tag1 tag2
#+DESCRIPTION: Brief description for SEO

Your content here...
```

**Required:**
- `#+TITLE:` - Post title
- `#+DATE:` - Publication date in org-mode format

**Optional:**
- `#+FILETAGS:` - Space-separated tags
- `#+DESCRIPTION:` - Meta description

### Filename Convention

Posts follow the pattern: `YYYY-MM-DD-post-title.org`

Example: `2026-02-11-on-distributed-consensus.org`

This generates the URL: `/blog/2026-02-11-on-distributed-consensus.html`

## Building Locally

### Prerequisites

- Emacs 27.1 or later
- Python 3 (for local preview server)
- [just](https://github.com/casey/just) (optional, recommended)
- Internet connection (first build downloads org-static-blog from MELPA)

Install `just` (optional):
```bash
# Arch Linux
sudo pacman -S just

# Other systems: see https://github.com/casey/just#installation
```

### Build and Preview

**With `just`:**
```bash
just serve
```

**Manual:**
```bash
emacs --batch -l build/publish.el
cd public && python -m http.server 8000
```

Visit: http://localhost:8000

### Development Workflow

**Auto-rebuild on changes** (requires `entr`):
```bash
just watch
```

**Other useful commands:**
```bash
just build      # Build without serving
just rebuild    # Clean and rebuild from scratch
just validate   # Check posts for required metadata
just list       # Show all posts and drafts
```

## Deployment

### Quick Deploy

**With `just`:**
```bash
just deploy
```

This builds, commits, and pushes to GitHub, triggering automatic deployment.

### Automatic (GitHub Actions)

Every push to `main` triggers:
1. Build via `emacs --batch -l build/publish.el`
2. Deploy `public/` to GitHub Pages

### GitHub Pages Setup

1. Push this repository to GitHub
2. Go to repository Settings → Pages
3. Source: "GitHub Actions"
4. The blog will be available at: `https://eapolinario.github.io/blog/`

**Important:** The repository must be **public** for GitHub Pages to work on a free plan.

## Customization

### Styling

Edit `static/style.css` for visual changes. The current design emphasizes:
- Readable typography (serif body, sans-serif headings)
- Minimal color palette
- Responsive layout without frameworks
- Automatic dark mode support

### HTML Structure

Edit `build/publish.el` to customize:
- `org-static-blog-page-preamble` - Site header/nav
- `liminal-desiderata-post-preamble` - Post metadata display
- `org-static-blog-page-postamble` - Site footer

### Configuration

Key variables in `build/publish.el`:
- `org-static-blog-publish-title` - Blog title
- `org-static-blog-publish-url` - Base URL
- `org-static-blog-enable-tags` - Tag support (enabled)

## Drafts

Place unpublished posts in `drafts/` directory. They won't appear in:
- Index page
- Archive
- Tag pages
- RSS feed

Move to `posts/` when ready to publish.

## Tags

Tags are enabled and appear:
- In post metadata
- As individual tag pages (`tag_tagname.html`)
- In RSS feed as `<category>` elements

Tags don't affect URLs—all posts use date-based URLs.

## Philosophy

From `context.md`:

> **Liminal Desiderata** explores thresholds between systems design and human judgment, between engineering tradeoffs and values, between what exists and what is desired but not yet realized.

Posts should:
- Connect technical specifics to broader judgments
- Name tradeoffs explicitly
- Assume technical sophistication
- Optimize for long-term intellectual coherence

See `context.md` for complete vision and guidelines.

## Manual Commands

If you prefer not to use `just`, here are the manual equivalents:

| Task | Manual Command |
|------|----------------|
| Build | `emacs --batch -l build/publish.el` |
| Serve | `cd public && python -m http.server 8000` |
| Clean | `rm -rf public/*` |
| List posts | `ls -1 posts/*.org drafts/*.org` |
| Check deps | Check if `emacs`, `python`, `git` are installed |

## Resources

- [org-static-blog](https://github.com/bastibe/org-static-blog) - Static site generator
- [Org Mode](https://orgmode.org/) - Markup language documentation
- [GitHub Actions](https://docs.github.com/en/actions) - CI/CD platform
- [just](https://github.com/casey/just) - Command runner

## License

Content: © Eduardo Apolinario. All rights reserved.

Code (build scripts, CSS): MIT License
