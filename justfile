# Liminal Desiderata - Build automation with just
# https://github.com/casey/just

# Default recipe (show available commands)
default:
    @just --list

# Build the blog (generate HTML from org files)
build:
    #!/usr/bin/env bash
    # Use GUI Emacs in daemon mode to get theme colors
    export DISPLAY="${DISPLAY:-:0}"
    emacs --eval "(progn (load-file \"build/publish.el\") (kill-emacs))" 2>/dev/null

# Serve the blog locally for preview
serve: build
    #!/usr/bin/env bash
    cd public
    # Create symlink for local development (allows /blog/ paths to work)
    ln -sfn . blog
    echo "Starting local server at http://localhost:8000/blog/"
    echo "Visit: http://localhost:8000/blog/"
    echo "Press Ctrl+C to stop"
    echo ""
    python -m http.server 8000

# Clean generated files
clean:
    rm -rf public/*
    @echo "Cleaned public/ directory"

# Rebuild from scratch (clean + build)
rebuild: clean build

# Watch for changes and rebuild (requires entr)
watch:
    @echo "Watching for changes in posts/ and drafts/..."
    @echo "Install 'entr' if not available: sudo pacman -S entr"
    find posts drafts static build -type f | entr -c just build

# Create a new post (requires emacs in interactive mode)
new-post:
    @echo "Creating new post..."
    @echo "Run this in Emacs: M-x org-static-blog-create-new-post"
    @echo "Or create manually: posts/YYYY-MM-DD-title.org"

# Create a new draft post with today's date
new-draft TITLE:
    #!/usr/bin/env bash
    DATE=$(date +%Y-%m-%d)
    DAY=$(date +%a)
    SLUG=$(echo "{{TITLE}}" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | tr -cd '[:alnum:]-')
    FILE="drafts/${DATE}-${SLUG}.org"
    cat > "$FILE" << EOF
    #+TITLE: {{TITLE}}
    #+DATE: <${DATE} ${DAY}>
    #+FILETAGS: 
    #+DESCRIPTION: 

    * Introduction

    Write your post content here...
    EOF
    echo "Created draft: $FILE"

# Publish a draft (move from drafts/ to posts/)
publish FILENAME:
    #!/usr/bin/env bash
    if [ ! -f "drafts/{{FILENAME}}" ]; then
        echo "Error: drafts/{{FILENAME}} not found"
        exit 1
    fi
    mv "drafts/{{FILENAME}}" "posts/{{FILENAME}}"
    echo "Published: drafts/{{FILENAME}} → posts/{{FILENAME}}"
    just build

# Check for required dependencies
check-deps:
    @echo "Checking dependencies..."
    @command -v emacs >/dev/null 2>&1 && echo "✓ emacs installed" || echo "✗ emacs not found (required)"
    @command -v python >/dev/null 2>&1 && echo "✓ python installed" || echo "✗ python not found (required for serve)"
    @command -v git >/dev/null 2>&1 && echo "✓ git installed" || echo "✗ git not found (required)"
    @command -v entr >/dev/null 2>&1 && echo "✓ entr installed (optional, for watch)" || echo "✗ entr not found (optional, for watch)"

# List all posts (published and drafts)
list:
    @echo "📝 Published posts:"
    @ls -1 posts/*.org 2>/dev/null | sed 's|posts/||' || echo "  (none)"
    @echo ""
    @echo "🚧 Drafts:"
    @ls -1 drafts/*.org 2>/dev/null | sed 's|drafts/||' || echo "  (none)"

# Validate org files (check for required metadata)
validate:
    #!/usr/bin/env bash
    echo "Validating posts..."
    for file in posts/*.org; do
        [ -f "$file" ] || continue
        if ! grep -q "^#+TITLE:" "$file"; then
            echo "✗ Missing TITLE in $file"
        fi
        if ! grep -q "^#+DATE:" "$file"; then
            echo "✗ Missing DATE in $file"
        fi
    done
    echo "✓ Validation complete"

# Open the blog in default browser (after serving)
open: serve
    xdg-open http://localhost:8000/blog/

# Deploy to GitHub Pages (via git push, triggers Actions)
deploy: build
    @echo "Deploying to GitHub Pages..."
    @echo "This will:"
    @echo "  1. Stage all changes"
    @echo "  2. Commit with message"
    @echo "  3. Push to origin/main"
    @echo "  4. GitHub Actions will build and deploy"
    @echo ""
    @read -p "Continue? (y/N) " confirm && [ "$$confirm" = "y" ] || exit 1
    git add .
    git commit -m "Update blog content"
    git push origin main
    @echo "✓ Pushed to GitHub. Check Actions tab for deployment status."

# Quick commit and push (for content updates)
commit MESSAGE: build
    git add posts drafts static
    git commit -m "{{MESSAGE}}"
    git push origin main
    @echo "✓ Committed and pushed: {{MESSAGE}}"
