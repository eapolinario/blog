;;; publish.el --- Build script for Liminal Desiderata blog -*- lexical-binding: t; -*-

;;; Commentary:
;; This script configures and runs org-static-blog to generate the blog.
;; Usage: emacs --batch -l build/publish.el

;;; Code:

;; Set up package management
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install org-static-blog if not present
(unless (package-installed-p 'org-static-blog)
  (package-install 'org-static-blog))

(require 'org-static-blog)

;; Core configuration
(setq org-static-blog-publish-title "Liminal Desiderata")
(setq org-static-blog-publish-url "https://eapolinario.github.io/blog/")
(setq org-static-blog-publish-directory "./public/")
(setq org-static-blog-posts-directory "./posts/")
(setq org-static-blog-drafts-directory "./drafts/")
(setq org-static-blog-enable-tags t)
(setq org-static-blog-langcode "en")

;; Page header - minimal, just CSS
(setq org-static-blog-page-header
      "<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link rel=\"stylesheet\" href=\"/blog/static/style.css\" type=\"text/css\"/>
<link rel=\"alternate\" type=\"application/rss+xml\" href=\"/blog/rss.xml\" title=\"RSS feed for Liminal Desiderata\">")

;; Page preamble - site header and navigation
(setq org-static-blog-page-preamble
      "<nav>
  <a href=\"/blog/index.html\">Home</a>
  <a href=\"/blog/archive.html\">Archive</a>
  <a href=\"/blog/rss.xml\">RSS</a>
</nav>
<header>
  <h1><a href=\"/blog/index.html\">Liminal Desiderata</a></h1>
  <p>Technical essays and personal philosophy at the thresholds</p>
</header>")

;; Page postamble - site footer
(setq org-static-blog-page-postamble
      "<footer>
  <p>Built with <a href=\"https://github.com/bastibe/org-static-blog\">org-static-blog</a></p>
</footer>")

;; Custom post preamble for semantic HTML
(defun liminal-desiderata-post-preamble (post-filename)
  "Generate semantic HTML preamble for POST-FILENAME."
  (concat
   "<article>\n"
   "<header class=\"post-header\">\n"
   "  <h2 class=\"post-title\">"
   (org-static-blog-get-title post-filename)
   "</h2>\n"
   "  <div class=\"post-meta\">\n"
   "    <time datetime=\""
   (format-time-string "%Y-%m-%d" (org-static-blog-get-date post-filename))
   "\">"
   (format-time-string "%B %d, %Y" (org-static-blog-get-date post-filename))
   "</time>\n"
   (let ((tags (org-static-blog-get-tags post-filename)))
     (if tags
         (concat
          "    <div class=\"post-tags\">\n"
          (mapconcat
           (lambda (tag)
             (concat "<a href=\"/blog/tag_" (downcase tag) ".html\">" tag "</a>"))
           tags
           " ")
          "\n    </div>\n")
       ""))
   "  </div>\n"
   "</header>\n"
   "<div class=\"post-content\">\n"))

;; Custom post postamble for semantic HTML
(defun liminal-desiderata-post-postamble (post-filename)
  "Generate semantic HTML postamble for POST-FILENAME."
  "</div>\n</article>\n")

;; Set custom functions
(setq org-static-blog-post-preamble #'liminal-desiderata-post-preamble)
(setq org-static-blog-post-postamble #'liminal-desiderata-post-postamble)

;; Publish the blog
(org-static-blog-publish)

(message "Blog published successfully to %s" org-static-blog-publish-directory)

;;; publish.el ends here
