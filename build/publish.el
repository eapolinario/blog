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

;; Page preamble - simple header with navigation
(setq org-static-blog-page-preamble
      "<div class=\"header\">
  <div class=\"sitelinks\">
    <a href=\"/blog/index.html\">Home</a>
    |
    <a href=\"/blog/blog.html\">All Posts</a>
    |
    <a href=\"/blog/about.html\">About</a>
    |
    <a href=\"/blog/contact.html\">Contact</a>
  </div>
</div>")

;; Page postamble - simple footer
(setq org-static-blog-page-postamble
      "Created by <a href=\"https://github.com/bastibe/org-static-blog/\">Org Static Blog</a>")

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

;; Copy static files to public directory
(defun copy-static-files ()
  "Copy static directory to public directory."
  (let ((static-src "./static")
        (static-dest "./public/static"))
    (when (file-exists-p static-dest)
      (delete-directory static-dest t))
    (copy-directory static-src static-dest)))

;; Export static pages (About, Contact, etc.)
(defun export-static-pages ()
  "Export static org pages to HTML."
  (require 'ox-html)
  (let ((org-html-head (concat "<link rel=\"stylesheet\" href=\"/blog/static/style.css\" type=\"text/css\"/>"))
        (org-html-preamble org-static-blog-page-preamble)
        (org-html-postamble org-static-blog-page-postamble)
        (org-html-validation-link nil))
    (dolist (file (directory-files "./pages" t "\\.org$"))
      (let* ((output-file (concat "./public/" 
                                  (file-name-base file) 
                                  ".html")))
        (with-temp-buffer
          (insert-file-contents file)
          (org-html-export-as-html)
          (write-file output-file))
        (message "Exported %s to %s" file output-file)))))

;; Create a custom homepage
(defun create-homepage ()
  "Create a landing page that is different from the blog index."
  (let ((homepage-content
         "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link rel=\"stylesheet\" href=\"/blog/static/style.css\" type=\"text/css\"/>
<title>Liminal Desiderata</title>
</head>
<body>
<div id=\"preamble\" class=\"status\">
<div class=\"header\">
  <div class=\"sitelinks\">
    <a href=\"/blog/index.html\">Home</a>
    |
    <a href=\"/blog/blog.html\">All Posts</a>
    |
    <a href=\"/blog/about.html\">About</a>
    |
    <a href=\"/blog/contact.html\">Contact</a>
  </div>
</div>
</div>
<div id=\"content\" class=\"homepage\">
<header class=\"hero\">
  <h1>Liminal Desiderata</h1>
  <p class=\"subtitle\">Technical essays and personal philosophy at the thresholds</p>
</header>

<section class=\"intro\">
  <p>This blog explores the space between systems design and human judgment, between engineering tradeoffs and values, between what exists and what is desired but not yet realized.</p>
  
  <p>Written for experienced engineers, technical leaders, and reflective practitioners who are interested in distributed systems, infrastructure, career decisions, and the values encoded in the systems we build.</p>
  
  <div class=\"cta\">
    <a href=\"/blog/blog.html\" class=\"button\">Read the Blog</a>
    <a href=\"/blog/about.html\" class=\"button-secondary\">Learn More</a>
  </div>
</section>
</div>
<div id=\"postamble\" class=\"status\">Created by <a href=\"https://github.com/bastibe/org-static-blog/\">Org Static Blog</a></div>
</body>
</html>"))
    (with-temp-file "./public/index.html"
      (insert homepage-content))
    (message "Created custom homepage")))

;; Rename org-static-blog's index to blog.html
(defun rename-blog-index ()
  "Rename the auto-generated index.html to blog.html."
  (when (file-exists-p "./public/index.html")
    (rename-file "./public/index.html" "./public/blog.html" t)
    (message "Renamed index.html to blog.html")))

;; Publish the blog
(org-static-blog-publish)

;; Rename blog index
(rename-blog-index)

;; Create custom homepage
(create-homepage)

;; Export static pages
(export-static-pages)

;; Copy static files after publishing
(copy-static-files)

(message "Blog published successfully to %s" org-static-blog-publish-directory)
(message "Static files copied to %s" "./public/static")

;;; publish.el ends here
