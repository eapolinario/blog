;;; publish.el --- Build script for Liminal Desiderata using org-publish -*- lexical-binding: t; -*-

;;; Commentary:
;; This script uses org-publish (built into org-mode) to generate the blog.
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

;; Install htmlize for syntax highlighting
(unless (package-installed-p 'htmlize)
  (package-install 'htmlize))

(require 'ox-html)
(require 'htmlize)

;; Force graphical mode in batch mode to get theme colors
;; This creates an invisible frame so face definitions include colors
(when noninteractive
  (setq display-type 'color)
  (when (and (getenv "DISPLAY")
             (not (eq window-system 'x)))
    ;; Try to initialize X window system
    (ignore-errors
      (x-initialize-window-system)
      ;; Create a minimal invisible frame
      (make-frame-on-display (getenv "DISPLAY") 
                             '((visibility . nil)
                               (width . 10)
                               (height . 10))))))

;; Load leuven theme for syntax highlighting (built-in light theme)
(load-theme 'leuven t)

;; Configure htmlize to generate CSS file from theme
(setq org-html-htmlize-output-type 'css)  ;; Generate CSS classes
(setq org-html-htmlize-font-prefix "org-")
(setq org-src-fontify-natively t)

;; Generate CSS file from htmlize with current theme
(defun generate-htmlize-css ()
  "Generate CSS file from htmlize based on current theme."
  (require 'htmlize)
  (let ((htmlbuf (with-temp-buffer
                   (insert "def hello():\n")
                   (insert "    \"\"\"Docstring\"\"\"\n")
                   (insert "    return 'world'\n")
                   (python-mode)
                   (font-lock-ensure)
                   (font-lock-fontify-buffer)
                   (htmlize-buffer))))
    (with-current-buffer htmlbuf
      (goto-char (point-min))
      (when (search-forward "<style type=\"text/css\">" nil t)
        (let* ((start (point))
               (end (progn (search-forward "</style>") (match-beginning 0)))
               (css (buffer-substring-no-properties start end)))
          (with-temp-file "./public/static/htmlize.css"
            (insert css))
          (message "Generated htmlize.css from theme"))))
    (kill-buffer htmlbuf)))

(generate-htmlize-css)

;; Configure HTML export
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-validation-link nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

;; Blog configuration
(defvar blog-base-url "https://eapolinario.github.io/blog/")
(defvar blog-title "Liminal Desiderata")
(defvar blog-subtitle "Technical essays and personal philosophy at the thresholds")

;; HTML head with CSS (with cache-busting timestamp)
(setq org-html-head
      (format "<link rel=\"stylesheet\" href=\"/blog/static/style.css?v=%s\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"/blog/static/htmlize.css?v=%s\" type=\"text/css\"/>
<link rel=\"alternate\" type=\"application/rss+xml\" href=\"/blog/rss.xml\" title=\"RSS feed for Liminal Desiderata\">"
              (format-time-string "%s")
              (format-time-string "%s")))

;; HTML preamble (navigation)
(setq org-html-preamble
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

;; HTML postamble (footer)
(setq org-html-postamble
      "<div id=\"postamble\" class=\"status\">
Created by <a href=\"https://orgmode.org\">Org Mode</a>
</div>")

;; Custom publishing function for hierarchical URLs
(defun blog-publish-post-with-date-path (plist filename pub-dir)
  "Publish post to YYYY/MM/DD/title.html structure."
  (let* ((date (with-temp-buffer
                 (insert-file-contents filename)
                 (goto-char (point-min))
                 (if (re-search-forward "^#\\+DATE: *<\\([^>]+\\)>" nil t)
                     (date-to-time (match-string 1))
                   (current-time))))
         (year (format-time-string "%Y" date))
         (month (format-time-string "%m" date))
         (day (format-time-string "%d" date))
         (base-name (file-name-sans-extension (file-name-nondirectory filename)))
         ;; Extract title slug from filename (after YYYY-MM-DD-)
         (title-slug (if (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-\\(.*\\)$" base-name)
                         (match-string 1 base-name)
                       base-name))
         (date-dir (expand-file-name (format "%s/%s/%s" year month day) pub-dir)))
    ;; Create directory structure
    (make-directory date-dir t)
    ;; Temporarily override publishing directory
    (let ((org-publish-timestamp-directory (plist-get plist :timestamp-directory))
          (plist (plist-put (copy-sequence plist) :publishing-directory date-dir)))
      ;; Publish the file
      (org-html-publish-to-html plist filename date-dir))
    ;; Rename the output to use title slug
    (let ((default-output (expand-file-name (concat base-name ".html") date-dir))
          (final-output (expand-file-name (concat title-slug ".html") date-dir)))
      (when (and (file-exists-p default-output)
                 (not (string= default-output final-output)))
        (rename-file default-output final-output t)))))

;; Define publishing projects
(setq org-publish-project-alist
      `(
        ;; Blog posts
        ("blog-posts"
         :base-directory "./posts"
         :base-extension "org"
         :publishing-directory "./public"
         :recursive nil
         :publishing-function blog-publish-post-with-date-path
         :with-author t
         :with-creator nil
         :with-date t
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil
         :html-head ,org-html-head
         :html-preamble ,org-html-preamble
         :html-postamble ,org-html-postamble
         :htmlized-source t)

        ;; Static pages (about, contact)
        ("blog-pages"
         :base-directory "./pages"
         :base-extension "org"
         :publishing-directory "./public"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-creator nil
         :with-date nil
         :with-toc nil
         :section-numbers nil
         :html-head ,org-html-head
         :html-preamble ,org-html-preamble
         :html-postamble ,org-html-postamble)

        ;; Static assets (CSS, JS, images)
        ("blog-static"
         :base-directory "./static"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|ico"
         :publishing-directory "./public/static"
         :recursive t
         :publishing-function org-publish-attachment)

        ;; Complete project
        ("blog" :components ("blog-posts" "blog-pages" "blog-static"))))

;; Helper function to get all posts sorted by date
(defun blog-get-posts ()
  "Get all blog posts sorted by date (newest first)."
  (let ((posts (directory-files "./posts" t "\\.org$")))
    (sort posts
          (lambda (a b)
            (let ((date-a (blog-get-post-date a))
                  (date-b (blog-get-post-date b)))
              (time-less-p date-b date-a))))))

(defun blog-get-post-date (file)
  "Extract date from org file FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+DATE: *<\\([^>]+\\)>" nil t)
        (date-to-time (match-string 1))
      (time-since 0))))

(defun blog-get-post-title (file)
  "Extract title from org file FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE: *\\(.*\\)$" nil t)
        (match-string 1)
      (file-name-base file))))

(defun blog-get-post-tags (file)
  "Extract tags from org file FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+FILETAGS: *\\(.*\\)$" nil t)
        (split-string (match-string 1))
      nil)))

;; Helper function to generate post URL from filename
(defun blog-get-post-url (file)
  "Generate hierarchical URL for post FILE."
  (let* ((date (blog-get-post-date file))
         (year (format-time-string "%Y" date))
         (month (format-time-string "%m" date))
         (day (format-time-string "%d" date))
         (base-name (file-name-sans-extension (file-name-nondirectory file)))
         (title-slug (if (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-\\(.*\\)$" base-name)
                         (match-string 1 base-name)
                       base-name)))
    (format "%s/%s/%s/%s.html" year month day title-slug)))

;; Generate blog index (list of posts)
(defun blog-generate-index ()
  "Generate blog index page."
  (with-temp-file "./public/blog.html"
    (insert "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link rel=\"stylesheet\" href=\"/blog/static/style.css\" type=\"text/css\"/>
<title>" blog-title "</title>
</head>
<body>
<div id=\"preamble\" class=\"status\">")
    (insert org-html-preamble)
    (insert "</div>
<div id=\"content\">
<h1 class=\"title\">Recent Posts</h1>\n")
    (dolist (post (blog-get-posts))
      (let* ((title (blog-get-post-title post))
             (date (blog-get-post-date post))
             (tags (blog-get-post-tags post))
             (post-url (blog-get-post-url post)))
        (insert (format "<h2 class=\"post-title\"><a href=\"/blog/%s\">%s</a></h2>\n" post-url title))
        (insert (format "<div class=\"post-date\">%s</div>\n" (format-time-string "%d %b %Y" date)))
        (when tags
          (insert "<div class=\"taglist\"><a href=\"/blog/tags.html\">Tags</a>: ")
          (dolist (tag tags)
            (insert (format "<a href=\"/blog/tag-%s.html\">%s</a> " tag tag)))
          (insert "</div>\n"))))
    (insert "</div>\n")
    (insert org-html-postamble)
    (insert "</body>\n</html>")))

;; Generate homepage
(defun blog-generate-homepage ()
  "Generate custom homepage."
  (with-temp-file "./public/index.html"
    (insert "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<link rel=\"stylesheet\" href=\"/blog/static/style.css\" type=\"text/css\"/>
<title>" blog-title "</title>
</head>
<body>
<div id=\"preamble\" class=\"status\">")
    (insert org-html-preamble)
    (insert "</div>
<div id=\"content\" class=\"homepage\">
<header class=\"hero\">
  <h1>" blog-title "</h1>
  <p class=\"subtitle\">" blog-subtitle "</p>
</header>
<section class=\"intro\">
  <p>This blog explores the space between systems design and human judgment, between engineering tradeoffs and values, between what exists and what is desired but not yet realized.</p>
  <p>Written for experienced engineers, technical leaders, and reflective practitioners who are interested in distributed systems, infrastructure, career decisions, and the values encoded in the systems we build.</p>
  <div class=\"cta\">
    <a href=\"/blog/blog.html\" class=\"button\">Read the Blog</a>
    <a href=\"/blog/about.html\" class=\"button-secondary\">Learn More</a>
  </div>
</section>
</div>\n")
    (insert org-html-postamble)
    (insert "</body>\n</html>")))

;; Publish everything
(message "Publishing blog...")
(org-publish-all t)

;; Generate custom pages
(message "Generating index pages...")
(blog-generate-index)
(blog-generate-homepage)

(message "Blog published successfully!")

;;; publish.el ends here
