;;; publish.el

;; Maintainer: Suat Karakusoglu <suatkarakusoglu@gmail.com>
;; Initialize package sources
(require 'package)

(setq user-full-name "Suat Karakusoglu")
(setq user-mail-address "suatkarakusoglu@gmail.com")

(defvar site/site-url (if (string-equal (getenv "PRODUCTION") "true")
                          "https://suat.page"
                        "http://localhost:8080")
  "The URL for the site being generated.")

(setq package-user-dir (expand-file-name "./.packages"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(require 'vc-git)
(require 'ox-publish)
(require 'subr-x)
(require 'cl-lib)
(require 'org-element)

(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package webfeeder
  :ensure t)

;; Generate tags
(defun get-org-filetags (org-files)
  (let ((filetags '()))
    (dolist (org-file org-files)
      (with-current-buffer (find-file-noselect org-file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (let ((tags (org-get-tags)))
             (setq filetags (append filetags tags))))
         nil 'file)))
    (cl-remove-duplicates filetags)))

(setq article-files (directory-files "./content/news/" t "\\.org$"))
(setq article-tags-vector (get-org-filetags article-files))
(setq article-tags-list (mapcar #'substring-no-properties article-tags-vector))

;; -- Tag count
(defun create-tag-count-map (article-tags-list)
  "Create a map containing the count of each tag in ARTICLE-TAGS-LIST."
  (let ((tag-map (make-hash-table :test 'equal))) ; create an empty hash table to store the counts
    (cl-loop for tag in article-tags-list
             do (puthash tag (1+ (gethash tag tag-map 0)) tag-map))
    tag-map))

(setq tag-count-map (create-tag-count-map article-tags-list))

;; Ordered unique tags
(setq article-tags-unique-list
      (sort (hash-table-keys tag-count-map)
            (lambda (tag1 tag2)
              (> (gethash tag1 tag-count-map 0)
                 (gethash tag2 tag-count-map 0)))))

;; Print number of article and tags
(maphash (lambda (k v) (princ (format "%s: %d\n" k v))) tag-count-map)
;; -- Tag count

(defvar tags-files-directory "./content/tags/")

(unless (file-exists-p tags-files-directory)
  (make-directory tags-files-directory))

(defun remove-files-in-directory (directory)
  (dolist (file (directory-files directory t))
    (unless (or (string-equal "." (substring file -1))
                (string-equal ".." (substring file -2)))
      (if (file-directory-p file)
          (remove-files-in-directory file)
        (delete-file file)))))

(remove-files-in-directory tags-files-directory)

(defun get-org-title (org-file-path)
  (let* ((buffer (find-file-noselect org-file-path))
         (title (with-current-buffer buffer
                  (org-element-map (org-element-parse-buffer) 'keyword
                    (lambda (keyword)
                      (when (string= (org-element-property :key keyword) "TITLE")
                        (org-element-property :value keyword)))
                    nil t))))
    (kill-buffer buffer)
    title))

(defun generate-tag-files-filled (tag-list)
  (mapc (lambda (tag)
          (let ((file-name (concat tags-files-directory (downcase tag) ".org")))
            (unless (file-exists-p file-name)
              (with-temp-buffer
                (insert "#+TITLE: " (replace-regexp-in-string "_" " " tag) "\n\n")
                (dolist (org-file article-files)
                  (when (member tag (get-org-filetags (list org-file)))
                    (let ((org-file-name (file-name-nondirectory org-file))
                          (org-file-title (get-org-title org-file)))
                      (insert (format "[[file:../../news/%s][%s]]\n\n" org-file-name org-file-title)))))
                (write-file file-name)))))
        tag-list))

(generate-tag-files-filled article-tags-unique-list)
;; Generate tags

(defvar yt-iframe-format
  (concat "<div class=\"video\">"
          "  <iframe src=\"https://www.youtube.com/embed/%s\" allowfullscreen></iframe>"
          "</div>"))

(defun site/embed-video (video-id)
  (format yt-iframe-format video-id))

(org-link-set-parameters
 "yt"
 :follow
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/watch?v="
            handle)))
 :export
 (lambda (path desc backend channel)
   (when (eq backend 'html)
     (site/embed-video path))))

(defun site/site-header ()
  (list `(header (@ (class "site-header"))
                 (div (@ (class "container"))
                      (div (@ (class "site-title"))
                           (h2 "Suat's Page")))
                 (div (@ (class "site-masthead"))
                      (div (@ (class "container"))
                           (nav (@ (class "nav"))
                                (a (@ (class "nav-link") (href "/")) "Me") " "
                                (a (@ (class "nav-link") (href "/news/")) "Contents") " "
                                (a (@ (class "nav-link") (href ,(concat site/site-url "/rss/"))) "RSS") " "
                                ))))))

(defun generate-tags-html (tags)
  (mapcar (lambda (tag)
            `(a (@ (class "tag") (href ,(concat site/site-url "/tags/" (downcase tag) "/")))
                ,(concat (replace-regexp-in-string "_" " " tag) " #" (number-to-string (gethash tag tag-count-map 0)))))
          tags))

(defun site/site-footer ()
  (list `(footer (@ (class "site-footer"))
                 (div (@ (class "container"))
                      (div (@ (class "row"))
                           (div (@ (class "column"))
                                (p
                                 (a (@ (rel "me") (href "https://github.com/suatkarakusoglu/suat_page")) "GitHub")
                                 " · "
                                 (a (@ (rel "me") (href "https://tr.linkedin.com/in/suat-karakusoglu")) "LinkedIn")
                                 ))))
                 (div (@ (class "container"))
                      (div (@ (class "row site-footer-tags"))
                           ,@(when article-tags-unique-list
                               (generate-tags-html article-tags-unique-list)))))))

(defun get-article-output-path (org-file pub-dir)
  (let ((article-dir (concat pub-dir
                             (downcase
                              (file-name-as-directory
                               (file-name-sans-extension
                                (file-name-nondirectory org-file)))))))

    (if (string-match "\\/index.org\\|\\/404.org$" org-file)
        pub-dir
      (progn
        (unless (file-directory-p article-dir)
          (make-directory article-dir t))
        article-dir))))

(defun site/get-commit-hash ()
  "Get the short hash of the latest commit in the current repository."
  (string-trim-right
   (with-output-to-string
     (with-current-buffer standard-output
       (vc-git-command t nil nil "rev-parse" "--short" "HEAD")))))

(cl-defun site/generate-page (title
                              content
                              info
                              &key
                              (publish-date)
                              (filetags)
                              (head-extra)
                              (pre-content)
                              (exclude-header)
                              (exclude-footer))
  (concat
   "<!-- Generated from " (site/get-commit-hash)  " on " (format-time-string "%Y-%m-%d @ %H:%M") " with " org-export-creator-string " -->\n"
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
           (head
            (meta (@ (charset "utf-8")))
            (meta (@ (author "Suat Karakusoglu")))
            (meta (@ (name "viewport")
                     (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
            (link (@ (rel "icon") (type "image/png") (href "/img/favicon.png")))
            (link (@ (rel "alternative")
                     (type "application/rss+xml")
                     (title "Suat's Page")
                     (href ,(concat site/site-url "/rss/news.xml"))))
            (link (@ (rel "stylesheet") (href ,(concat site/site-url "/fonts/iosevka-aile/iosevka-aile.css"))))
            (link (@ (rel "stylesheet") (href ,(concat site/site-url "/fonts/jetbrains-mono/jetbrains-mono.css"))))
            (link (@ (rel "stylesheet") (href ,(concat site/site-url "/css/code.css"))))
            (link (@ (rel "stylesheet") (href ,(concat site/site-url "/css/site.css"))))
            (link (@ (rel "stylesheet") (href ,(concat site/site-url "/css/code_highlighter_dark_theme.css"))))
            ,(when head-extra head-extra)
            (title ,(concat title "- Suat's Page")))
           (body ,@(unless exclude-header
                     (site/site-header))
                 (div (@ (class "container"))
                      (div (@ (class "site-post"))
                           (h1 (@ (class "site-post-title"))
                               ,title)
                           ,@(when filetags
                               (generate-tags-html filetags))
                           ,(when publish-date
                              `(p (@ (class "site-post-meta")) ,publish-date))
                           ,(if-let ((video-id (plist-get info :video)))
                                (site/embed-video video-id))
                           ,(when pre-content pre-content)
                           (div (@ (id "content"))
                                ,content)))
                 ,@(unless exclude-footer
                     (site/site-footer)))))))

(defun site/org-html-template (contents info)
  (site/generate-page (org-export-data (plist-get info :title) info)
                      contents
                      info
                      :publish-date (org-export-data (org-export-get-date info "%B %e, %Y") info)
                      :filetags (plist-get info :filetags)))

(defun site/org-html-link (link contents info)
  "Removes file extension and changes the path into lowercase file:// links."
  (when (and (string= 'file (org-element-property :type link))
             (string= "org" (file-name-extension (org-element-property :path link))))
    (org-element-put-property link :path
                              (downcase
                               (file-name-sans-extension
                                (org-element-property :path link)))))

  (let ((exported-link (org-export-custom-protocol-maybe link contents 'html info)))
    (cond
     (exported-link exported-link)
     ((string-match-p "\\(.*\\.\\(jpg\\|png\\)\\)" (org-element-property :raw-link link))
      (format "<img src=\"/img/%s\" class=\"content-image\" width=\"%s\">"
              (file-name-nondirectory (org-element-property :path link))
              ;; (org-export-read-attribute :attr_html link)
              ;; TODO Read from #+ATTR_HTML
              "100%"
              ))
     ((equal contents nil)
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              (org-element-property :raw-link link)))
     ((string-prefix-p "/" (org-element-property :raw-link link))
      (format "<a href=\"%s\">%s</a>"
              (org-element-property :raw-link link)
              contents))
     (t (org-export-with-backend 'html link contents info)))))

(defun site/make-heading-anchor-name (headline-text)
  (thread-last headline-text
               (downcase)
               (replace-regexp-in-string " " "-")
               (replace-regexp-in-string "[^[:alnum:]_-]" "")))

(defun site/org-html-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (level (min 7 (when level (1+ level))))
         (anchor-name (site/make-heading-anchor-name text))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         (format "<h%d%s><a id=\"%s\" class=\"anchor\" href=\"#%s\">¶</a>%s</h%d>%s"
                 level
                 (or attributes "")
                 anchor-name
                 anchor-name
                 text
                 level
                 (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

;; Path for pygments or command name
;; brew install pygments
(defvar pygments-path "pygmentize")

(defun pygments-org-html-code (code contents info)
  ;; Generating tmp file path.
  ;; Current date and time hash will ideally pass our needs.
  (setq temp-source-file (format "/tmp/pygmentize-%s.txt"(md5 (current-time-string))))
  ;; Writing block contents to the file.
  (with-temp-file temp-source-file (insert (org-element-property :value code)))
  (shell-command-to-string (format "%s -l \"%s\" -f html %s"
                                   pygments-path
                                   (or (org-element-property :language code) "")
                                   temp-source-file)))

;; (defun site/org-html-src-block (src-block _contents info)
;;   (let* ((lang (org-element-property :language src-block))
;;          (code (org-html-format-code src-block info)))
;;     (format "<pre>%s</pre>" (string-trim code))))

(defun site/org-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (attributes (org-export-read-attribute :attr_html special-block)))
    (format "<div class=\"%s center\">\n%s\n</div>"
            block-type
            (or contents
                (if (string= block-type "cta")
                    "Breathe."
                  "")))))

(org-export-define-derived-backend 'site-html 'html
                                   :translate-alist
                                   '((template . site/org-html-template)
                                     (link . site/org-html-link)
                                     (src-block . pygments-org-html-code)
                                     (special-block . site/org-html-special-block)
                                     (headline . site/org-html-headline))
                                   :options-alist
                                   '((:video "VIDEO" nil nil)))

(defun org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, using the FILENAME as the output directory."
  (let ((article-path (get-article-output-path filename pub-dir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pub-dir)
                 ;; The 404 page is a special case, it must be named "404.html"
                 (concat article-path
                         (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (concat "." (or (plist-get plist :html-extension)
                                          "html"))
                          plist
                          article-path))))

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./.org-cache/"
      org-export-with-section-numbers nil
      org-export-use-babel nil
      org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-tags 'not-in-toc
      org-html-htmlize-output-type 'css
      org-html-prefer-user-labels t
      org-html-link-home site/site-url
      org-html-link-use-abs-url t
      org-html-link-org-files-as-html t
      org-html-html5-fancy t
      org-html-self-link-headlines t
      org-export-with-toc nil
      make-backup-files nil)

(defun site/format-news-entry (entry style project)
  "Format posts with author and published data in the index page."
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]] - %s"
                 entry
                 (org-publish-find-title entry project)
                 (format-time-string "%B %d, %Y"
                                     (org-publish-find-date entry project))))
        ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun site/news-sitemap (title files)
  (format "#+title: %s\n\n%s"
          title
          (mapconcat (lambda (file)
                       (format "- %s\n" file))
                     (cadr files)
                     "\n")))

(defun site/rss-extract-title (html-file)
  "Extract the title from an HTML file."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (dom-text (car (dom-by-class dom "site-post-title"))))))

(defun site/rss-extract-date (html-file)
  "Extract the post date from an HTML file."
  (with-temp-buffer
    (insert-file-contents html-file)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (date-string (dom-text (car (dom-by-class dom "site-post-meta"))))
           (parsed-date (parse-time-string date-string))
           (day (nth 3 parsed-date))
           (month (nth 4 parsed-date))
           (year (nth 5 parsed-date)))
      ;; NOTE: Hardcoding this at 8am for now
      (encode-time 0 0 8 day month year))))

(setq webfeeder-title-function #'site/rss-extract-title
      webfeeder-date-function #'site/rss-extract-date)

(setq org-publish-project-alist
      (list '("site_project:main"
              :base-directory "./content"
              :base-extension "org"
              :publishing-directory "./docs"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)
            '("site_project:assets"
              :base-directory "./assets"
              :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|woff2\\|ttf"
              :publishing-directory "./docs"
              :recursive t
              :publishing-function org-publish-attachment)
            '("site_project:news"
              :base-directory "./content/news"
              :base-extension "org"
              :publishing-directory "./docs/news"
              :publishing-function org-html-publish-to-html
              :auto-sitemap t
              :sitemap-filename "../news.org"
              :sitemap-title "Contents"
              :sitemap-format-entry site/format-news-entry
              :sitemap-style list
              ;; :sitemap-function site/news-sitemap
              :sitemap-sort-files anti-chronologically
              :with-title nil
              :with-timestamps nil)
            '("site_project:tags"
              :base-directory "./content/tags"
              :base-extension "org"
              :publishing-directory "./docs/tags"
              :publishing-function org-html-publish-to-html
              :with-title nil
              :with-timestamps nil)))

(defun site/generate-redirects (redirects)
  (dolist (redirect redirects)
    (let ((output-path (concat "./docs/" (car redirect) "/index.html"))
          (redirect-url (concat site/site-url "/" (cdr redirect) "/")))
      (make-directory (file-name-directory output-path) t)
      (with-temp-file output-path
        (insert
         (site/generate-page "Redirecting..."
                             (concat "You are being redirected to "
                                     "<a href=\"" redirect-url "\">" redirect-url "</a>")
                             '()
                             :head-extra
                             (concat "<meta http-equiv=\"refresh\" content=\"0; url='" redirect-url "'\"/>")))))))

(defun site/publish ()
  "Publish the entire site."
  (interactive)
  (org-publish-all t)

  (webfeeder-build "rss/news.xml"
                   "./docs"
                   site/site-url
                   (let ((default-directory (expand-file-name "./docs/")))
                     (remove "news/index.html"
                             (directory-files-recursively "news"
                                                          ".*\\.html$")))
                   :builder 'webfeeder-make-rss
                   :title "Suat's Contents"
                   :description "Shared contents by Suat"
                   :author "Suat Karakusoglu")

  ;; Copy the domains file to ensure the custom domain resolves
  (copy-file ".domains" "docs/.domains" t)

  ;; Copy the .well-known folder
  (unless (file-exists-p "docs/.well-known")
    (copy-directory ".well-known" "docs/" t)))

(provide 'publish)
;;; publish.el ends here
