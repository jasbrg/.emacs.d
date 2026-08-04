;;;; Jotbooks Publishing

;; Notebook scans (multi-page PDFs from the iPhone) live under
;; `my/jotbooks-directory'.  Every PDF is one jotbook, published at its
;; directory path — analog/mug.pdf appears under /jotbooks/analog/mug/.
;; Directories are sections: each gets an index page listing the
;; jotbooks beneath it, its own RSS feed, and a crumb in the breadcrumb
;; bar.  Publishing renders each page and a thumbnail to JPEG
;; (pdftoppm) and generates one org file per page with prev/next
;; navigation, a thumbnail sheet per jotbook, and a master index — all
;; under `my/jotbooks-staging-directory', which org-publish then
;; exports.  The render/generate step runs as the project's
;; :preparation-function, so dropping a new PDF or sub-directory into
;; the Jotbooks directory is picked up by a plain `org-publish-all'.
;;
;; Jotbooks form one continuous archive in reading order — strictly by
;; creation time (an explicit #+DATE, else org-node's :TIME_CREATED:,
;; else the PDF's modification time), descending into sections along
;; the way: the sections let a reader find and select a thing, but
;; clicking through its pages sweeps them into the linear flow of time.
;; The last page of a jotbook links onward to its successor's first
;; page — or up to the master index at the end — and the first page
;; links back into its predecessor.
;;
;; Optional metadata comes from <stem>.meta.org beside a PDF: #+TITLE,
;; #+FILETAGS, #+CATEGORY, #+DESCRIPTION, #+DATE keywords, plus free
;; text that becomes the jotbook index's introduction.  A meta.org
;; inside a section directory titles and introduces the section's own
;; index the same way.  Tags and categories get index pages under
;; tags/ and categories/, and RSS feeds are generated for the whole
;; archive (feed.xml), per section, and per tag and category.

(require 'url-util)

(defvar my/jotbooks-directory "~/Documents/Jotbooks")  ;; write
(defvar my/jotbooks-staging-directory                  ;; generated
  (file-name-concat my/jotbooks-directory ".staging"))
(defvar my/jotbooks-html-directory                     ;; read
  (file-name-concat my/html-blog-directory "jotbooks"))

(defvar my/jotbooks-site-url "https://jasbrg.com"
  "Absolute site root, used for the RSS feeds' links.")

(defvar my/jotbooks-page-pixels 1600
  "Long edge, in pixels, of a rendered notebook page.")
(defvar my/jotbooks-thumb-pixels 320
  "Long edge, in pixels, of a page thumbnail.")

(defvar my/jotbooks-html-head
  (concat
   "<style>"
   " .jotpage img { display: block; margin: 1em auto; max-width: 100%; height: auto; }"
   " .sheet img { height: 160px; width: auto; margin: 4px; border: 1px solid #999; }"
   " .pagenav { text-align: center; }"
   " </style>"
   "<link rel=\"alternate\" type=\"application/rss+xml\" title=\"Jotbooks\""
   " href=\"/jotbooks/feed.xml\">"
   "<script>document.addEventListener('keydown', function (e) {"
   " if (e.key !== 'ArrowLeft' && e.key !== 'ArrowRight') return;"
   " var l = document.querySelector('link[rel=' + (e.key === 'ArrowLeft' ? 'prev' : 'next') + ']');"
   " if (l) location.href = l.href; });</script>"))

;;; Scanning & metadata

(defun my/jotbooks--scan (dir rel)
  "Recursively collect jotbooks and sections under DIR.
REL is DIR's path relative to `my/jotbooks-directory' (\"\" at the
root).  Every PDF is one jotbook named by its path sans extension;
every directory with a PDF somewhere beneath it is a section.
Returns (NOTEBOOKS . SECTIONS), where SECTIONS are (REL . DIR) pairs
excluding the root itself."
  (let (notebooks sections)
    (dolist (name (directory-files dir))
      (unless (string-prefix-p "." name)
        (let ((path (file-name-concat dir name))
              (child (if (equal rel "") name (concat rel "/" name))))
          (cond
           ((string-match-p "\\.pdf\\'" name)
            (push (list :name (file-name-sans-extension child)
                        :pdfs (list path)
                        :meta-file (concat (file-name-sans-extension path)
                                           ".meta.org"))
                  notebooks))
           ((file-directory-p path)
            (let ((below (my/jotbooks--scan path child)))
              (when (car below)
                (push (cons child path) sections)
                (setq notebooks (append (car below) notebooks)
                      sections (append (cdr below) sections)))))))))
    (cons notebooks sections)))

(defun my/jotbooks--meta (notebook)
  "Read NOTEBOOK's metadata file into an alist of (KEYWORD . VALUE).
A leading `:PROPERTIES:'/`:END:' drawer — as org-node maintains, with
:ID:, :TIME_CREATED:, and :TIME_MODIFIED: — is read the same as any
`#+KEYWORD:' line, so a meta.org captured via `org-node' (`C-c n c')
needs no extra keywords of its own.  Keywords are downcased;
non-keyword, non-drawer content is collected under \"body\"."
  (let ((file (plist-get notebook :meta-file))
        meta body in-drawer)
    (when (and file (file-readable-p file))
      (dolist (line (split-string (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string))
                                  "\n"))
        (cond
         ((string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" line)
          (setq in-drawer t))
         ((string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
          (setq in-drawer nil))
         (in-drawer
          (when (string-match "\\`[ \t]*:\\([A-Za-z_]+\\):[ \t]*\\(.*\\)\\'" line)
            (push (cons (downcase (match-string 1 line))
                        (string-trim (match-string 2 line)))
                  meta)))
         ((string-match "\\`#\\+\\([A-Za-z_]+\\):[ \t]*\\(.*\\)\\'" line)
          (push (cons (downcase (match-string 1 line))
                      (string-trim (match-string 2 line)))
                meta))
         (t (push line body))))
      (let ((text (my/jotbooks--strip-id-links
                   (string-trim (mapconcat #'identity (nreverse body) "\n")))))
        (unless (equal text "")
          (push (cons "body" text) meta))))
    meta))

(defun my/jotbooks--strip-id-links (text)
  "Replace org id: links in TEXT with their description text.
The org-node notes they point to are not part of the published site,
and an unresolvable link would abort the export."
  (replace-regexp-in-string
   "\\[\\[id:[^][]*\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
   (lambda (match) (or (match-string 1 match) ""))
   text t t))

(defun my/jotbooks--slug (name)
  "File-name-safe slug for a tag or category NAME."
  (string-trim (replace-regexp-in-string "[^[:alnum:]_-]+" "-" (downcase name))
               "-+" "-+"))

(defun my/jotbooks--rel (target from-dir)
  "Staging-relative TARGET as a link relative to FROM-DIR (\"\" = root)."
  (let ((staging (expand-file-name my/jotbooks-staging-directory)))
    (file-relative-name (file-name-concat staging target)
                        (file-name-concat staging from-dir))))

;;; Rendering

(defun my/jotbooks--pdftoppm (pdf prefix pixels)
  (with-temp-buffer
    (unless (zerop (call-process "pdftoppm" nil t nil
                                 "-jpeg" "-jpegopt" "quality=85"
                                 "-scale-to" (number-to-string pixels)
                                 (expand-file-name pdf) prefix))
      (error "pdftoppm failed on %s: %s" pdf (buffer-string)))))

(defun my/jotbooks--images (img-dir regexp)
  (sort (directory-files img-dir nil regexp) #'string-version-lessp))

(defun my/jotbooks--render-pdf (pdf img-dir)
  "Render PDF's pages and thumbnails into IMG-DIR unless up to date.
Returns (PAGES . THUMBS), file names relative to IMG-DIR in page order."
  (let* ((stem (file-name-base pdf))
         (stamp (file-name-concat img-dir (concat stem ".stamp"))))
    (when (file-newer-than-file-p pdf stamp)
      (make-directory img-dir t)
      (dolist (old (directory-files
                    img-dir nil
                    (concat "\\`" (regexp-quote stem)
                            "\\(-thumb\\)?-[0-9]+\\.jpg\\'")))
        (delete-file (file-name-concat img-dir old)))
      (my/jotbooks--pdftoppm pdf (file-name-concat img-dir stem)
                             my/jotbooks-page-pixels)
      (my/jotbooks--pdftoppm pdf (file-name-concat img-dir
                                                   (concat stem "-thumb"))
                             my/jotbooks-thumb-pixels)
      (write-region "" nil stamp nil 'silent))
    (cons (my/jotbooks--images
           img-dir (concat "\\`" (regexp-quote stem) "-[0-9]+\\.jpg\\'"))
          (my/jotbooks--images
           img-dir (concat "\\`" (regexp-quote stem) "-thumb-[0-9]+\\.jpg\\'")))))

(defun my/jotbooks--creation-time (notebook meta)
  "NOTEBOOK's creation instant as (TIME . HAS-TIME-OF-DAY).
An explicit #+DATE wins, then org-node's :TIME_CREATED:, then the
PDF's own modification time."
  (let ((string (or (cdr (assoc "date" meta))
                    (cdr (assoc "time_created" meta)))))
    (or (and string
             (condition-case nil
                 (cons (org-time-string-to-time string)
                       (and (string-match-p "[0-9]?[0-9]:[0-9][0-9]" string) t))
               (error nil)))
        (cons (file-attribute-modification-time
               (file-attributes (car (last (plist-get notebook :pdfs)))))
              t))))

(defun my/jotbooks--collect (notebook)
  "Render NOTEBOOK's images and gather its metadata.
Returns an info plist; org generation happens later, once the
notebook's neighbors in the archive's reading order are known."
  (let* ((name (plist-get notebook :name))
         (meta (my/jotbooks--meta notebook))
         (out-dir (file-name-concat
                   (expand-file-name my/jotbooks-staging-directory) name))
         (img-dir (file-name-concat out-dir "img"))
         pages thumbs)
    (dolist (pdf (plist-get notebook :pdfs))
      (let ((rendered (my/jotbooks--render-pdf pdf img-dir)))
        (setq pages (append pages (car rendered))
              thumbs (append thumbs (cdr rendered)))))
    (let ((stamp (my/jotbooks--creation-time notebook meta)))
      (list :name name
            :title (or (cdr (assoc "title" meta)) (file-name-nondirectory name))
            :time (car stamp)
            :date (format-time-string
                   (if (cdr stamp) "%Y-%m-%d %H:%M" "%Y-%m-%d")
                   (car stamp))
            :id (cdr (assoc "id" meta))
            :created (cdr (assoc "time_created" meta))
            :updated (cdr (assoc "time_modified" meta))
            :tags (split-string (or (cdr (assoc "filetags" meta)) "") ":" t)
            :category (cdr (assoc "category" meta))
            :meta meta
            :pages pages
            :thumbs thumbs
            :out-dir out-dir))))

;;; Org generation

(defun my/jotbooks--write (file content)
  "Write CONTENT to FILE only when it differs, to keep org-publish caching.
Returns FILE's base name."
  (unless (and (file-exists-p file)
               (equal content (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
    (write-region content nil file nil 'silent))
  (file-name-nondirectory file))

(defun my/jotbooks--html-name (org-file)
  (concat (file-name-sans-extension org-file) ".html"))

(defun my/jotbooks--page-content (title n i image prev next)
  "Org source for page I of N titled TITLE, showing IMAGE.
PREV and NEXT are (ORG-FILE . LABEL) navigation targets; PREV is nil
only on the archive's very first page.  NEXT always exists: within the
notebook, into the next notebook, or up to the master index."
  (concat
   (format "#+TITLE: %s — p. %d/%d\n" title i n)
   "#+OPTIONS: toc:nil num:nil\n"
   (and prev (format "#+HTML_HEAD_EXTRA: <link rel=\"prev\" href=\"%s\">\n"
                     (my/jotbooks--html-name (car prev))))
   (format "#+HTML_HEAD_EXTRA: <link rel=\"next\" href=\"%s\">\n"
           (my/jotbooks--html-name (car next)))
   "\n#+begin_pagenav\n"
   (if prev (format "[[file:%s][%s]]" (car prev) (cdr prev)) "·")
   (format " | [[file:index.org][%s]] | " title)
   (format "[[file:%s][%s]]" (car next) (cdr next))
   "\n#+end_pagenav\n"
   "\n#+begin_jotpage\n"
   ;; clicking the page advances along the same path as the next link
   (format "[[file:%s][file:%s]]\n" (car next) image)
   "#+end_jotpage\n"))

(defun my/jotbooks--meta-table (info)
  "Org table of INFO's metadata, keys and values verbatim (`=...=').
Exports via Org's default table attributes into the same bordered,
grouped-rules table used elsewhere on the site, with each cell
rendered as `<code>' — a plain key/value dump of ID, timestamps, tags,
category, and page count."
  (let* ((tags (plist-get info :tags))
         (category (plist-get info :category))
         (id (plist-get info :id))
         (created (plist-get info :created))
         (updated (plist-get info :updated))
         (rows (delq nil
                     (list
                      (and id (cons "ID" id))
                      (and created (cons "Created" created))
                      (and updated (cons "Updated" updated))
                      (and tags (cons "Tags" (mapconcat #'identity tags ", ")))
                      (and category (cons "Category" category))
                      (cons "Pages"
                            (number-to-string
                             (length (plist-get info :pages))))))))
    (concat (mapconcat (lambda (row)
                         (format "| =%s= | =%s= |" (car row) (cdr row)))
                       rows "\n")
            "\n")))

(defun my/jotbooks--notebook-index (info)
  "Org source for a notebook's index: metadata, intro, thumbnail sheet."
  (let ((meta (plist-get info :meta))
        (tags (plist-get info :tags))
        (category (plist-get info :category)))
    (concat
     (format "#+TITLE: %s\n" (plist-get info :title))
     "#+OPTIONS: toc:nil num:nil\n"
     (mapconcat (lambda (keyword)
                  (let ((value (cdr (assoc keyword meta))))
                    (if value (format "#+%s: %s\n" (upcase keyword) value) "")))
                '("filetags" "category" "description" "date")
                "")
     "\n"
     (my/jotbooks--meta-table info)
     "\n"
     (let ((body (cdr (assoc "body" meta))))
       (if body (concat body "\n\n") ""))
     (let ((parts
            (delq nil
                  (list
                   (and tags
                        (concat "tags: "
                                (mapconcat
                                 (lambda (tag)
                                   (format "[[file:%s][%s]]"
                                           (my/jotbooks--rel
                                            (format "tags/%s.org"
                                                    (my/jotbooks--slug tag))
                                            (plist-get info :name))
                                           tag))
                                 tags ", ")))
                   (and category
                        (format "in [[file:%s][%s]]"
                                (my/jotbooks--rel
                                 (format "categories/%s.org"
                                         (my/jotbooks--slug category))
                                 (plist-get info :name))
                                category))))))
       (if parts (concat (mapconcat #'identity parts " — ") "\n\n") ""))
     "#+begin_sheet\n"
     (let ((i 0))
       (mapconcat (lambda (thumb)
                    (setq i (1+ i))
                    (format "[[file:p%03d.org][file:img/%s]]" i thumb))
                  (plist-get info :thumbs)
                  "\n"))
     "\n#+end_sheet\n")))

(defun my/jotbooks--write-notebook (info prev-info next-info)
  "Write INFO's page sequence and index into the staging tree.
The last page's next link continues into NEXT-INFO's first page — or up
to the master index when INFO is the archive's newest notebook — and
the first page's prev link reaches back into PREV-INFO's last page."
  (let* ((title (plist-get info :title))
         (out-dir (plist-get info :out-dir))
         (pages (plist-get info :pages))
         (n (length pages))
         (i 0)
         written)
    (dolist (image pages)
      (setq i (1+ i))
      (let ((prev (cond ((> i 1)
                         (cons (format "p%03d.org" (1- i))
                               (format "← p. %d" (1- i))))
                        (prev-info
                         (cons (my/jotbooks--rel
                                (format "%s/p%03d.org"
                                        (plist-get prev-info :name)
                                        (length (plist-get prev-info :pages)))
                                (plist-get info :name))
                               (format "← %s" (plist-get prev-info :title))))))
            (next (cond ((< i n)
                         (cons (format "p%03d.org" (1+ i))
                               (format "p. %d →" (1+ i))))
                        (next-info
                         (cons (my/jotbooks--rel
                                (concat (plist-get next-info :name) "/p001.org")
                                (plist-get info :name))
                               (format "%s →" (plist-get next-info :title))))
                        (t (cons (my/jotbooks--rel "index.org"
                                                   (plist-get info :name))
                                 "Jotbooks →")))))
        (push (my/jotbooks--write
               (file-name-concat out-dir (format "p%03d.org" i))
               (my/jotbooks--page-content title n i (concat "img/" image)
                                          prev next))
              written)))
    (push (my/jotbooks--write (file-name-concat out-dir "index.org")
                              (my/jotbooks--notebook-index info))
          written)
    (dolist (file (directory-files out-dir nil "\\.\\(org\\|xml\\)\\'"))
      (unless (member file written)
        (delete-file (file-name-concat out-dir file))))))

(defun my/jotbooks--listing (infos from-dir)
  "Org list over INFOS, newest first.
Jotbook, tag, and category links are made relative to FROM-DIR, the
staging-relative directory of the listing page itself (\"\" = root)."
  (mapconcat
   (lambda (info)
     (format "- =%s= [[file:%s][%s]] (%d pages)%s%s\n"
             (plist-get info :date)
             (my/jotbooks--rel (concat (plist-get info :name) "/index.org")
                               from-dir)
             (plist-get info :title)
             (length (plist-get info :pages))
             (let ((tags (plist-get info :tags)))
               (if tags
                   (concat " — tags: "
                           (mapconcat
                            (lambda (tag)
                              (format "[[file:%s][%s]]"
                                      (my/jotbooks--rel
                                       (format "tags/%s.org"
                                               (my/jotbooks--slug tag))
                                       from-dir)
                                      tag))
                            tags ", "))
                 ""))
             (let ((category (plist-get info :category)))
               (if category
                   (format " — in [[file:%s][%s]]"
                           (my/jotbooks--rel
                            (format "categories/%s.org"
                                    (my/jotbooks--slug category))
                            from-dir)
                           category)
                 ""))))
   (reverse infos)
   ""))

(defun my/jotbooks--index-content (infos sections tags categories)
  "Org source for the master index over jotbook INFOS, newest first."
  (concat
   "#+TITLE: Jotbooks\n"
   "#+OPTIONS: toc:nil num:nil\n"
   "\n"
   (my/jotbooks--listing infos "")
   "\n[[file:feed.xml][RSS]]"
   (if sections
       (concat " · sections: "
               (mapconcat (lambda (section)
                            (format "[[file:%s/index.org][%s]]"
                                    (car section) (car section)))
                          (sort (copy-sequence sections)
                                (lambda (a b) (string< (car a) (car b))))
                          ", "))
     "")
   (if tags " · [[file:tags/index.org][tags]]" "")
   (if categories " · [[file:categories/index.org][categories]]" "")
   "\n"))

;;; Section pages

(defun my/jotbooks--section-index (rel title meta members)
  "Org source for the section at REL: title, intro, member listing."
  (concat
   (format "#+TITLE: %s\n" title)
   "#+OPTIONS: toc:nil num:nil\n"
   (format (concat "#+HTML_HEAD_EXTRA: <link rel=\"alternate\""
                   " type=\"application/rss+xml\" title=\"%s\" href=\"feed.xml\">\n")
           title)
   "\n"
   (let ((body (cdr (assoc "body" meta))))
     (if body (concat body "\n\n") ""))
   (my/jotbooks--listing members rel)
   "\n[[file:feed.xml][RSS]]\n"))

(defun my/jotbooks--write-section (section infos)
  "Write the index and feed of SECTION, a (REL . SOURCE-DIR) pair.
Its members are the INFOS whose path lies beneath REL."
  (let* ((rel (car section))
         (dir (file-name-concat (expand-file-name my/jotbooks-staging-directory)
                                rel))
         (meta (my/jotbooks--meta
                (list :meta-file (file-name-concat (cdr section) "meta.org"))))
         (title (or (cdr (assoc "title" meta)) (file-name-nondirectory rel)))
         (members (seq-filter
                   (lambda (info)
                     (string-prefix-p (concat rel "/") (plist-get info :name)))
                   infos)))
    (make-directory dir t)
    (let ((written
           (list (my/jotbooks--write
                  (file-name-concat dir "index.org")
                  (my/jotbooks--section-index rel title meta members))
                 (my/jotbooks--write
                  (file-name-concat dir "feed.xml")
                  (my/jotbooks--feed (format "Jotbooks — %s" title)
                                     (format "jotbooks/%s/feed.xml" rel)
                                     members)))))
      (dolist (file (directory-files dir nil "\\.\\(org\\|xml\\)\\'"))
        (unless (member file written)
          (delete-file (file-name-concat dir file)))))))

;;; Tag & category pages

(defun my/jotbooks--terms (infos key)
  "Group INFOS by KEY (:tags or :category).
Returns a slug-sorted list of (SLUG DISPLAY INFO...) with each term's
infos still in reading order."
  (let (terms)
    (dolist (info infos)
      (let ((value (plist-get info key)))
        (dolist (term (if (listp value) value (and value (list value))))
          (let ((cell (assoc (my/jotbooks--slug term) terms)))
            (if cell
                (nconc cell (list info))
              (push (list (my/jotbooks--slug term) term info) terms))))))
    (sort terms (lambda (a b) (string< (car a) (car b))))))

(defun my/jotbooks--term-page (kind dir-name term)
  "Org source for the page of TERM (a (SLUG DISPLAY INFO...) list).
KIND is \"tag\" or \"category\"; DIR-NAME the directory it lives in."
  (let ((label (format "%s: %s" kind (cadr term)))
        (feed (concat (car term) ".xml")))
    (concat
     (format "#+TITLE: %s\n" label)
     "#+OPTIONS: toc:nil num:nil\n"
     (format (concat "#+HTML_HEAD_EXTRA: <link rel=\"alternate\""
                     " type=\"application/rss+xml\" title=\"%s\" href=\"%s\">\n")
             label feed)
     "\n"
     (my/jotbooks--listing (cddr term) dir-name)
     (format "\n[[file:%s][RSS feed]]\n" feed))))

(defun my/jotbooks--terms-index (title terms)
  "Org source for the index over TERMS."
  (concat
   (format "#+TITLE: %s\n" title)
   "#+OPTIONS: toc:nil num:nil\n"
   "\n"
   (mapconcat (lambda (term)
                (format "- [[file:%s.org][%s]] (%d)\n"
                        (car term) (cadr term) (length (cddr term))))
              terms
              "")))

(defun my/jotbooks--write-terms (dir-name kind terms)
  "Write DIR-NAME/{index.org,<slug>.org,<slug>.xml} for TERMS.
Prunes stale files; removes the directory when TERMS is empty."
  (let ((dir (file-name-concat (expand-file-name my/jotbooks-staging-directory)
                               dir-name)))
    (if (null terms)
        (when (file-directory-p dir)
          (delete-directory dir t))
      (make-directory dir t)
      (let ((written
             (cons (my/jotbooks--write (file-name-concat dir "index.org")
                                       (my/jotbooks--terms-index dir-name terms))
                   (mapcan
                    (lambda (term)
                      (list
                       (my/jotbooks--write
                        (file-name-concat dir (concat (car term) ".org"))
                        (my/jotbooks--term-page kind dir-name term))
                       (my/jotbooks--write
                        (file-name-concat dir (concat (car term) ".xml"))
                        (my/jotbooks--feed
                         (format "Jotbooks — %s: %s" kind (cadr term))
                         (format "jotbooks/%s/%s.xml" dir-name (car term))
                         (cddr term)))))
                    terms))))
        (dolist (file (directory-files dir nil "\\.\\(org\\|xml\\)\\'"))
          (unless (member file written)
            (delete-file (file-name-concat dir file))))))))

;;; RSS feeds

(defun my/jotbooks--xml-escape (string)
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;")
                  (">" . "&gt;") ("\"" . "&quot;"))
                string)
    (setq string (string-replace (car pair) (cdr pair) string))))

(defun my/jotbooks--rfc822 (time)
  "RFC 822 timestamp for the Emacs TIME value."
  (let ((decoded (decode-time time)))
    (format "%s, %02d %s %d %02d:%02d:%02d %s"
            (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
                  (decoded-time-weekday decoded))
            (decoded-time-day decoded)
            (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
                  (1- (decoded-time-month decoded)))
            (decoded-time-year decoded)
            (decoded-time-hour decoded)
            (decoded-time-minute decoded)
            (decoded-time-second decoded)
            (format-time-string "%z" time))))

(defun my/jotbooks--feed-item (info)
  (let* ((base (url-encode-url (format "%s/jotbooks/%s/"
                                       my/jotbooks-site-url
                                       (plist-get info :name))))
         (link (concat base "index.html"))
         (meta (plist-get info :meta))
         (text (or (cdr (assoc "description" meta))
                   (cdr (assoc "body" meta))
                   (format "%d pages" (length (plist-get info :pages)))))
         (cover (car (plist-get info :thumbs))))
    (concat
     "<item>\n"
     (format "<title>%s</title>\n"
             (my/jotbooks--xml-escape (plist-get info :title)))
     (format "<link>%s</link>\n" link)
     ;; a persistent notebook ID keeps the guid stable across renames;
     ;; without one, fall back to the permalink
     (let ((id (plist-get info :id)))
       (if id
           (format "<guid isPermaLink=\"false\">urn:uuid:%s</guid>\n" id)
         (format "<guid>%s</guid>\n" link)))
     (format "<pubDate>%s</pubDate>\n"
             (my/jotbooks--rfc822 (plist-get info :time)))
     (mapconcat (lambda (tag)
                  (format "<category>%s</category>\n"
                          (my/jotbooks--xml-escape tag)))
                (plist-get info :tags)
                "")
     (format "<description>%s</description>\n"
             (my/jotbooks--xml-escape
              (concat (and cover (format "<img src=\"%simg/%s\"/>" base cover))
                      text)))
     "</item>\n")))

(defun my/jotbooks--feed (title path infos)
  "RSS 2.0 document titled TITLE over INFOS, newest first.
PATH is the feed's site-relative location, e.g. \"jotbooks/feed.xml\";
the channel link points at the index beside it."
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
   "<channel>\n"
   (format "<title>%s</title>\n" (my/jotbooks--xml-escape title))
   (format "<link>%s/%sindex.html</link>\n"
           my/jotbooks-site-url (file-name-directory path))
   (format "<description>%s</description>\n" (my/jotbooks--xml-escape title))
   (format "<atom:link href=\"%s/%s\" rel=\"self\" type=\"application/rss+xml\"/>\n"
           my/jotbooks-site-url path)
   (mapconcat #'my/jotbooks--feed-item (reverse infos) "")
   "</channel>\n</rss>\n"))

;;; Publishing

(defun my/jotbooks--prune-staging (notebooks sections)
  "Delete staging directories with no source counterpart.
NOTEBOOKS and SECTIONS are staging-relative paths; tags/ and
categories/ at the root are managed by `my/jotbooks--write-terms'."
  (letrec ((prune
            (lambda (dir rel)
              (dolist (name (directory-files dir nil "\\`[^.]"))
                (let ((path (file-name-concat dir name))
                      (child (if (equal rel "") name (concat rel "/" name))))
                  (when (file-directory-p path)
                    (cond
                     ((member child notebooks))
                     ((member child sections)
                      (funcall prune path child))
                     ((and (equal rel "") (member name '("tags" "categories"))))
                     (t (delete-directory path t)))))))))
    (funcall prune (expand-file-name my/jotbooks-staging-directory) "")))

(defun my/jotbooks-prepare (&rest _)
  "Scan `my/jotbooks-directory' and refresh the staging tree.
Runs as the jotbooks project's :preparation-function, so new PDFs and
sub-directories are picked up by a plain `org-publish-all'."
  (let* ((staging (expand-file-name my/jotbooks-staging-directory))
         (scanned (my/jotbooks--scan (expand-file-name my/jotbooks-directory)
                                     ""))
         (sections (cdr scanned))
         (infos (sort (mapcar #'my/jotbooks--collect (car scanned))
                      (lambda (a b)
                        (or (time-less-p (plist-get a :time)
                                         (plist-get b :time))
                            (and (time-equal-p (plist-get a :time)
                                               (plist-get b :time))
                                 (string-version-lessp (plist-get a :name)
                                                       (plist-get b :name)))))))
         (tags (my/jotbooks--terms infos :tags))
         (categories (my/jotbooks--terms infos :category)))
    (make-directory staging t)
    (let ((prev nil)
          (rest infos))
      (while rest
        (my/jotbooks--write-notebook (car rest) prev (cadr rest))
        (setq prev (car rest)
              rest (cdr rest))))
    (dolist (section sections)
      (my/jotbooks--write-section section infos))
    (my/jotbooks--write-terms "tags" "tag" tags)
    (my/jotbooks--write-terms "categories" "category" categories)
    (my/jotbooks--write (file-name-concat staging "index.org")
                        (my/jotbooks--index-content infos sections tags
                                                    categories))
    (my/jotbooks--write (file-name-concat staging "feed.xml")
                        (my/jotbooks--feed "Jotbooks" "jotbooks/feed.xml" infos))
    (my/jotbooks--prune-staging (mapcar (lambda (info) (plist-get info :name))
                                        infos)
                                (mapcar #'car sections))))

(defun my/jotbooks-breadcrumb (info)
  "Home / jotbooks / … bar in the style of `my/org-blog-section-bar'.
Directories become crumbs, and so does the file itself (as \"p. N\" for
notebook pages) — except index files, which their directory covers."
  (let* ((rel (file-relative-name
               (plist-get info :input-file)
               (expand-file-name my/jotbooks-staging-directory)))
         (parts (split-string rel "/"))
         (base (file-name-base (car (last parts))))
         (path "/jotbooks")
         (crumbs
          (append (list (cons "Home" "/index.html")
                        (cons "jotbooks" "/jotbooks/index.html"))
                  (mapcar (lambda (part)
                            (setq path (concat path "/" part))
                            (cons part (concat path "/index.html")))
                          (butlast parts))
                  (unless (equal base "index")
                    (list (cons (if (string-match "\\`p0*\\([0-9]+\\)\\'" base)
                                    (concat "p. " (match-string 1 base))
                                  base)
                                (concat path "/" base ".html")))))))
    (concat
     "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\""
     " rules=\"groups\" frame=\"hsides\"><tbody><tr>"
     (mapconcat (lambda (crumb)
                  (format "<td class=\"org-left\"><a href=\"%s\"><code>%s</code></a></td>"
                          (cdr crumb) (car crumb)))
                crumbs "")
     "</tr></tbody></table>")))

(defun my/jotbooks-publish (&optional force)
  "Publish the jotbooks project; with prefix FORCE, republish everything."
  (interactive "P")
  (org-publish "jotbooks" force))

(provide 'jotbook)
