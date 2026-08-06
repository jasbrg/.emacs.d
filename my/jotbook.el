;;;; Jotbooks Publishing

;; Notebook scans (multi-page PDFs from the iPhone) and standalone
;; photos (HEIC, also from the iPhone) live under
;; `my/jotbooks-directory'.  Every PDF or HEIC file is one jotbook,
;; published at its directory path — analog/mug.pdf appears under
;; /jotbooks/analog/mug/, and a HEIC photo the same way, as a
;; single-page jotbook.  Directories are sections: each gets an index
;; page listing the jotbooks beneath it, its own RSS feed, and a crumb
;; in the breadcrumb bar.  Publishing renders each page and a
;; thumbnail to JPEG — PDF pages via pdftoppm, HEIC photos via sips —
;; and generates one org file per page with prev/next navigation, a
;; thumbnail sheet per jotbook, and a master index — all under
;; `my/jotbooks-staging-directory', which org-publish then exports.
;; The render/generate step runs as the project's
;; :preparation-function, so dropping a new PDF, HEIC, or
;; sub-directory into the Jotbooks directory is picked up by a plain
;; `org-publish-all'.
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
;; index the same way.  Tags and categories span both ontologies and
;; publish at the site root — /tags/ and /categories/ — as their own
;; ways of random access, with RSS feeds per term alongside the whole
;; archive's feed (feed.xml) and the per-section feeds.
;;
;; Nodes are a sibling ontology, published under /nodes/: org-node
;; entries — files or subtrees with an :ID: — carrying
;; `my/nodes-publish-tag', discovered anywhere under
;; `my/nodes-source-directories' (by default `org-mem-watch-dirs', the
;; same universe org-node sees).  The two ontologies overlap: a jotbook
;; whose metadata has an ID is also a node, and its page is the jotbook
;; page.  While publishing, every published ID is held in a registry so
;; that id: links in any body resolve to the target's page (unpublished
;; targets collapse to their description), and each page's metadata
;; table gains a "Backlinks" row — so nodes and jotbooks
;; link to and from each other.

(require 'url-util)

(defvar my/jotbooks-directory "~/Documents/Jotbooks")  ;; write
(defvar my/jotbooks-staging-directory                  ;; generated
  (file-name-concat my/jotbooks-directory ".staging"))
(defvar my/jotbooks-html-directory                     ;; read
  (file-name-concat my/html-blog-directory "jotbooks"))

(defvar my/jotbooks-site-url "https://jasbrg.com"
  "Absolute site root, used for the RSS feeds' links.")

(defvar my/nodes-staging-directory
  (file-name-concat my/jotbooks-directory ".staging-nodes")
  "Generated org sources for the published nodes.")

(defvar my/terms-staging-directory
  (file-name-concat my/jotbooks-directory ".staging-terms")
  "Generated org sources for the tag and category trees.
They publish at the site root (/tags/, /categories/) because terms
span both the jotbooks and nodes ontologies.")
(defvar my/nodes-html-directory
  (file-name-concat my/html-blog-directory "nodes")) ;; read

(defvar my/nodes-publish-tag "publish"
  "Tag that marks an org-node entry for publishing.
A file-level node needs it among its #+filetags, a subtree node on its
own heading; either also needs an :ID:.")

(defvar my/nodes-source-directories nil
  "Directories scanned recursively for publishable nodes.
When nil, `org-mem-watch-dirs' is used, so candidate nodes may live
anywhere org-node can see them.  Paths containing a substring from
`org-mem-exclude' are skipped.")

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
root).  Every PDF or HEIC file is one jotbook named by its path sans
extension; every directory with one somewhere beneath it is a
section.  Returns (NOTEBOOKS . SECTIONS), where SECTIONS are (REL
. DIR) pairs excluding the root itself."
  (let (notebooks sections)
    (dolist (name (directory-files dir))
      (unless (string-prefix-p "." name)
        (let ((path (file-name-concat dir name))
              (child (if (equal rel "") name (concat rel "/" name)))
              (case-fold-search t))
          (cond
           ((string-match-p "\\.\\(pdf\\|heic\\)\\'" name)
            (push (list :name (file-name-sans-extension child)
                        :sources (list path)
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

(defun my/jotbooks--parse-org (text)
  "Parse the leading metadata of org TEXT.
A leading `:PROPERTIES:'/`:END:' drawer — as org-node maintains, with
:ID:, :TIME_CREATED:, and :TIME_MODIFIED: — is read the same as the
`#+KEYWORD:' lines that may follow it, so a file captured via
`org-node' needs no extra keywords of its own.  Keywords are
downcased; everything after the metadata block is kept untouched
under \"body\"."
  (let ((lines (split-string text "\n"))
        meta body in-drawer done)
    (dolist (line lines)
      (cond
       (done (push line body))
       ((and (not in-drawer)
             (string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" line))
        (setq in-drawer t))
       ((and in-drawer (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line))
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
       ((string-match-p "\\`[ \t]*\\'" line))
       (t (setq done t)
          (push line body))))
    (let ((body-text (string-trim (mapconcat #'identity (nreverse body) "\n"))))
      (unless (equal body-text "")
        (push (cons "body" body-text) meta)))
    meta))

(defun my/jotbooks--meta (notebook)
  "Read NOTEBOOK's metadata file into an alist of (KEYWORD . VALUE)."
  (let ((file (plist-get notebook :meta-file)))
    (when (and file (file-readable-p file))
      (my/jotbooks--parse-org (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))))

;;; The published-ID registry & backlinks

(defvar my/jotbooks--registry nil
  "Bound while publishing: hash of org ID → (:url :title) of its page.
Covers both ontologies — jotbooks whose metadata has an ID, and nodes.")

(defvar my/jotbooks--backlinks nil
  "Bound while publishing: hash of org ID → IDs of pages linking to it.")

(defun my/jotbooks--html-link (url text)
  "Root-relative URL link as an inline HTML snippet, safe inside tables."
  (format "@@html:<a href=\"%s\">%s</a>@@"
          url (string-replace "|" "/" (my/jotbooks--xml-escape text))))

(defun my/jotbooks--resolve-id-links (text)
  "Rewrite org id: links in TEXT against `my/jotbooks--registry'.
Links to published pages become root-relative links titled by their
description (or the target's title); the rest collapse to their
description text, since their targets are not part of the site."
  (replace-regexp-in-string
   "\\[\\[id:\\([^][]*\\)\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
   (lambda (match)
     (let ((target (and my/jotbooks--registry
                        (gethash (match-string 1 match)
                                 my/jotbooks--registry)))
           (description (match-string 2 match)))
       (if target
           (my/jotbooks--html-link (plist-get target :url)
                                   (or description (plist-get target :title)))
         (or description ""))))
   text t t))

(defun my/jotbooks--strip-id-links (text)
  "Replace org id: links in TEXT with their bare description text.
For contexts like RSS descriptions where no link markup can render."
  (replace-regexp-in-string
   "\\[\\[id:[^][]*\\]\\(?:\\[\\([^][]*\\)\\]\\)?\\]"
   (lambda (match) (or (match-string 1 match) ""))
   text t t))

(defun my/jotbooks--backlink-map (sources)
  "Hash of target ID → source IDs, over SOURCES of (ID . RAW-TEXT).
Only links between published IDs (per `my/jotbooks--registry') count."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (source sources)
      (let ((start 0))
        (while (string-match "\\[\\[id:\\([^][]*\\)\\]" (cdr source) start)
          (setq start (match-end 0))
          (let ((target (match-string 1 (cdr source))))
            (when (and (gethash target my/jotbooks--registry)
                       (car source)
                       (not (equal target (car source)))
                       (not (member (car source) (gethash target map))))
              (push (car source) (gethash target map)))))))
    map))

(defun my/jotbooks--stream-nav (entry)
  "Prev/next navigation through the node stream around ENTRY.
The stream — every entry with an ID, pure nodes and jotbook metadata
pages alike, oldest first — is annotated onto entries by
`my/jotbooks-prepare' as (URL . TITLE) neighbor conses.  Returns
keyword lines plus a pagenav block; the newest entry exits up to the
nodes index."
  (let ((prev (plist-get entry :stream-prev))
        (next (plist-get entry :stream-next)))
    (concat
     (and prev (format "#+HTML_HEAD_EXTRA: <link rel=\"prev\" href=\"%s\">\n"
                       (car prev)))
     (format "#+HTML_HEAD_EXTRA: <link rel=\"next\" href=\"%s\">\n"
             (if next (car next) "/nodes/index.html"))
     "\n#+begin_pagenav\n"
     (if prev
         (my/jotbooks--html-link (car prev) (concat "← " (cdr prev)))
       "·")
     " | "
     (my/jotbooks--html-link "/nodes/index.html" "nodes")
     " | "
     (if next
         (my/jotbooks--html-link (car next) (concat (cdr next) " →"))
       (my/jotbooks--html-link "/nodes/index.html" "nodes →"))
     "\n#+end_pagenav\n")))

(defun my/jotbooks--backlinks-cell (id)
  "Table-cell markup linking the pages that link to ID, or nil."
  (let ((sources (and id my/jotbooks--backlinks
                      (gethash id my/jotbooks--backlinks))))
    (when sources
      (mapconcat
       (lambda (target)
         (my/jotbooks--html-link (plist-get target :url)
                                 (plist-get target :title)))
       (sort (delq nil (mapcar (lambda (source)
                                 (gethash source my/jotbooks--registry))
                               sources))
             (lambda (a b)
               (string< (plist-get a :title) (plist-get b :title))))
       ", "))))

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

(defun my/jotbooks--sips-heic (heic out pixels)
  (unless (zerop (call-process "sips" nil nil nil
                               "-s" "format" "jpeg"
                               "-s" "formatOptions" "85"
                               "-Z" (number-to-string pixels)
                               (expand-file-name heic)
                               "--out" out))
    (error "sips failed on %s" heic)))

(defun my/jotbooks--render-heic (heic img-dir)
  "Render HEIC as a single page and thumbnail into IMG-DIR unless up
to date.  Returns (PAGES . THUMBS), mirroring
`my/jotbooks--render-pdf''s single-page case, so downstream code
doesn't care whether a jotbook came from a PDF or a photo."
  (let* ((stem (file-name-base heic))
         (stamp (file-name-concat img-dir (concat stem ".stamp"))))
    (when (file-newer-than-file-p heic stamp)
      (make-directory img-dir t)
      (dolist (old (directory-files
                    img-dir nil
                    (concat "\\`" (regexp-quote stem)
                            "\\(-thumb\\)?-[0-9]+\\.jpg\\'")))
        (delete-file (file-name-concat img-dir old)))
      (my/jotbooks--sips-heic heic (file-name-concat img-dir (concat stem "-1.jpg"))
                              my/jotbooks-page-pixels)
      (my/jotbooks--sips-heic heic (file-name-concat img-dir (concat stem "-thumb-1.jpg"))
                              my/jotbooks-thumb-pixels)
      (write-region "" nil stamp nil 'silent))
    (cons (my/jotbooks--images
           img-dir (concat "\\`" (regexp-quote stem) "-[0-9]+\\.jpg\\'"))
          (my/jotbooks--images
           img-dir (concat "\\`" (regexp-quote stem) "-thumb-[0-9]+\\.jpg\\'")))))

(defun my/jotbooks--render-source (source img-dir)
  "Render SOURCE (a PDF or HEIC file) into IMG-DIR.
Dispatches on SOURCE's extension; returns (PAGES . THUMBS) as
`my/jotbooks--render-pdf' and `my/jotbooks--render-heic' do."
  (if (let ((case-fold-search t)) (string-match-p "\\.pdf\\'" source))
      (my/jotbooks--render-pdf source img-dir)
    (my/jotbooks--render-heic source img-dir)))

(defun my/jotbooks--creation-time (notebook meta)
  "NOTEBOOK's creation instant as (TIME . HAS-TIME-OF-DAY).
An explicit #+DATE wins, then org-node's :TIME_CREATED:, then the
source file's own modification time."
  (let ((string (or (cdr (assoc "date" meta))
                    (cdr (assoc "time_created" meta)))))
    (or (and string
             (condition-case nil
                 (cons (org-time-string-to-time string)
                       (and (string-match-p "[0-9]?[0-9]:[0-9][0-9]" string) t))
               (error nil)))
        (cons (file-attribute-modification-time
               (file-attributes (car (last (plist-get notebook :sources)))))
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
    (dolist (source (plist-get notebook :sources))
      (let ((rendered (my/jotbooks--render-source source img-dir)))
        (setq pages (append pages (car rendered))
              thumbs (append thumbs (cdr rendered)))))
    (let ((stamp (my/jotbooks--creation-time notebook meta)))
      (list :kind 'jotbook
            :name name
            :url (format "/jotbooks/%s/index.html" name)
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
            :refs (cdr (assoc "roam_refs" meta))
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

(defun my/jotbooks--org-table (rows)
  "Render ROWS of (KEY . ORG-MARKUP) as the site's metadata table.
Keys render as `<code>' via `=...='; values are org markup as given.
Exports via Org's default table attributes into the same bordered,
grouped-rules table used elsewhere on the site."
  (concat (mapconcat (lambda (row)
                       (format "| =%s= | %s |" (car row) (cdr row)))
                     rows "\n")
          "\n"))

(defun my/jotbooks--meta-table (info)
  "Org table of INFO's metadata — ID, timestamps, tags, category,
external links (ROAM_REFS), page count, and a \"Backlinks\" row when
other published pages link to INFO's ID."
  (let* ((tags (plist-get info :tags))
         (category (plist-get info :category))
         (refs (plist-get info :refs))
         (id (plist-get info :id))
         (created (plist-get info :created))
         (updated (plist-get info :updated))
         (backlinks (my/jotbooks--backlinks-cell id)))
    (my/jotbooks--org-table
     (delq nil
           (list
            (and id (cons "ID" (format "=%s=" id)))
            (and created (cons "Created" (format "=%s=" created)))
            (and updated (cons "Updated" (format "=%s=" updated)))
            (and tags (cons "Tags" (my/jotbooks--tag-links tags)))
            (and category (cons "Category"
                                (my/jotbooks--category-link category)))
            (and refs (cons "Links" (my/jotbooks--refs-links refs)))
            (cons "Pages" (format "=%d=" (length (plist-get info :pages))))
            (and backlinks (cons "Backlinks" backlinks)))))))

(defun my/jotbooks--notebook-index (info)
  "Org source for a notebook's index: metadata, intro, thumbnail sheet.
When the notebook has an ID it is also a node, so its metadata page is
a stop in the node stream and carries the stream's navigation."
  (let ((meta (plist-get info :meta)))
    (concat
     (format "#+TITLE: %s\n" (plist-get info :title))
     "#+OPTIONS: toc:nil num:nil\n"
     (mapconcat (lambda (keyword)
                  (let ((value (cdr (assoc keyword meta))))
                    (if value (format "#+%s: %s\n" (upcase keyword) value) "")))
                '("filetags" "category" "description" "date")
                "")
     (if (plist-get info :id) (my/jotbooks--stream-nav info) "")
     "\n"
     (my/jotbooks--meta-table info)
     "\n"
     (let ((body (cdr (assoc "body" meta))))
       (if body (concat (my/jotbooks--resolve-id-links body) "\n\n") ""))
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
                                 "jotbooks →")))))
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

(defun my/jotbooks--entry-newer-p (a b)
  "Order entries A and B newest first, by creation time then title."
  (let ((ta (or (plist-get a :time) 0))
        (tb (or (plist-get b :time) 0)))
    (or (time-less-p tb ta)
        (and (time-equal-p ta tb)
             (string< (plist-get a :title) (plist-get b :title))))))

(defun my/jotbooks--tag-links (tags)
  "TAGS as comma-separated links to their tag pages."
  (mapconcat (lambda (tag)
               (my/jotbooks--html-link
                (format "/tags/%s.html" (my/jotbooks--slug tag))
                tag))
             tags ", "))

(defun my/jotbooks--category-link (category)
  (my/jotbooks--html-link
   (format "/categories/%s.html" (my/jotbooks--slug category))
   category))

(defun my/jotbooks--ref-link (ref)
  "Render one ROAM_REFS token REF as a table-cell link, or as literal
text when it isn't a URL — e.g. a citation key such as \"@doe2020\".
Recognizes org's bracketed link syntax (\"[[URL]]\" or
\"[[URL][DESC]]\") as well as bare URLs; a surrounding pair of double
quotes, per org-roam's convention for refs with spaces, is stripped
first."
  (let ((ref (if (string-match "\\`\"\\(.*\\)\"\\'" ref) (match-string 1 ref) ref)))
    (cond
     ((string-match "\\`\\[\\[\\([^][]+\\)\\]\\(?:\\[\\([^][]+\\)\\]\\)?\\]\\'" ref)
      (my/jotbooks--html-link (match-string 1 ref)
                              (or (match-string 2 ref) (match-string 1 ref))))
     ((string-match-p "\\`[a-z]+://" ref)
      (my/jotbooks--html-link ref ref))
     (t (format "=%s=" ref)))))

(defun my/jotbooks--refs-tokens (refs-string)
  "Tokenize a ROAM_REFS value on whitespace, except within a
double-quoted string or a bracketed org link (\"[[URL]]\" or
\"[[URL][DESC]]\"), which stay whole even when they contain spaces."
  (let ((re "\\[\\[[^][]*\\]\\(?:\\[[^][]*\\]\\)?\\]\\|\"[^\"]*\"\\|[^ \t\n]+")
        (start 0) tokens)
    (while (string-match re refs-string start)
      (push (match-string 0 refs-string) tokens)
      (setq start (match-end 0)))
    (nreverse tokens)))

(defun my/jotbooks--refs-links (refs-string)
  "Table-cell markup for REFS-STRING, a ROAM_REFS value."
  (mapconcat #'my/jotbooks--ref-link
             (my/jotbooks--refs-tokens refs-string)
             ", "))

(defun my/jotbooks--listing (entries)
  "Org list over ENTRIES — jotbooks and nodes alike — newest first.
All links are root-relative, so a listing renders identically from any
page on the site."
  (mapconcat
   (lambda (entry)
     (format "- %s%s%s%s%s\n"
             (let ((date (plist-get entry :date)))
               (if (and date (not (equal date "")))
                   (format "=%s= " date)
                 ""))
             (my/jotbooks--html-link (plist-get entry :url)
                                     (plist-get entry :title))
             (if (eq (plist-get entry :kind) 'jotbook)
                 (format " (%d pages)" (length (plist-get entry :pages)))
               "")
             (let ((tags (plist-get entry :tags)))
               (if tags
                   (concat " — tags: " (my/jotbooks--tag-links tags))
                 ""))
             (let ((category (plist-get entry :category)))
               (if category
                   (format " — in %s" (my/jotbooks--category-link category))
                 ""))))
   (sort (copy-sequence entries) #'my/jotbooks--entry-newer-p)
   ""))

(defun my/jotbooks--index-content (infos sections)
  "Org source for the master index over jotbook INFOS, newest first."
  (concat
   "#+TITLE: Jotbooks\n"
   "#+OPTIONS: toc:nil num:nil\n"
   "\n"
   (my/jotbooks--listing infos)
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
     (if body (concat (my/jotbooks--resolve-id-links body) "\n\n") ""))
   (my/jotbooks--listing members)
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

(defun my/jotbooks--term-page (kind term)
  "Org source for the page of TERM (a (SLUG DISPLAY ENTRY...) list).
KIND is \"tag\" or \"category\"."
  (let ((label (format "%s: %s" kind (cadr term)))
        (feed (concat (car term) ".xml")))
    (concat
     (format "#+TITLE: %s\n" label)
     "#+OPTIONS: toc:nil num:nil\n"
     (format (concat "#+HTML_HEAD_EXTRA: <link rel=\"alternate\""
                     " type=\"application/rss+xml\" title=\"%s\" href=\"%s\">\n")
             label feed)
     "\n"
     (my/jotbooks--listing (cddr term))
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
Lives in `my/terms-staging-directory', publishing at the site root.
Prunes stale files; removes the directory when TERMS is empty."
  (let ((dir (file-name-concat (expand-file-name my/terms-staging-directory)
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
                        (my/jotbooks--term-page kind term))
                       (my/jotbooks--write
                        (file-name-concat dir (concat (car term) ".xml"))
                        (my/jotbooks--feed
                         (format "Jotbooks — %s: %s" kind (cadr term))
                         (format "%s/%s.xml" dir-name (car term))
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

(defun my/jotbooks--feed-item (entry)
  (if (eq (plist-get entry :kind) 'node)
      (my/jotbooks--node-feed-item entry)
    (my/jotbooks--jotbook-feed-item entry)))

(defun my/jotbooks--node-feed-item (node)
  (let ((link (concat my/jotbooks-site-url (plist-get node :url))))
    (concat
     "<item>\n"
     (format "<title>%s</title>\n"
             (my/jotbooks--xml-escape (plist-get node :title)))
     (format "<link>%s</link>\n" link)
     (format "<guid isPermaLink=\"false\">urn:uuid:%s</guid>\n"
             (plist-get node :id))
     (unless (equal (plist-get node :time) 0)
       (format "<pubDate>%s</pubDate>\n"
               (my/jotbooks--rfc822 (plist-get node :time))))
     (mapconcat (lambda (tag)
                  (format "<category>%s</category>\n"
                          (my/jotbooks--xml-escape tag)))
                (plist-get node :tags)
                "")
     (format "<description>%s</description>\n"
             (my/jotbooks--xml-escape
              (my/jotbooks--strip-id-links (plist-get node :body))))
     "</item>\n")))

(defun my/jotbooks--jotbook-feed-item (info)
  (let* ((base (url-encode-url (format "%s/jotbooks/%s/"
                                       my/jotbooks-site-url
                                       (plist-get info :name))))
         (link (concat base "index.html"))
         (meta (plist-get info :meta))
         (text (my/jotbooks--strip-id-links
                (or (cdr (assoc "description" meta))
                    (cdr (assoc "body" meta))
                    (format "%d pages" (length (plist-get info :pages))))))
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

(defun my/jotbooks--feed (title path entries)
  "RSS 2.0 document titled TITLE over ENTRIES, newest first.
ENTRIES may mix jotbooks and nodes.  PATH is the feed's site-relative
location, e.g. \"jotbooks/feed.xml\"; the channel link points at the
index beside it."
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
   (mapconcat #'my/jotbooks--feed-item
              (sort (copy-sequence entries) #'my/jotbooks--entry-newer-p)
              "")
   "</channel>\n</rss>\n"))

;;; Nodes

(defun my/nodes--source-files ()
  "All org files that may hold publishable nodes.
Scans `my/nodes-source-directories' (or `org-mem-watch-dirs')
recursively, skipping paths matching `org-mem-exclude' substrings."
  (let ((dirs (or my/nodes-source-directories
                  (and (boundp 'org-mem-watch-dirs) org-mem-watch-dirs)))
        (exclude (and (boundp 'org-mem-exclude) org-mem-exclude))
        files)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files-recursively
                       (expand-file-name dir) "\\.org\\'"))
          (unless (seq-some (lambda (pattern) (string-search pattern file))
                            exclude)
            (push file files)))))
    (nreverse files)))

(defun my/nodes--in-file (file)
  "Publishable nodes in FILE, as plists.
A file-level node needs an :ID: in its leading drawer and
`my/nodes-publish-tag' among its #+filetags; a subtree node needs the
tag on its own heading and an :ID: in the drawer below it."
  (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
        (tag-re (concat ":" (regexp-quote my/nodes-publish-tag) ":"))
        nodes)
    (let* ((meta (my/jotbooks--parse-org text))
           (id (cdr (assoc "id" meta)))
           (tags (cdr (assoc "filetags" meta))))
      (when (and id tags (string-match-p tag-re tags))
        (push (list :id id
                    :title (or (cdr (assoc "title" meta))
                               (file-name-base file))
                    :tags (split-string tags ":" t)
                    :category (cdr (assoc "category" meta))
                    :refs (cdr (assoc "roam_refs" meta))
                    :created (or (cdr (assoc "time_created" meta))
                                 (cdr (assoc "created" meta))
                                 (cdr (assoc "date" meta)))
                    :updated (cdr (assoc "time_modified" meta))
                    :body (or (cdr (assoc "body" meta)) ""))
              nodes)))
    (let ((start 0)
          (heading-re (concat "^\\(\\*+\\) +\\(.*?\\)[ \t]+"
                              "\\(:[[:alnum:]_@#%:]*"
                              (regexp-quote my/nodes-publish-tag)
                              "[[:alnum:]_@#%:]*:\\)[ \t]*$")))
      (while (string-match heading-re text start)
        ;; capture everything from this match before `string-match' and
        ;; `split-string' below clobber the match data
        (let* ((level (length (match-string 1 text)))
               (title (match-string 2 text))
               (tag-string (match-string 3 text))
               (begin (match-end 0))
               (end (or (and (string-match
                              (format "^\\*\\{1,%d\\} " level) text begin)
                             (match-beginning 0))
                        (length text)))
               (meta (my/jotbooks--parse-org (substring text begin end)))
               (id (cdr (assoc "id" meta))))
          (setq start begin)
          (when id
            (push (list :id id
                        :title title
                        :tags (split-string tag-string ":" t)
                        :category (cdr (assoc "category" meta))
                        :refs (cdr (assoc "roam_refs" meta))
                        :created (or (cdr (assoc "time_created" meta))
                                     (cdr (assoc "created" meta)))
                        :updated (cdr (assoc "time_modified" meta))
                        :body (or (cdr (assoc "body" meta)) ""))
                  nodes)))))
    (nreverse nodes)))

(defun my/nodes--stamp (created)
  "Parse the org timestamp string CREATED as (TIME . HAS-TIME-OF-DAY)."
  (and created
       (condition-case nil
           (cons (org-time-string-to-time created)
                 (and (string-match-p "[0-9]?[0-9]:[0-9][0-9]" created) t))
         (error nil))))

(defun my/nodes--collect ()
  "Discover publishable nodes, as entry plists like the jotbooks'.
Duplicate IDs keep their first appearance; a slug collision is
disambiguated with a prefix of the node's ID."
  (let (nodes ids slugs)
    (dolist (file (my/nodes--source-files))
      (dolist (node (my/nodes--in-file file))
        (unless (member (plist-get node :id) ids)
          (push (plist-get node :id) ids)
          (let ((slug (my/jotbooks--slug (plist-get node :title)))
                (stamp (my/nodes--stamp (plist-get node :created))))
            (when (or (equal slug "") (member slug slugs))
              (setq slug (concat (if (equal slug "") "node" slug) "-"
                                 (substring (plist-get node :id) 0 8))))
            (push slug slugs)
            (push (append (list :kind 'node
                                :slug slug
                                :url (format "/nodes/%s.html" slug)
                                :time (or (car stamp) 0)
                                :date (if stamp
                                          (format-time-string
                                           (if (cdr stamp) "%Y-%m-%d %H:%M"
                                             "%Y-%m-%d")
                                           (car stamp))
                                        "")
                                :tags (remove my/nodes-publish-tag
                                              (plist-get node :tags)))
                          node)
                  nodes)))))
    (nreverse nodes)))

(defun my/nodes--meta-table (node)
  "Org table of NODE's metadata, including its external links
(ROAM_REFS) and its backlinks."
  (let ((created (plist-get node :created))
        (updated (plist-get node :updated))
        (tags (plist-get node :tags))
        (category (plist-get node :category))
        (refs (plist-get node :refs))
        (backlinks (my/jotbooks--backlinks-cell (plist-get node :id))))
    (my/jotbooks--org-table
     (delq nil
           (list
            (cons "ID" (format "=%s=" (plist-get node :id)))
            (and created (cons "Created" (format "=%s=" created)))
            (and updated (cons "Updated" (format "=%s=" updated)))
            (and tags (cons "Tags" (my/jotbooks--tag-links tags)))
            (and category (cons "Category"
                                (my/jotbooks--category-link category)))
            (and refs (cons "Links" (my/jotbooks--refs-links refs)))
            (and backlinks (cons "Backlinks" backlinks)))))))

(defun my/nodes--page (node)
  "Org source for NODE's page, a stop in the node stream."
  (concat
   (format "#+TITLE: %s\n" (plist-get node :title))
   "#+OPTIONS: toc:nil num:nil\n"
   (my/jotbooks--stream-nav node)
   "\n"
   (my/nodes--meta-table node)
   "\n"
   (let ((body (plist-get node :body)))
     (if (equal body "") ""
       (concat (my/jotbooks--resolve-id-links body) "\n")))))

(defun my/nodes--index (entries)
  "Org source for the nodes index over ENTRIES, newest first.
ENTRIES spans both ontologies: every node, plus every jotbook whose
metadata carries an ID — those are nodes too."
  (concat
   "#+TITLE: Nodes\n"
   "#+OPTIONS: toc:nil num:nil\n"
   (concat "#+HTML_HEAD_EXTRA: <link rel=\"alternate\""
           " type=\"application/rss+xml\" title=\"Nodes\" href=\"feed.xml\">\n")
   "\n"
   (if (null entries)
       "No nodes published yet.\n"
     (my/jotbooks--listing entries))
   "\n[[file:feed.xml][RSS]]\n"))

(defun my/nodes--write (nodes entries)
  "Write the nodes staging tree: a page per node in NODES, plus the
index over ENTRIES (which also includes the jotbooks with IDs)."
  (let ((dir (expand-file-name my/nodes-staging-directory))
        written)
    (make-directory dir t)
    (dolist (node nodes)
      (push (my/jotbooks--write
             (file-name-concat dir (concat (plist-get node :slug) ".org"))
             (my/nodes--page node))
            written))
    (push (my/jotbooks--write (file-name-concat dir "index.org")
                              (my/nodes--index entries))
          written)
    (push (my/jotbooks--write (file-name-concat dir "feed.xml")
                              (my/jotbooks--feed "Nodes" "nodes/feed.xml"
                                                 entries))
          written)
    (dolist (file (directory-files dir nil "\\.\\(org\\|xml\\)\\'"))
      (unless (member file written)
        (delete-file (file-name-concat dir file))))))

;;; Publishing

(defun my/jotbooks--prune-staging (notebooks sections)
  "Delete staging directories with no source counterpart.
NOTEBOOKS and SECTIONS are staging-relative paths."
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
         ;; a jotbook with an ID is already a node; its page is the
         ;; jotbook page, so no separate node page is generated
         (nodes (let ((jotbook-ids (delq nil (mapcar (lambda (info)
                                                       (plist-get info :id))
                                                     infos))))
                  (seq-remove (lambda (node)
                                (member (plist-get node :id) jotbook-ids))
                              (my/nodes--collect))))
         ;; tags and categories span both ontologies
         (entries (append infos nodes))
         (tags (my/jotbooks--terms entries :tags))
         (categories (my/jotbooks--terms entries :category))
         (my/jotbooks--registry
          (let ((registry (make-hash-table :test #'equal)))
            (dolist (entry entries)
              (when (plist-get entry :id)
                (puthash (plist-get entry :id)
                         (list :url (plist-get entry :url)
                               :title (plist-get entry :title))
                         registry)))
            registry))
         (my/jotbooks--backlinks
          (my/jotbooks--backlink-map
           (append
            (mapcar (lambda (info)
                      (cons (plist-get info :id)
                            (or (cdr (assoc "body" (plist-get info :meta)))
                                "")))
                    infos)
            (mapcar (lambda (node)
                      (cons (plist-get node :id) (plist-get node :body)))
                    nodes)))))
    (make-directory staging t)
    (make-directory (expand-file-name my/terms-staging-directory) t)
    ;; thread the node stream — every entry with an ID, pure nodes and
    ;; jotbook metadata pages alike — through time, oldest first
    (let ((stream (sort (seq-filter (lambda (entry) (plist-get entry :id))
                                    (copy-sequence entries))
                        (lambda (a b) (my/jotbooks--entry-newer-p b a))))
          (prev nil))
      (dolist (entry stream)
        (when prev
          (plist-put prev :stream-next (cons (plist-get entry :url)
                                             (plist-get entry :title)))
          (plist-put entry :stream-prev (cons (plist-get prev :url)
                                              (plist-get prev :title))))
        (setq prev entry)))
    ;; the nodes index also lists the jotbooks that are nodes
    (my/nodes--write nodes
                     (append nodes
                             (seq-filter (lambda (info) (plist-get info :id))
                                         infos)))
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
                        (my/jotbooks--index-content infos sections
						    ;; tags categories
						    ))
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
          (append (list (cons "home" "/index.html")
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
    (my/jotbooks--crumb-bar crumbs)))

(defun my/jotbooks--crumb-bar (crumbs)
  "Render CRUMBS of (LABEL . HREF) as the site's breadcrumb table."
  (concat
   "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\""
   " rules=\"groups\" frame=\"hsides\"><tbody><tr>"
   (mapconcat (lambda (crumb)
                (format "<td class=\"org-left\"><a href=\"%s\"><code>%s</code></a></td>"
                        (cdr crumb) (car crumb)))
              crumbs "")
   "</tr></tbody></table>"))

(defun my/nodes-breadcrumb (info)
  "Home / nodes / <page> bar for the nodes ontology."
  (let ((base (file-name-base (plist-get info :input-file))))
    (my/jotbooks--crumb-bar
     (append (list (cons "home" "/index.html")
                   (cons "nodes" "/nodes/index.html"))
             (unless (equal base "index")
               (list (cons base (format "/nodes/%s.html" base))))))))

(defun my/terms-breadcrumb (info)
  "Home / tags / <term> bar (likewise for categories)."
  (let* ((rel (file-relative-name
               (plist-get info :input-file)
               (expand-file-name my/terms-staging-directory)))
         (parts (split-string rel "/"))
         (dir (car parts))
         (base (file-name-base (car (last parts)))))
    (my/jotbooks--crumb-bar
     (append (list (cons "home" "/index.html")
                   (cons dir (format "/%s/index.html" dir)))
             (unless (equal base "index")
               (list (cons base (format "/%s/%s.html" dir base))))))))

(defun my/jotbooks-publish (&optional force)
  "Publish the jotbooks project; with prefix FORCE, republish everything."
  (interactive "P")
  (org-publish "jotbooks" force))

(defun my/nodes-publish (&optional force)
  "Publish the nodes project; with prefix FORCE, republish everything."
  (interactive "P")
  (org-publish "nodes" force))

(defun my/terms-publish (&optional force)
  "Publish the tag and category trees; with prefix FORCE, republish all."
  (interactive "P")
  (org-publish "terms" force))

(provide 'jotbook)
