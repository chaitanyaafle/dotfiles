;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Chaitanya Afle"
      user-mail-address "aflechaitanya@gmail.com")

(setq
 undo-limit 20000000                    ; Raise undo limit to 20Mb
 )

(display-time-mode 1)                   ; Enable time display in mode-line
(setq display-time-day-and-date t)
(display-time)

(setq display-line-numbers-type t)      ; Enable Relative line numbering display

(defun doom-modeline-conditional-buffer-encoding () ; Remove the encoding display in mode-line unless its not UTF-8
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook
          'doom-modeline-conditional-buffer-encoding)

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 8 8)))

(setq doom-theme 'doom-one)             ; Set the theme

(setq doom-font (font-spec :family "Iosevka Term SS04" :size 18)
      doom-variable-pitch-font (font-spec :family "Fira Mono")
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
;; (custom-set-faces!
;;   '(font-lock-comment-face :slant italic)
;;   '(font-lock-keyword-face :slant italic))

(add-hook! '(+doom-dashboard-mode-hook)
         (setq fancy-splash-image "~/.doom.d/images/black_hole.png"))
        ;; (setq fancy-splash-image "~/.doom.d/images/Time.png"))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")))

(setq org-directory "~/org/"
      org-roam-directory "~/org/"
      org-ellipsis " ▼ ")

(defun ca/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ca/org-mode-visual-fill))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("j" "Journal" entry
      "* %<%I:%M %p>: %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: Dailies\n\n" ("Journal"))
      :unnarrowed t)
     ("i" "Idea" entry
      "* %<%I:%M %p>: %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: Dailies\n\n" ("Ideas"))
      :unnarrowed t)
     ("t" "Task" entry
      "** TODO %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: Dailies\n\n" ("Tasks"))
      :unnarrowed t)
     ("r" "Reading" entry
      "** TODO %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: Dailies\n\n" ("Reading"))
      :unnarrowed t)))
  (org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "misc/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+filetags:%^g\n\n")
           :unnarrowed t)
          ("r" "Research" plain "%?"
           :if-new (file+head "research/${slug}.org" "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+filetags:%^g\n\n")
           :unnarrowed t)
          ("a" "Research note with an attachment" entry "*** %?"
          :if-new
          (file+head"research/${slug}.org"
                    "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+filetags: notes\n* ${title}\n  :PROPERTIES:\n  :NOTER_DOCUMENT: [[~/org/research/pdfs/%^{pdf_filename}]]\n  :END:\n\n")
          :unnarrowed t)))

          ;; ("a" "Research note with an attachment" plain "%?"
          ;; :if-new (file+head "research/${slug}.org" "#+title: ${title}\n#+author: %(concat      user-full-name)\n#+email: %(concat user-mail-address)\n#+file: [[~/org/research/pdfs/%^]]\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+filetags:%^g\n\n%^{file}")
          ;; :unnarrowed t)
          ;))
  (require 'org-roam-dailies)
  (require 'org-roam-node)
  :config
  (org-roam-setup))

(after! org-roam
    (setq zot_bib (concat org-roam-directory "/master.bib")))

(use-package! org-ref
    :after org-roam
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list zot_bib)
         ;; org-ref-bibliography-notes (concat org-roam-directory "/bibnotes.org")
         ;; org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory org-roam-directory
         org-ref-notes-function 'orb-edit-notes
))

(use-package! helm-bibtex
  :after org-roam
  :init
  ; blah blah
  :config
  ;blah blah
  (setq bibtex-format-citation-functions
      '((org-mode . (lambda (x) (insert (concat
                                         "\\cite{"
                                         (mapconcat 'identity x ",")
                                         "}")) ""))))
(setq
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography
      (list (concat org-roam-directory "/master.bib"))
      bibtex-completion-library-path (list org-roam-directory)
      ;; not needed as I take notes in org-roam
      ; bibtex-completion-notes-path "articles.org"
))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords" "date"))
  (setq orb-file-field-extensions '("pdf" "epub" "html" "mp4" "mp3"))
  (add-to-list 'org-roam-capture-templates
        '("b" "Bibliography reference note" entry "*** %?"
          :if-new
          (file+head"research/refs/${citekey}.org"
                    "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+filetags: Reference notes\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${citekey}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: [[${file}]]\n  :NOTER_PAGE:\n  :KEYWORDS: ${keywords}\n  :END:\n\n")
          :unnarrowed t)))
(after! org-roam (org-roam-bibtex-mode))

(setq deft-directory org-roam-directory)
(setq deft-recursive t)
(setq deft-use-filter-string-for-filename t)
(setq deft-default-extension "org")

(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))
