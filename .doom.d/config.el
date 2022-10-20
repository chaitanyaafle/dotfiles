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

(setq debug-on-error t)

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 8 8)))

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

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
  (setq visual-fill-column-width 180
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

; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-agenda-files (list
                   (concat org-directory "tasks.org")
                   (concat org-directory "habits.org")
                   (concat org-directory "reading.org")))

(setq org-agenda-category-icon-alist
      `(("work" ,(list (all-the-icons-faicon "briefcase")) nil nil :ascent center)
        ("laundry" ,(list (all-the-icons-material "local_laundry_service")) nil nil :ascent center)
        ("archive" ,(list (all-the-icons-faicon "archive")) nil nil :ascent center)
        ("buy" ,(list (all-the-icons-faicon "shopping-cart")) nil nil :ascent center)
        ("watch" ,(list (all-the-icons-material "tv")) nil nil :ascent center)
        ("shower" "~/.doom.d/icons/shower.svg" nil nil :ascent center :mask heuristic)
        ("clean" "~/.doom.d/icons/broom.svg" nil nil :ascent center :mask heuristic)
        ("walk" ,(list (all-the-icons-material "directions_walk")) nil nil :ascent center)
        ("exercise" "~/.doom.d/icons/dumbbell.svg" nil nil :ascent center :mask heuristic)
        ("sports" "~/.doom.d/icons/futbol.svg" nil nil :ascent center :mask heuristic)
        ("travel" ,(list (all-the-icons-faicon "plane")) nil nil :ascent center)
        ("food" "~/.doom.d/icons/utensils.svg" nil nil :ascent center :mask heuristic)
        ("meeting" "~/.doom.d/icons/handshake.svg" nil nil :ascent center :mask heuristic)
        ("daily-process" ,(list (all-the-icons-material "replay")) nil nil :ascent center)
        ("social" ,(list (all-the-icons-faicon "users")) nil nil :ascent center)
        ("grind" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
        ("water", (list (all-the-icons-faicon "leaf")) nil nil :ascent center)
        ("chore" ,(list (all-the-icons-faicon "check-circle")) nil nil :ascent center)
        ("read" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))
(setq org-agenda-hidden-separator "‌‌ ")

(setq org-agenda-block-separator nil)

(setq org-agenda-breadcrumbs-separator " ❱ "
     org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈┈┈ now"
     org-agenda-time-grid '((weekly today require-timed)
                            (800 1000 1200 1400 1600 1800 2000)
                            "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
     org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                (todo . " %i %-12:c")
                                (tags . " %i %-12:c")
                                (search . " %i %-12:c")))

;(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
;                                                    "\n"
;                                                    (org-agenda-format-date-aligned date))))
;(setq org-cycle-separator-lines 2)
(setq org-agenda-custom-commands
      '(
        ("a" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero t)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 2)
                      (org-agenda-overriding-header "------------\n  CALENDER \n------------")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈┈ NOW")
                      (org-agenda-breadcrumbs-separator " ❱ ")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) () "           " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (tags-todo "-CATEGORY=\"work\"" (
                      (org-agenda-overriding-header "---------\n  TO DO\n---------")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

         (tags "+project-CATEGORY=\"work\"" (
                      (org-agenda-overriding-header "------------\n  PROJECTS\n------------")
                      (org-agenda-remove-tags t)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i %?b %(org-agenda-get-progress)")
                      (org-agenda-todo-keyword-format "")))
         ))

      ("w" "Work Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero t)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 2)
                      (org-agenda-overriding-header "------------\n  CALENDER \n------------")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ NOW")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (tags-todo "+CATEGORY=\"work\"" (
                      (org-agenda-overriding-header "---------\n  TO DO\n---------")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

         (tags "+project+CATEGORY=\"work\"" (
                      (org-agenda-overriding-header "------------\n  PROJECTS\n------------")
                      (org-agenda-remove-tags t)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-show-inherited-tags nil)
                      (org-agenda-prefix-format "   %-2i %?b %(org-agenda-get-progress)")
                      (org-agenda-todo-keyword-format "")))
         ))


("mo" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero nil)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 3)
                      (org-agenda-overriding-header "------------\n  CALENDER \n------------")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "   %i %?-2 t%s")
                      ;; (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                       ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ NOW")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                      (org-agenda-time-grid nil)))

          (todo "TODO" (
                      (org-agenda-overriding-header "---------\n  TO DO\n---------")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %?b")
                      (org-agenda-todo-keyword-format "")))

          ))
))

(use-package visual-fill-column
  :hook (org-super-agenda-mode . ca/org-mode-visual-fill))


(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(defun ca/krofna-hack ()
  (when (looking-back (rx "C-c C-x C-l"))
    (save-excursion
      (backward-char 1)
      (org-toggle-latex-fragment))))

(add-hook 'org-mode-hook
          (lambda ()
            (org-cdlatex-mode)
            (add-hook 'post-self-insert-hook #'ca/krofna-hack 'append 'local)))

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

(use-package ledger-mode
  :mode "\\.ledger\\'")

(load-file "~/.doom.d/ledger-pricedb/ledger-pricedb.el")
(set 'ledger-pricedb--stocks '("GOOGL" "REVS" "TGT" "JEPI" "AAPL" "SCHD" "VOO" "VNQ" "VYM" "CHPT"))
(set 'ledger-pricedb--pricedb "~/org/finances/ledger.pricedb")

(global-set-key (kbd "C-c s") (lambda () (interactive) (ledger-pricedb-save-pricedb)))
