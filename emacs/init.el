(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  :init
  (recentf-mode))

(use-package vertico
  :straight t
  :custom
  (vertico-resize t)
  :init
  (vertico-mode))

(use-package tab-bar
  :custom
  (tab-bar-show nil)
  (mode-line-format
   (append mode-line-format
           (list
            '(:eval
              (let* ((current-tab (-find (lambda (tab)
                                           (equal (car tab)
                                                  'current-tab))
                                         (tab-bar-tabs)))
                     (current-tab-name (or
                                        (and current-tab
                                             (cdadr current-tab))
                                        "")))
                (format "[%s]" current-tab-name))))))
  :init
  (tab-bar-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults '())
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  ("C-;" . embark-act))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

(use-package eshell
  :custom
  (eshell-banner-message '(format "%s %s\n"
                                  (propertize (format " %s " (string-trim (buffer-name)))
                                              'face 'mode-line-highlight)
                                  (propertize (current-time-string)
                                              'face 'font-lock-keyword-face))))

(use-package consult
  :straight t
  :bind
  (:map global-map
        ("M-g g" . consult-goto-line)
        ("M-g f" . consult-flymake)
        ("M-g j" . consult-compile-error)

        ("M-s M-j" . consult-outline)
        ("M-s M-l" . consult-locate)
        ("M-s M-o" . consult-multi-occur)

        ("M-s f" . consult-fd)
        ("M-s g" . consult-ripgrep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-recent-file)
        ("M-s i" . consult-imenu)
        ("M-s l" . consult-focus-lines))
  (:map ctl-x-map
        ("b" . consult-buffer)
        ("M-:" . consult-complex-command))
  (:map ctl-x-r-map
        ("b" . consult-bookmark)
        ("x" . consult-register))
  (:map minibuffer-local-map
        ("C-r" . consult-history)))

(use-package gcmh
  :straight t
  :init
  (gcmh-mode))

(use-package magit
  :straight t)
  
(use-package minions
  :straight t
  :init
  (minions-mode))

(use-package org
  :straight t
  :hook
  (org-mode . auto-fill-mode))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  :init
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)
  :config
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  :bind
  (:map global-map
        ("C-c n f" . org-roam-node-find))
  (:map org-mode-map
        ("C-c n l" . org-roam-buffer-toggle)
        ("C-c n I" . org-roam-node-insert)
        ("C-c n i" . org-roam-node-insert-immediate)))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package emacs
  :custom
  (user-full-name "David Holmqvist")
  (user-mail-address "david.holmqvist@mailbox.org")
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backup"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (ring-bell-function 'ignore)
  (indent-tabs-mode nil)
  (truncate-lines t)
  (inhibit-startup-screen t)
  (echo-keystrokes 0.1)
  (indicate-buffer-boundaries t)
  (indicate-empty-lines nil)
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  (bidi-display-reordering 'left-to-right)
  (scroll-conservatively 101)
  :init
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (blink-cursor-mode 0))

(use-package cus-face
  :config
  (cl-flet ((generate-face (font height)
                           `((t (:family ,font :height ,height)))))
    (let ((font "FantasqueSansMono Nerd Font")
          (height 120))
      (custom-set-faces
       `(default ,(generate-face font height))
       `(fixed-pitch ,(generate-face font height))
       `(variable-pitch ,(generate-face font (+ height 10)))))))

(use-package modus-themes
  :custom
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-paren-match '(underline))
  (modus-themes-intense-paren-match t)
  (modus-themes-bold-constructs t)
  (modus-themes-links '(faint neutral-underline))
  (modus-themes-hl-line '(intense))
  (modus-themes-completions 'opinionated)
  (modus-themes-prompts '(bold background))
  (modus-themes-org-blocks 'grayscale)
  (modus-themes-tabs-accented t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-diffs 'desaturated)
  (modus-themes-syntax nil)
  (modus-themes-scale-headings t)
  (modus-themes-scale-1 1.1)
  (modus-themes-scale-2 1.15)
  (modus-themes-scale-3 1.20)
  (modus-themes-scale-4 1.25)
  (modus-themes-scale-title 1.30)
  (modus-themes-section-headings nil)
  (modus-themes-region '(no-extend accented))
  (modus-themes-variable-pitch-headings nil)
  (modus-themes-headings '((t . (background overline rainbow))))
  (modus-themes-fringes 'subtle))

(use-package circadian
  :straight t
  :custom
  (calendar-latitude 56.031200)
  (calendar-longitude 14.154950)
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset . modus-vivendi)))
  :init
  (circadian-setup))
