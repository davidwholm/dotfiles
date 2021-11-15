;;; init.el --- Emacs configuration file
;;; Commentary:
;;; Code:

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

(use-package yasnippet
  :straight t
  :custom
  (yas-snippet-dirs (list
                     (file-truename (concat user-emacs-directory "snippets"))))
  :hook
  (prog-mode . yas-minor-mode))

(use-package cc-mode
  :custom
  (c-basic-offset 4))

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :hook
  ((prog-mode . tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (defun +tree-sitter-handle-fail-gracefully (orig-fn &rest args)
    (condition-case e
        (apply orig-fn args)
      (error
       (unless (string-match-p (concat "^Cannot find shared library\\|"
                                       "^No language registered\\|"
                                       "cannot open shared object file")
                               (error-message-string e))
         (signal (car e) (cadr e))))))
  (advice-add #'tree-sitter-mode :around #'+tree-sitter-handle-fail-gracefully)
  :init
  (require 'tree-sitter-langs))

(use-package restclient
  :straight t)

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  :init
  (recentf-mode))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package tabspaces
  :load-path "lisp"
  :bind
  (:map ctl-x-map
        ("t C-l" . tabspaces-show-tabspaces))
  :init
  (require 'tabspaces)
  (tabspaces-mode))

(use-package flymake
  :straight t
  :custom
  (flymake-no-changes-timeout 3)
  (flymake-fringe-indicator-position nil)
  :hook
  (prog-mode . flymake-mode))

(use-package eglot
  :straight t
  :custom
  (eglot-stay-out-of '(company flymake eldoc))
  (eglot-autoshutdown t))

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
  :init
  (require 'em-alias)
  (mapcar (lambda (alias-spec)
            (apply #'eshell/alias alias-spec))
          '(("ff" "find-file $1")
            ("ffow" "find-file-other-window $1")
            ("stb" "switch-to-buffer $1")
            ("stbow" "switch-to-buffer-other-window $1")))
  :bind
  (:map eshell-mode-map
        ("C-c <tab>" . lisp-indent-line))
  :custom
  (eshell-banner-message '(format "%s %s\n"
                                  (propertize (format " %s " (string-trim (buffer-name)))
                                              'face 'mode-line-highlight)
                                  (propertize (current-time-string)
                                              'face 'font-lock-keyword-face))))

(use-package eshell-syntax-highlighting
  :straight t
  :init
  (eshell-syntax-highlighting-global-mode))

(use-package goggles
  :straight t
  :hook
  ((prog-mode text-mode) . goggles-mode)
  :custom
  (goggles-pulse t))

(use-package corfu
  :straight t
  :init
  (corfu-global-mode))

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
  (defun +org-roam-node-insert-immediate (arg &rest args)
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
        ("C-c n i" . +org-roam-node-insert-immediate)))

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
    (let ((font "Iosevka Fixed")
          (height 135))
      (custom-set-faces
       `(default ,(generate-face font height))
       `(fixed-pitch ,(generate-face font height))
       `(variable-pitch ,(generate-face font (+ height 10)))))))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-vibrant t))

(use-package mood-line
  :straight t
  :init
  (mood-line-mode))

;;; init.el ends here
