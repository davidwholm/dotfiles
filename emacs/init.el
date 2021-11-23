;;; init.el --- Summary: Emacs initialization file.
;;; Commentary:
;;; Code:

;;; straight.el bootstrap

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

;;; General

(use-package emacs
  :custom
  (user-full-name "David Holmqvist")
  (user-mail-address "david.holmqvist@mailbox.org")
  (echo-keystrokes 0.1)
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  (bidi-display-reordering 'left-to-right)
  (inhibit-splash-screen t)
  (tab-always-indent 'complete)
  (truncate-lines t)
  (indent-tabs-mode nil)
  (ring-bell-function 'ignore)
  (scroll-conservatively 101))

;;; Blackout

(use-package blackout
  :straight t)

;;; Minibuffer

(use-package minibuffer
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;;; Org
(use-package org
  :straight t
  :custom
  (org-directory (file-truename "~/Documents/org"))
  :hook
  (org-mode . auto-fill-mode))

(use-package org-agenda
  :custom
  (org-agenda-files (list org-directory))
  :bind
  ("C-c o a" . org-agenda))

(use-package org-capture
  :custom
  (org-capture-templates
   `(("t"
      "Todo"
      entry
      (file+headline
       "todo.org"
       "Todos")
      ,(concat "* TODO %^{Title} %^g\n"
               "SCHEDULED: %^t\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%i%?"))))
  :bind
  ("C-c o c" . org-capture))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
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
  :straight t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;;; Completion

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package vertico
  :straight t
  :custom
  (vertico-resize t)
  :init
  (vertico-mode))

(use-package corfu
  :straight t
  :init
  (corfu-global-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-background)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Visuals

(use-package cus-face
  :init
  (custom-set-faces
   '(default ((t (:family "JetBrains Mono" :height 110))))
   '(variable-pitch ((t (:family "Jetbrains Mono" :height 100))))))

(use-package ace-window
  :straight t
  :blackout t
  :bind
  ("C-c w" . ace-window))

(use-package frame
  :init
  (blink-cursor-mode 0))

(use-package scroll-bar
  :init
  (scroll-bar-mode 0))

(use-package menu-bar
  :init
  (menu-bar-mode 0))

(use-package tool-bar
  :init
  (tool-bar-mode 0))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-vibrant t))

;;; Programming

(use-package flycheck
  :straight t
  :hook
  (prog-mode . flycheck-mode))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-symbol-highlighting nil))

(use-package consult-lsp
  :straight t)

(use-package consult-flycheck
  :straight t)

(use-package eldoc
  :blackout t)

;;; Utilities

(use-package embark
  :straight t
  :bind
  ("C-;" . embark-act))

(use-package consult
  :straight t)

;;; Garbage collector

(use-package gcmh
  :straight t
  :blackout t
  :init
  (gcmh-mode))

;;; Treesitter

(use-package tree-sitter
  :straight t
  :init
  (advice-add 'tree-sitter-mode :around (lambda (orig-fn &rest args)
                                          (condition-case e
                                              (apply orig-fn args)
                                            (error
                                             (unless (string-match-p (concat "^Cannot find shared library\\|"
                                                                             "^No language registered\\|"
                                                                             "cannot open shared object file")
                                                                     (error-message-string e))
                                               (signal (car e) (cadr e)))))))
  :hook
  (prog-mode . tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

;;; Request

(use-package restclient
  :straight t)

;;; Editing

(use-package avy
  :straight t
  :bind
  ("C-c j" . avy-goto-char-timer))

(use-package paren
  :init
  (show-paren-mode))

(use-package cc-mode
  :custom
  (c-basic-offset 4))

;;; Files

(use-package files
  :custom
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backup"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5))

;;; Version control

(use-package magit
  :straight t)

(provide 'init)
;;; init.el ends here
