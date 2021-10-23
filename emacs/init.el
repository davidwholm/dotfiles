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

(use-package which-key
  :straight t
  :custom
  (which-key-show-early-on-C-h t)
  :init
  (which-key-mode))

(use-package evil
  :straight t
  :defer t
  :bind
  ("<escape>" . keyboard-escape-quit)
  ("C-M-u" . universal-argument)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-disable-insert-state-bindings t)
  (evil-search-module 'isearch)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  (evil-normal-state-modes '(prog-mode
                             text-mode
                             conf-mode))
  (evil-insert-state-modes '())
  (evil-visual-state-modes '())
  (evil-replace-state-modes '())
  (evil-operator-state-modes '())
  (evil-motion-state-modes '())
  (evil-emacs-state-modes '())
  (evil-default-state 'emacs)
  :init
  (evil-mode))

(use-package gcmh
  :straight t
  :init (gcmh-mode))

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 35)
  :init
  (doom-modeline-mode))

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (truncate-lines t)
  (indent-tabs-mode nil)
  (cursor-in-non-selected-windows nil)
  (modus-themes-no-mixed-fonts t)
  :init
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode)
  (blink-cursor-mode 0)
  (electric-pair-mode)
  (load-theme 'modus-operandi)
  (push '(font . "JetBrainsMono Nerd Font-12") default-frame-alist ))

(use-package mct
  :straight (mct :type git :host gitlab :repo "protesilaos/mct")
  :init
  (mct-mode))

(use-package consult
  :straight t)

(use-package embark
  :straight t
  :bind
  ("C-;" . embark-act))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults '())
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :straight t
  :init
  (corfu-global-mode))

(use-package sudo-edit
  :straight t
  :after embark
  :bind
  (:map embark-file-map
        ("s" . sudo-edit)))

(use-package racket-mode
  :straight t)

(use-package magit
  :straight t)

(use-package org
  :straight t
  :hook
  (org-mode . auto-fill-mode))

(use-package org-roam
  :straight t
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (file-truename "~/org-roam"))
  :bind
  ("C-c n f" . org-roam-node-find)
  :init
  (ignore-errors
    (make-directory "~/org-roam"))
  (org-roam-db-autosync-mode))

(use-package sly
  :straight t)

(defun dh:zathura ()
  (interactive)
  (let ((filename (read-file-name "[PDF]: " (file-truename "~/Documents/"))))
    (start-process-shell-command (concat filename "-zathura-proc")
                                 nil
                                 (concat "zathura" " " filename))))
