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

(use-package gcmh
  :straight t
  :init
  (gcmh-mode))

(use-package kaolin-themes
  :straight t
  :init
  (load-theme 'kaolin-bubblegum t))

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 35)
  :init
  (doom-modeline-mode))

(use-package emacs
  :straight nil
  :custom
  (inhibit-splash-screen t)
  (truncate-lines t)
  (indent-tabs-mode nil)
  (cursor-in-non-selected-windows nil)
  :init
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode)
  (blink-cursor-mode 0)
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-13")))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

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
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :straight t
  :init
  (corfu-global-mode))

(use-package racket-mode
  :straight t)

(use-package magit
  :straight t)

(use-package org-superstar
  :straight t
  :custom
  (org-superstar-headline-bullets-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  :hook
  (org-mode . org-superstar-mode))

(use-package org
  :straight t
  :custom
  (org-ellipsis "↝"))
