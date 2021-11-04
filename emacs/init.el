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

(use-package minions
  :straight t
  :init
  (minions-mode))

(use-package org
  :straight t)

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
        ("C-c n i" . org-roam-node-insert)
        ("C-c n I" . org-roam-node-insert-immediate)))

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
  (ring-bell-function 'ignore)
  (indent-tabs-mode nil)
  (truncate-lines t)
  (inhibit-startup-screen t)
  :init
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (set-frame-font "FantasqueSansMono Nerd Font-13" t t))

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
  (modus-themes-fringes 'subtle)
  :init
  (load-theme 'modus-operandi t))
