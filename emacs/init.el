(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t)
    :config
    (leaf-keywords-init))

(leaf evil
  :ensure t
  :bind
  (("<escape>" . keyboard-escape-quit)
   ("C-M-u" . universal-argument))
  :custom
  `((evil-want-C-u-scroll . t)
    (evil-want-C-d-scroll . t)
    (evil-disable-insert-state-bindings . t)
    (evil-search-module . 'isearch)
    (evil-want-Y-yank-to-eol . t)
    (evil-undo-system . 'undo-redo)
    (evil-normal-state-modes . '(prog-mode text-mode))
    (evil-insert-state-modes . '())
    (evil-visual-state-modes . '())
    (evil-replace-state-modes . '())
    (evil-operator-state-modes . '())
    (evil-motion-state-modes . '())
    (evil-emacs-state-modes . '())
    (evil-default-state . 'emacs))
  :init
  (evil-mode))

(leaf gcmh
  :ensure t
  :init (gcmh-mode))

(leaf leuven-theme
  :ensure t
  :init
  (load-theme 'leuven t))

(leaf doom-modeline
  :ensure t
  :custom
  `((doom-modeline-height . 35))
  :init
  (doom-modeline-mode))

(leaf emacs
  :custom
  `((inhibit-splash-screen . t)
    (truncate-lines . t)
    (indent-tabs-mode . nil)
    (cursor-in-non-selected-windows . nil))
  :init
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (show-paren-mode)
  (blink-cursor-mode 0)
  (electric-pair-mode)
  (push '(font . "JetBrainsMono Nerd Font-12") default-frame-alist ))

(leaf vertico
  :ensure t
  :init
  (vertico-mode))

(leaf consult
  :ensure t)

(leaf embark
  :ensure t
  :bind
  (("C-;" . embark-act)))

(leaf orderless
  :ensure t
  :custom
  `((completion-styles . '(orderless))
    (completion-category-defaults . '())
    (completion-category-overrides . '((file (styles . (partial-completion)))))))

(leaf corfu
  :ensure t
  :init
  (corfu-global-mode))

(leaf racket-mode
  :ensure t)

(leaf magit
  :ensure t)

(leaf org
  :ensure t)

(leaf org-roam
  :ensure t
  :custom
  `((org-roam-v2-ack . t)
    (org-roam-directory . ,(file-truename "~/org-roam")))
  :init
  (ignore-errors
    (make-directory "~/org-roam"))
  (org-roam-db-autosync-mode))

(leaf sly
  :ensure t)

(defun dh:zathura ()
  (interactive)
  (let ((filename (read-file-name "[PDF]: " (file-truename "~/Documents/"))))
    (start-process-shell-command (concat filename "-zathura-proc")
                                 nil
                                 (concat "zathura" " " filename))))
