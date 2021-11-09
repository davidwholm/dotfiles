;;; tabspaces.el --- Use tabs to handle workspaces
;;; Commentary:
;;; Code:
;; Package-Requires: ((dash "2.19.1"))

(require 'tab-bar)
(require 'subr-x)
(require 'cl-lib)

(defgroup tabspaces
  nil
  "Provide functions and mode-line lighter for handling tabspaces."
  :group 'convenience)

(defcustom tabspaces-current-tab
  '((t :inherit highlight))
  "The face used for highlighting the current tab."
  :type 'face
  :group 'tabspaces)

(defcustom tabspaces-grouping
  '("[" . "]")
  "Delimiters used when grouping tabspaces in the echo area."
  :type 'pair
  :group 'tabspaces)

(defcustom tabspaces-delimiter
  " "
  "Delimiters used when delimiting tabs in the echo area."
  :type 'string
  :group 'tabspaces)

(defcustom tabspaces-mode-line-component-function
  #'tabspaces--current-tabspace
  "Mode-line components for showing the current tabspace."
  :type 'function
  :group 'tabspaces)

(defvar tabspaces--old-mode-line-format
  nil
  "The `mode-line-format' which will be restored when turning off `tabspaces' mode.")

(defun tabspaces--current-tabspace ()
  "Get the current tabspace as a string."
  (let* ((current-tab (-find (lambda (tab)
                               (equal (car tab)
                                      'current-tab))
                             (tab-bar-tabs)))
         (current-tab-name (alist-get 'name current-tab))
         (explicit-name-p (alist-get 'explicit-name current-tab)))
    (format "[%s]" (or (and explicit-name-p
                            current-tab-name)
                       ""))))

(defun tabspaces--list-tabspaces ()
  "Collect all tabs into correctly propertized string.
The current tab is highlighted with the `tabspaces-current-tab' face,
all other as regular strings."
  (string-join (cl-loop for tab in (tab-bar-tabs)
                        collect (let* ((explicit-name-p (alist-get 'explicit-name tab))
                                       (current-tab-p (equal (car tab) 'current-tab))
                                       (tab-name (or (and explicit-name-p
                                                          (alist-get 'name tab))
                                                     " ")))
                                  (concat
                                   (car tabspaces-grouping)
                                   (if current-tab-p
                                       (propertize tab-name 'face tabspaces-current-tab)
                                     tab-name)
                                   (cdr tabspaces-grouping))))
               tabspaces-delimiter))

(defun tabspaces-show-tabspaces ()
  "Show tabspaces in echo area."
  (interactive)
  (message "%s" (tabspaces--list-tabspaces)))

;;;###autoload
(define-minor-mode tabspaces-mode
  "Toggle `tabspaces' mode."
  :global t
  (if tabspaces-mode
      ;; turning tabspaces on
      (progn
        (setq tabspaces--old-mode-line-format mode-line-format)
        (setq-default mode-line-format (append tabspaces--old-mode-line-format
                                       (list
                                        '(:eval (funcall tabspaces-mode-line-component-function)))))
        (or (not tab-bar-show)
            (setq tab-bar-show nil))
        (tab-bar-mode 1)
        (advice-add #'tab-next :after #'tabspaces-show-tabspaces)
        (advice-add #'tab-previous :after #'tabspaces-show-tabspaces))
    ;; turning tabspaces off
    (setq-default mode-line-format tabspaces--old-mode-line-format)
    (setq tabspaces--old-mode-line-format nil)
    (tab-bar-mode 0)
    (advice-remove #'tab-next #'tabspaces-show-tabspaces)
    (advice-remove #'tab-previous #'tabspaces-show-tabspaces)))


(provide 'tabspaces)
;;; tabspaces.el ends here
