;;; emacs-email-templates-wanderlust.el --- Wanderlust integration for email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration with Wanderlust to compose emails using templates.

;;; Code:

(require 'emacs-email-templates-core)
(require 'wl)

(defun emacs-email-compose-with-template-wanderlust (template-name)
  "Compose a new Wanderlust email using TEMPLATE-NAME."
  (interactive "sTemplate Name: ")
  (let ((template (cl-find-if (lambda (tpl) (string= (plist-get tpl :name) template-name))
                               emacs-email-templates-list)))
    (if template
        (wl-summary-mail-forward)
      (message "Template '%s' not found." template-name))
    ;; Wait for Wanderlust compose buffer to be ready
    (with-current-buffer (wl-summary-buffer)
      (when template
        (wl-message-goto-body)
        (insert (plist-get template :body))
        (goto-char (point-min))
        (when (re-search-forward "^Subject: " nil t)
          (replace-match (concat "Subject: " (plist-get template :subject))))))
    (message "Composed new email with template '%s'." template-name)))

;;;###autoload
(defun emacs-email-insert-template-wanderlust ()
  "Insert a selected template into the current Wanderlust compose buffer."
  (interactive)
  (let ((template-name (completing-read "Choose a template: "
                                       (mapcar (lambda (tpl) (plist-get tpl :name))
                                               emacs-email-templates-list)
                                       nil t)))
    (emacs-email-compose-with-template-wanderlust template-name)))

(provide 'emacs-email-templates-wanderlust)
;;; emacs-email-templates-wanderlust.el ends here
