;;; emacs-email-templates-mu4e.el --- mu4e integration for email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration with mu4e to compose emails using templates.

;;; Code:

(require 'emacs-email-templates-core)
(require 'mu4e)

(defun emacs-email-compose-with-template-mu4e (template-name)
  "Compose a new mu4e email using TEMPLATE-NAME."
  (interactive "sTemplate Name: ")
  (let ((template (cl-find-if (lambda (tpl) (string= (plist-get tpl :name) template-name))
                               emacs-email-templates-list)))
    (if template
        (mu4e-compose-new)
      (message "Template '%s' not found." template-name))
    ;; Wait for mu4e compose buffer to be ready
    (with-current-buffer (mu4e-compose-buffer)
      (when template
        (insert (plist-get template :body))
        (goto-char (point-min))
        (when (re-search-forward "^Subject: " nil t)
          (replace-match (concat "Subject: " (plist-get template :subject))))))
    (message "Composed new email with template '%s'." template-name)))

;;;###autoload
(defun emacs-email-insert-template-mu4e ()
  "Insert a selected template into the current mu4e compose buffer."
  (interactive)
  (let ((template-name (completing-read "Choose a template: "
                                       (mapcar (lambda (tpl) (plist-get tpl :name))
                                               emacs-email-templates-list)
                                       nil t)))
    (emacs-email-compose-with-template-mu4e template-name)))

(provide 'emacs-email-templates-mu4e)
;;; emacs-email-templates-mu4e.el ends here
