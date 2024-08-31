;;; emacs-email-templates-notmuch.el --- Notmuch integration for email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration with Notmuch to compose emails using templates.

;;; Code:

(require 'emacs-email-templates-core)
(require 'notmuch)

(defun emacs-email-compose-with-template-notmuch (template-name)
  "Compose a new Notmuch email using TEMPLATE-NAME."
  (interactive "sTemplate Name: ")
  (let ((template (cl-find-if (lambda (tpl) (string= (plist-get tpl :name) template-name))
                               emacs-email-templates-list)))
    (if template
        (notmuch-mua-new-mail)
      (message "Template '%s' not found." template-name))
    ;; Wait for notmuch compose buffer to be ready
    (with-current-buffer (notmuch-mua--get-compose-buffer)
      (when template
        (goto-char (point-min))
        (insert (concat "Subject: " (plist-get template :subject) "\n\n" (plist-get tpl :body))))
      (message "Composed new email with template '%s'." template-name))))

;;;###autoload
(defun emacs-email-insert-template-notmuch ()
  "Insert a selected template into the current Notmuch compose buffer."
  (interactive)
  (let ((template-name (completing-read "Choose a template: "
                                       (mapcar (lambda (tpl) (plist-get tpl :name))
                                               emacs-email-templates-list)
                                       nil t)))
    (emacs-email-compose-with-template-notmuch template-name)))

(provide 'emacs-email-templates-notmuch)
;;; emacs-email-templates-notmuch.el ends here
