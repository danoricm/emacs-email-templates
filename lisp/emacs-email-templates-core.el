;;; emacs-email-templates-core.el --- Core functionalities for email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Core functions for managing email templates, including loading, saving,
;; creating, editing, deleting, and listing templates.

;;; Code:

(defgroup emacs-email-templates nil
  "Manage email templates across multiple email clients."
  :group 'tools)

(defcustom emacs-email-client 'mu4e
  "The email client currently in use. Options include 'mu4e, 'notmuch, and 'wanderlust."
  :type 'symbol
  :group 'emacs-email-templates)

(defvar emacs-email-templates-list nil
  "List of email templates.")

(defun emacs-email-load-templates ()
  "Load templates from the user's template file."
  (let ((template-file "~/.emacs.d/emacs-email-templates/templates.el"))
    (when (file-exists-p template-file)
      (setq emacs-email-templates-list
            (with-temp-buffer
              (insert-file-contents template-file)
              (read (buffer-string)))))))

(defun emacs-email-save-templates ()
  "Save the current list of templates to a file."
  (let ((template-file "~/.emacs.d/emacs-email-templates/templates.el"))
    (make-directory (file-name-directory template-file) 'parents)
    (with-temp-file template-file
      (insert (prin1-to-string emacs-email-templates-list)))))

(defun emacs-email-create-template ()
  "Interactively create a new email template."
  (interactive)
  (let* ((name (read-string "Template Name: "))
         (subject (read-string "Subject: "))
         (body (read-string "Body: "))
         (template `(:name ,name :subject ,subject :body ,body)))
    (add-to-list 'emacs-email-templates-list template)
    (emacs-email-save-templates)
    (message "Template '%s' created successfully!" name)))

(defun emacs-email-edit-template (name)
  "Edit an existing template by NAME."
  (interactive "sTemplate Name: ")
  (let ((template (cl-find-if (lambda (tpl) (string= (plist-get tpl :name) name))
                              emacs-email-templates-list)))
    (if template
        (progn
          (setf (plist-get template :subject) (read-string "New Subject: " (plist-get template :subject)))
          (setf (plist-get template :body) (read-string "New Body: " (plist-get template :body)))
          (emacs-email-save-templates)
          (message "Template '%s' updated successfully!" name))
      (message "Template '%s' not found." name))))

(defun emacs-email-delete-template (name)
  "Delete a template by NAME."
  (interactive "sTemplate Name: ")
  (setq emacs-email-templates-list
        (cl-remove-if (lambda (tpl) (string= (plist-get tpl :name) name))
                      emacs-email-templates-list))
  (emacs-email-save-templates)
  (message "Template '%s' deleted successfully!" name))

(defun emacs-email-list-templates ()
  "List all available templates."
  (interactive)
  (if emacs-email-templates-list
      (with-output-to-temp-buffer "*Email Templates*"
        (dolist (template emacs-email-templates-list)
          (princ (format "Name: %s\nSubject: %s\n\n"
                         (plist-get template :name)
                         (plist-get template :subject)))))
    (message "No templates available.")))

(provide 'emacs-email-templates-core)
;;; emacs-email-templates-core.el ends here
