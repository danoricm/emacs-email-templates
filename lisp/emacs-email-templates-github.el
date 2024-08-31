;;; emacs-email-templates-github.el --- GitHub integration for email templates -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to upload and download email templates from a GitHub repository.

;;; Code:

(require 'emacs-email-templates-core)
(require 'url)
(require 'json)

(defcustom emacs-email-template-repo-url "https://api.github.com/repos/yourusername/emacs-email-template-repo"
  "URL of the GitHub repository storing the email templates."
  :type 'string
  :group 'emacs-email-templates)

(defun emacs-email-upload-template (name)
  "Upload a template by NAME to the template repository on GitHub."
  (interactive "sTemplate Name: ")
  (let ((template (cl-find-if (lambda (tpl) (string= (plist-get tpl :name) name))
                              emacs-email-templates-list)))
    (if template
        (let* ((api-url (concat emacs-email-template-repo-url "/contents/templates/" name ".el"))
               (content (base64-encode-string (prin1-to-string template)))
               (json-data (json-encode `(("message" . ,(concat "Add template " name))
                                        ("content" . ,content))))
               (auth-token (read-string "GitHub Token: ")))
          (url-retrieve
           api-url
           (lambda (status)
             (if (plist-get status :error)
                 (message "Error uploading template: %s" (plist-get status :error))
               (message "Template '%s' uploaded successfully!" name)))
           nil
           `(("Content-Type" . "application/json")
             ("Authorization" . ,(concat "token " auth-token))))
          )
      (message "Template '%s' not found." name))))

(defun emacs-email-download-template (name)
  "Download a template by NAME from the template repository on GitHub."
  (interactive "sTemplate Name: ")
  (let ((api-url (concat emacs-email-template-repo-url "/contents/templates/" name ".el")))
    (url-retrieve
     api-url
     (lambda (status)
       (if (plist-get status :error)
           (message "Error downloading template: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "\n\n" nil t)
         (let* ((json-object-type 'plist)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (json (json-read))
                (content (plist-get json :content))
                (decoded (ignore-errors (base64-decode-string content))))
           (if decoded
               (let ((template (read decoded)))
                 (add-to-list 'emacs-email-templates-list template)
                 (emacs-email-save-templates)
                 (message "Template '%s' downloaded successfully!" name))
             (message "Failed to decode template '%s'." name)))))
     nil t)))

(defun emacs-email-search-templates (query)
  "Search for templates in the GitHub repository matching QUERY."
  (interactive "sSearch Query: ")
  ;; Simple search implementation: list all templates and filter locally
  (url-retrieve
   (concat emacs-email-template-repo-url "/contents/templates")
   (lambda (status)
     (if (plist-get status :error)
         (message "Error fetching templates: %s" (plist-get status :error))
       (goto-char (point-min))
       (re-search-forward "\n\n" nil t)
       (let* ((json-object-type 'plist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json (json-read))
              (templates (mapcar (lambda (tpl) (plist-get tpl :name))
                                 json))
              (matches (cl-remove-if-not (lambda (name) (string-match-p (regexp-quote query) name))
                                         templates)))
         (if matches
             (with-output-to-temp-buffer "*Template Search*"
               (dolist (name matches)
                 (princ (format "Template: %s\n" name))))
           (message "No templates match your query.")))))
   nil t))

(provide 'emacs-email-templates-github)
;;; emacs-email-templates-github.el ends here
