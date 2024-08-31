;;; emacs-email-templates.el --- Email template management for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; A powerful Emacs package for managing email templates across multiple
;; email clients like mu4e, notmuch, and wanderlust. It allows users to
;; create, manage, share, and download email templates effortlessly.

;;; Code:

(require 'emacs-email-templates-core)
(require 'emacs-email-templates-mu4e)
(require 'emacs-email-templates-notmuch)
(require 'emacs-email-templates-wanderlust)
(require 'emacs-email-templates-github)

(provide 'emacs-email-templates)
;;; emacs-email-templates.el ends here
