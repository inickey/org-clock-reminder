;;; org-clock-reminder.el --- Notifications that remind you about clocked-in tasks -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Nikolay Brovko <i@nickey.ru>

;; Author: Nikolay Brovko <i@nickey.ru>
;; URL: https://github.com/inickey/org-clock-reminder
;; Keywords: calendar, convenience
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; In programming, you often have to switch between nested tasks,
;; which makes it quite easy to miss the original goal.  This package
;; is designed to remind you of the current task, or its absence at
;; specified intervals.
;;
;;; Code:

(require 'notifications)
(require 'org-clock)
(require 'org-duration)
(require 'cl-lib)
(require 'format-spec)


;; Configuration

(defgroup org-clock-reminder nil
  "Don't worry about forgetting current task."
  :group 'org-clock)

(defcustom org-clock-reminder-interval 10
  "How frequently reminder should be triggered.

This may be one of two types:

 - A number, which is used for both active (clocked-in) and
   inactive (clocked-out) reminders.
 - A pair of numbers, (active . inactive), where active is used
   for active reminders and inactive is used for inactive
   reminders."
  :type `(choice (number :tag "Active and Inactive")
                 (cons :tag "Separate times for Active and Inactive"
                       (number :tag "Active" :value 10)
                       (number :tag "Inactive" :value 10 )))
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-inactive-notifications-p nil
  "Should reminders be flagged when inactive?

If t, reminders are shown when there is no clocked-in task; if
nil, reminders are not shown.  Additionally, a zero-argument
function may be provided to decide given current time."
  :type '(choice boolean
                 function)
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-formatters
  '((?c . (org-duration-from-minutes (org-clock-get-clocked-time)))
    (?h . org-clock-heading))
  "Format specifiers for `org-clock-reminder-format-message'."
  :type '(repeat (cons :tag "Specifier"
                       (character :tag "Character")
                       (sexp :tag "Expression")))
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-active-title "Productivity notification"
  "Title of notification for `org-clock-reminder' when active (clocked-in).

Format characters described in `org-clock-reminder-formatters'
are available for use."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-active-text "You worked for %c on <br/>%h"
  "Notification Body for `org-clock-reminder' when active (clocked-in).

Format characters described in `org-clock-reminder-formatters'
are available for use."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-inactive-title "Productivity notification"
  "Title of notification for `org-clock-reminder' when inactive (clocked-out).

Format characters described in `org-clock-reminder-formatters'
are available for use."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-inactive-text "No task is being clocked. Close all distracting windows and continue working..."
  "Notification Body for `org-clock-reminder' when inactive (clocked-out).

Format characters described in `org-clock-reminder-formatters'
are available for use."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-icons (cons (expand-file-name "icons/clocking.png"
                                                            (file-name-directory (or buffer-file-name load-file-name)))
                                          (expand-file-name "icons/inactivity.png"
                                                            (file-name-directory (or buffer-file-name load-file-name))))
  "Paths for clocking icons.  Car is path to active icon, cdr path to inactive.

Note: if nil, no icons will be shown."
  :type '(choice (cons (file :tag "Active Icon")
                       (file :tag "Inactive Icon"))
                 (constant :tag "Don't show Icons" nil))
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-notifiers
  (list #'org-clock-reminder-notify)
  "List of functions to call in turn as reminder notifications.

Functions take two arguments, TITLE and MESSAGE."
  :group 'org-clock-reminder
  :type 'hook)


;; Variables

(defvar org-clock-reminder-timer nil
  "Notification timer object itself.")

(defvar org-clock-reminder-state :dormant
  "Current state of the `org-clock-reminder' state machine.

In general, this should not be modified manually.

Acceptable states are:
`:dormant'
`:clocked-out'
`:clocked-in'

The following transition matrix is used:

Current State     Next State        How?
-------------     --------------    -----------------------------------------
`:dormant'        `:clocked-out'    Activation of `org-clock-reminder-mode'
`:clocked-out'    `:clocked-in'     `org-clock-reminder-on-clock-in'
`:clocked-in'     `:clocked-out'    `org-clock-reminder-on-clock-out'
`:clocked-out'    `:dormant'        Deactivation of `org-clock-reminder-mode'
`:clocked-in'     `:dormant'        Deactivation of `org-clock-reminder-mode'")


;; Default Notification Functions

(defun org-clock-reminder--icon ()
  "Icon path for current clocking state."
  (when org-clock-reminder-icons
    (if (equal org-clock-reminder-state :clocked-in)
        (car org-clock-reminder-icons)
      (cdr org-clock-reminder-icons))))

(defun org-clock-reminder-notify (title message)
  "Sends MESSAGE with given TITLE with `notifications-notify."
  (let ((icon-path (org-clock-reminder--icon)))
    (notifications-notify :title title
                          :body message
                          :app-icon icon-path)))


;; Utility Functions

(defun org-clock-reminder-format-message (message)
  "Format MESSAGE using `org-clock-reminder-formatters' for display."
  (if (not (string-match-p "%" message))
      message
    (let ((format-specifiers
           (mapcar (lambda (spec)
                     (cons (car spec) (eval (cdr spec))))
                   (cl-remove-if-not (lambda (spec)
                                       (string-match-p (format "%%%c" (car spec))
                                                       message))
                                     org-clock-reminder-formatters))))
      (format-spec message format-specifiers))))

(defun org-clock-reminder-run-notifications (title body)
  "Run the hook `org-clock-reminder-notifiers' with TITLE and BODY arguments."
  (run-hook-with-args 'org-clock-reminder-notifiers title body))



;; Basic Implementation

(cl-defun org-clock-reminder-reset-timer (&optional reminder-interval)
  "Reset `org-clock-reminder-timer' given optional REMINDER-INTERVAL.

Obey various time related settings, including:
 - `org-clock-reminder-interval'
 - `org-clock-rounding-minutes'"
  (interactive)
  (let* ((reminder-interval (let ((obj (or reminder-interval org-clock-reminder-interval)))
                              (if (numberp obj)
                                  obj
                                (if (equal org-clock-reminder-state :clocked-in)
                                    (car obj)
                                  (cdr obj)))))
         (next-time (* 60 reminder-interval)))
    (when (timerp org-clock-reminder-timer)
      (cancel-timer org-clock-reminder-timer))
    (setf org-clock-reminder-timer (run-at-time next-time nil #'org-clock-reminder-on-timer))))

(defun org-clock-reminder-variable-watcher (symbol new-value op _where)
  "Watcher function for SYMBOL, when changed to NEW-VALUE with OP."
  (when (equal op 'set)
    (message "%s is now %s." symbol new-value)
    (org-clock-reminder-reset-timer new-value)))

(defun org-clock-reminder--show-inactive-notifications-p ()
  "Should inactive notifications be shown?"
  (if (functionp org-clock-reminder-inactive-notifications-p)
      (funcall org-clock-reminder-inactive-notifications-p)
    org-clock-reminder-inactive-notifications-p))

(defun org-clock-reminder-on-timer ()
  "On the `org-clock-reminder-timer', act on `org-clock-reminder-state'.

State           Action
--------------  -------------------------------
`:dormant'      Set timer to nil if not already.
`:clocked-out'  If `org-clock-reminder-remind-inactivity-p',
                show an \"inactive notification\" and reset timer.
`:clocked-in'   Show a notification of in-progress status and reset timer."
  (cl-case org-clock-reminder-state
    (:dormant
     (setf org-clock-reminder-timer nil))
    (:clocked-out
     (when (org-clock-reminder--show-inactive-notifications-p)
       (org-clock-reminder-run-notifications (org-clock-reminder-format-message org-clock-reminder-inactive-title)
                                             (org-clock-reminder-format-message org-clock-reminder-inactive-text))))
    (:clocked-in
     (org-clock-reminder-run-notifications (org-clock-reminder-format-message org-clock-reminder-active-title)
                                           (org-clock-reminder-format-message org-clock-reminder-active-text))))
  (unless (eq org-clock-reminder-state :dormant)
    (org-clock-reminder-reset-timer)))



;; User Entry Points

(defun org-clock-reminder-on-clock-in ()
  "Change `org-clock-reminder-state' on clock-in."
  (interactive)
  (setf org-clock-reminder-state :clocked-in)
  (org-clock-reminder-reset-timer))

(defun org-clock-reminder-on-clock-out ()
  "Change `org-clock-reminder-state' on clock-out."
  (interactive)
  (org-clock-reminder-run-notifications (org-clock-reminder-format-message org-clock-reminder-active-title)
                                        (org-clock-reminder-format-message org-clock-reminder-active-text))
  (setf org-clock-reminder-state :clocked-out)
  (org-clock-reminder-reset-timer))

(define-minor-mode org-clock-reminder-mode
  "Periodically show productivity reminders based on `org-mode' clocking state."
  :global t
  :init-value nil
  :lighter " OCRM"
  :group 'org-clock
  (if org-clock-reminder-mode
      (progn
        (setf org-clock-reminder-state :clocked-out)
        (add-hook 'org-clock-in-hook #'org-clock-reminder-on-clock-in)
        (add-hook 'org-clock-out-hook #'org-clock-reminder-on-clock-out)
        (add-variable-watcher 'org-clock-reminder-interval #'org-clock-reminder-variable-watcher)
        (org-clock-reminder-reset-timer))
    (setf org-clock-reminder-state :dormant)
    (remove-hook 'org-clock-in-hook #'org-clock-reminder-on-clock-in)
    (remove-hook 'org-clock-out-hook #'org-clock-reminder-on-clock-out)
    (remove-variable-watcher 'org-clock-reminder-interval #'org-clock-reminder-variable-watcher)
    (when (timerp org-clock-reminder-timer)
      (cancel-timer org-clock-reminder-timer))
    (setf org-clock-reminder-timer nil)))

(provide 'org-clock-reminder)

;;; org-clock-reminder.el ends here
