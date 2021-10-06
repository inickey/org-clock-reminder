;;; org-clock-reminder.el --- Reminds current clocking task or its absence -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Nikolay Brovko <i@nickey.ru>

;; Author: Nikolay Brovko <i@nickey.ru>
;; URL: https://github.com/inickey/org-clock-reminder
;; Keywords: calendar, convenience
;; Version: 0.1
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
;; which makes it quite easy to miss the original goal. This package
;; is designed to remind you of the current task, or its absence at
;; specified intervals.
;;
;;; Code:

(require 'notifications)
(require 'org-clock)
(require 'org-duration)

(defgroup org-clock-reminder nil
  "Don't worry about forgetting current task."
  :group 'org-clock)

(defcustom org-clock-reminder-interval 600
  "Notification interval."
  :type 'integer
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-remind-inactivity nil
  "Inactivity reminds are sent when there's no current clocking task.  If you don't want to send this type of notifications, change value to nil."
  :type 'boolean
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-notification-title "Productivity notification"
  "Notification title."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-format #'org-clock-reminder--format
  "Notification message format function."
  :type 'function
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-format-string "You working for %s on<br/>%s"
  "Notification message format string."
  :type 'string
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-method #'org-clock-reminder--notify
  "Notification sending function."
  :type 'function
  :group 'org-clock-reminder)

(defcustom org-clock-reminder-empty-text
  "There is no currently clocking task. Close all distracting windows and continue working..."
  "Text which will be sent when there's no current clocking task."
  :type 'string
  :group 'org-clock-reminder)

(defvar org-clock-reminder--timer nil
  "Notification timer object itself.")

(defun org-clock-reminder--format ()
  "Text message for notification body."
  (if (org-clocking-p)
      (format org-clock-reminder-format-string
              (org-duration-from-minutes (org-clock-get-clocked-time))
              org-clock-heading)
    org-clock-reminder-empty-text))

(defun org-clock-reminder--notify (message)
  "Sends MESSAGE with given body with `notifications-notify."
  (notifications-notify :title org-clock-reminder-notification-title
                        :body message))

(defun org-clock-reminder--timer-function ()
  "This function will be called each timer iteration to prepare and send notification."
  (when (or (org-clocking-p) org-clock-reminder-remind-inactivity)
    (funcall org-clock-reminder-method (funcall org-clock-reminder-format))))

(defun org-clock-reminder-activate ()
  "Activate notification timer.  If you change `org-clock-reminder-interval value after activating, you should restart it."
  (interactive)
  (unless (timerp org-clock-reminder--timer)
    (setq org-clock-reminder--timer (run-with-timer org-clock-reminder-interval
                                                    org-clock-reminder-interval
                                                    #'org-clock-reminder--timer-function))))

(defun org-clock-reminder-deactivate ()
  "Deactivate notification timer."
  (interactive)
  (when (timerp org-clock-reminder--timer)
    (setq org-clock-reminder--timer (cancel-timer org-clock-reminder--timer))))

(provide 'org-clock-reminder)

;;; org-clock-reminder.el ends here
