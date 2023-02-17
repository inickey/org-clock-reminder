# org-clock-reminder

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

In programming, you often have to switch between nested tasks, which makes it quite easy to miss the original goal. This package is designed to remind you of the current task, or its absence at specified intervals.

<div>
<img src="./screenshots/screenshot-1.png" width=47.5%>
<img src="./screenshots/screenshot-2.png" width=47.5%>
</div>

## Installation

This package is not in any emacs package repository yet, so you can't install it with `package.el` or any elpa package manager.

### Install with [straight.el](https://github.com/raxod502/straight.el)

Good news is [straight.el](https://github.com/raxod502/straight.el) can install packages directly from Github. So if you use it, just place code bellow to your emacs configuration file.

```emacs-lisp
(straight-use-package
  '(org-clock-reminder :type git :host github :repo "inickey/org-clock-reminder"))

(require 'org-clock-reminder)
(org-clock-reminder-mode)
```

Or you can use code bellow if you use both [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package).

```emacs-lisp
(use-package org-clock-reminder
  :straight (:host github :repo "inickey/org-clock-reminder")
  :config (org-clock-reminder-mode))
```

### Manual Installation

When you have cloned this repo to your machine you can load package by adding its location to `load-path` and requiring it.

```emacs-lisp
(let ((org-clock-reminder-path "~/pr/org-clock-reminder"))
  (when (file-directory-p org-clock-reminder-path)
    (add-to-list 'load-path org-clock-reminder-path)
    (require 'org-clock-reminder)
    (org-clock-reminder-mode)))
```
    
Or you can do same thing with [use-package](https://github.com/jwiegley/use-package) if you prefer to use it (only with locally cloned repo):

```lisp
(use-package org-clock-reminder
  :if (file-exists-p "~/pr/org-clock-reminder/org-clock-reminder.el")
  :load-path "~/pr/org-clock-reminder"
  :config (org-clock-reminder-mode))
```

## Activation and deactivation

After you have installed this package you can activate/deactivate with the `org-clock-reminder-mode` function.  This command follows the normal semantics for global minor modes.  In general, it is sufficient to activate the mode in your `init.el`.

## Customization

The notifications interval can be customized with `org-clock-reminder-interval` which is a number of *minutes*.  It may also keep a pair of numbers, with the car as the number of minutes to wait when active, and the cdr as the number of minutes to wait when inactive, ex:

```emacs-lisp
(setq org-clock-reminder-interval 10) ; By default, notifications (active, inactive) are shown every 10 minutes

(setq org-clock-reminder-interval (cons 3 10)) ; Active notifications are shown every 3 minute, inactive every 3
```

These changes are picked up automatically, and the timer is automatically reset on change.

Reminders during periods of inactivity are off by default, but may be activated easily, by setting `org-clock-reminder-inactive-notifications-p` to a non-nil value.

Notification contents can be changed quite easily.  Titles and bodies are formatted using the format specifiers `org-clock-reminder-formatters` (a list of `(char . expr)` pairs), which by default has `%c` as the current clocked in time, and `%h` as the current clocked-in task.  Format strings available are:

 - `org-clock-reminder-active-title` and `org-clock-reminder-active-text`: The title and body of active (clocked-in) notifications, respectively.
 - `org-clock-reminder-inactive-title` and `org-clock-reminder-inactive-text`: The title and body of inactive (clocked-out) notifications, respectively.


Icons for (default notifications) can be configured using `org-clock-reminder-icons`, as follows.

```emacs-lisp
(setq org-clock-reminder-icons (cons "~/img/clocking.png" "~/img/inactivity.png")) ; Use clocking.png for when a clock is active, inactivity.png for when no clock is active

(setq org-clock-reminder-icons nil) ; Don't show icons.
```


Finally, you can customize how your are notified.  By default the `notifications` library is used, but you may set multiple or alternate notifiers using the hook `org-clock-reminder-notifiers`, with each function taking title and message arguments.

## Contributions

Feel free to send pull requests, feature requests, bug reports or fixes. Also I'd like to keep this package as simple as it can be.

## Icons

I am not designer or picture artist at all, so provided icons are found on the Internet. If you have a better idea for mnemonic icons to use, or you have your own icons, please let me know.

Icons made by <a href="https://www.flaticon.com/authors/freepik" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a>
