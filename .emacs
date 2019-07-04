;; Wrangle emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(frame-background-mode (quote dark))
 '(js-indent-level 2)
 '(org-agenda-custom-commands
   (quote
    (("d" todo "DELEGATED" nil)
     ("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
	(lambda nil
	  (org-agenda-skip-entry-if
	   (quote notregexp)
	   "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo "UNSCHEDULED"
      ((org-agenda-skip-function
	(lambda nil
	  (org-agenda-skip-entry-if
	   (quote scheduled)
	   (quote deadline)
	   (quote regexp)
	   "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/ORG/todo.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+headline "~/ORG/todo.org" "Tasks")
      "* TODO %?
 %u")
     ("n" "Notes" entry
      (file "~/ORG/notes.org")
      "* %u %?"))))
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/ORG/notes.org")
 '(org-directory "~/ORG")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (projectile htmlize plantuml-mode markdown-mode magit terraform-mode exec-path-from-shell)))
 '(plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d3743" :foreground "#e1e1e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Menlo"))))
 '(cursor ((t (:background "MediumPurple1")))))

;; Ensure that the exec path has all my env goodies
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Magit
(global-set-key (kbd "\C-x g") 'magit-status)

;; ORG
(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (define-key org-todo-state-map "x" #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d" #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f" #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l" #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s" #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w" #'(lambda nil (interactive) (org-todo "WAITING")))
     (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
     ))
