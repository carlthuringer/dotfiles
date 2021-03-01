;; .emacs --- Emacs setup

;;; Commentary:
; Emacs setup...

;;; Code:

;; Must of this is borrowed from jwiegley's dot-emacs repo:
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el

;; Initialization
(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 100000000
                   gc-cons-percentage 0.5
		   read-process-output-max (* 1024 1024))
             (garbage-collect)) t)

;; Environment
(eval-and-compile
  ;; Adds a directory to the load path to ease loading more custom emacs lisp code.
  (add-to-list 'load-path "~/.emacs.d/lisp/" t)
  ;; Set up package.el the traditional way.
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  ;; Bootstrap jwiegley's use-package
  (unless (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
  (require 'use-package)

  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

  ;; For some reason this cannot be set in use-package.
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (setq x-underline-at-descent-line t)
  (setq visible-bell 1)

  ;; Open Org scratch on startup
  (setq inhibit-startup-screen t)
  (setq initial-major-mode 'org-mode)

  ;; Set visual line mode on markdown
  (add-hook 'markdown-mode-hook #'visual-line-mode)

  ;; Delete trailing whitespace on save in prog modes.
  (add-hook 'prog-mode-hook
	    (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
  )

;; Customize.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "/tmp/" t)) nil nil "Customized with use-package files")
 '(backup-directory-alist '((".*" . "/tmp/")) nil nil "Customized with use-package files")
 '(completion-styles '(flex) nil nil "Customized with use-package minibuffer")
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(helm-completion-style 'emacs)
 '(lsp-enable-snippet nil)
 '(make-backup-files nil nil nil "Customized with use-package files")
 '(org-journal-dir "~/org/")
 '(package-selected-packages
   '(flycheck-ledger ledger-mode lsp-ui lsp-mode docker-tramp docker forge plantuml-mode dumb-jump helm-lsp restclient org-present graphviz-dot-mode jest-test-mode beacon transient-dwim cdlatex company-auctex auctex diminish smart-mode-line isolate mixed-pitch company-org-roam org-roam visual-fill-column iedit nvm helm-tramp default-text-scale prettier-js typescript-mode flycheck yaml-mode inf-ruby helm-ag expand-region company rspec-mode gnu-elpa-keyring-update dap-mode markdown-mode dockerfile-mode magit exec-path-from-shell solarized-theme helm-projectile projectile helm-ls-git helm which-key use-package))
 '(safe-local-variable-values
   '((rspec-use-bundler-when-possible)
     (prettier-js-args "--single-quote" "--trailing-comma" "all" "--no-semi")))
 '(select-enable-clipboard t nil nil "Customized with use-package select")
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ;; Package Configuration


;; General-use Packages
(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package beacon
  :diminish
  :demand t
  :config (beacon-mode 1))
(use-package default-text-scale
  :demand t
  :config (default-text-scale-mode))

(use-package dumb-jump
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package env
  :after exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs '("NPM_TOKEN" "BUNDLE_GEM__FURY__IO" "BUNDLE_GEMS__CONTRIBSYS__COM"))
  ;; In order to use ssh-agent with git and gpg, set the ssh auth sock
  (setenv "SSH_AUTH_SOCK"
          (substring
           (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")
           0 -1)))

(use-package exec-path-from-shell
  :demand t
  ;; :if (memq window-system '(ns))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package faces
  :config
  ;; Set my preferred font.
  (set-face-font 'default "Fira Code-14"))

(use-package files
  :custom (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (make-backup-files nil))

(use-package fira-code-mode
  :diminish
  :if (memq window-system '(ns))
  :load-path "lisp"
  :hook (prog-mode . fira-code-mode))

(use-package flycheck
  :hook (ledger-mode . flycheck-mode))

(use-package flycheck-ledger
  :after ledger-mode)

(use-package frame
  :demand t
  :config (blink-cursor-mode 0))

(use-package helm
  :diminish
  :demand t
  :commands (helm-M-x)
  :bind ("M-x" . helm-M-x)
  :config (helm-mode 1))

(use-package linum
  :diminish
  :hook (prog-mode . linum-mode))

(use-package minibuffer
  :custom (completion-styles '(flex)))
(use-package select
  :if (memq window-system '(x))
  :custom (select-enable-clipboard t))

(use-package smart-mode-line
  :demand t
  :config
  (setq sml/theme 'automatic)
  (sml/setup))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t))

(use-package paren
  :demand t
  :config (show-paren-mode 1))

(use-package unfill-paragraph
  :bind (("M-Q" . unfill-paragraph)))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  )

(use-package which-key
  :diminish
  :defer 5
  :commands which-key-mode
  :config
  (which-key-mode))

;; Git and Source Control
(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit
  :custom (forge-topic-list-limit '(100 . -1))
  )

;; Org-mode

(use-package org
  :diminish
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
	 (org-mode . mixed-pitch-mode))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-format-latex-options '(:foreground default :background default :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 2 :matchers
					  ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-agenda-files '("~/org/"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
)

(use-package org-roam
  :diminish
  :defer 5
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/")
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "daily/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d>\n\n")))
  (org-roam-buffer-position 'bottom)
  (org-roam-index-file "~/org/index.org")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph)
	       ("C-c n n" . org-roam-dailies-today))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package org-present
  :hook ((org-present-mode . (lambda ()
			       (org-present-big)
			       (org-present-show-cursor)
			       (org-redisplay-inline-images)
			       ))
	 (org-present-mode-quit-hook . (lambda ()
					 (org-present-small)
					 (org-remove-inline-images)
					 ))
	 ))

;; Programming

;; - LSP
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
    :hook (
            (ruby-mode . lsp)
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)


(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; -END LSP

(use-package company
  :diminish
  :hook (prog-mode . company-mode))

(use-package dap-hydra
  :after dap-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

(use-package dap-mode
  :diminish
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package dap-ruby
  :after dap-mode
  :custom (dap-ruby-debug-program '("node" "/Users/carl/.emacs.d/.extension/vscode/rebornix.Ruby/extension/dist/debugger/main.js"))
  :config
  (dap-ruby-setup))

(use-package graphql-mode
  :diminish
  :mode "\\.graphqls\\'")

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot)

(use-package helm-ls-git
  :after helm
  :bind ("C-x C-d" . 'helm-browse-project))

(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))

(use-package iedit-mode :diminish :defer 5
  :bind ("C-;" . iedit-mode))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter))
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-s" . inf-ruby-console-auto)))

(use-package isolate
  :hook (activate-mark . my-isolate-set-transient-map)
  :preface
  (defun my-isolate-set-transient-map ()
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "s" #'isolate-quick-add)
       (define-key map "S" #'isolate-long-add)
       (define-key map "d" #'isolate-quick-delete)
       (define-key map "D" #'isolate-long-delete)
       (define-key map "c" #'isolate-quick-change)
       (define-key map "C" #'isolate-long-change)
       map)
     #'region-active-p))
  :commands (isolate-quick-add isolate-long-add isolate-quick-delete isolate-long-delete isolate-quick-change isolate-long-change))

(use-package js
  :custom
  (js-indent-level 2))

(use-package plantuml-mode
  :custom (plantuml-default-exec-mode 'executable))

(use-package prettier-js
  :diminish
  :hook ((typescript-mode . prettier-js-mode)
	 (js-mode . prettier-js-mode)))

(use-package projectile
  :diminish
  :defer 5
  :diminish
  :hook (prog-mode . projectile-mode)
  :bind-keymap (("s-p" . projectile-command-map))
  :custom (projectile-completion-system 'helm)
  (projectile-enable-caching t)
  :preface
  ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
  (defun modi/advice-projectile-no-sub-project-files ()
    "Directly call `projectile-get-ext-command'. No need to try to get a
        list of sub-project files if the vcs is git."
    (projectile-files-via-ext-command (projectile-get-ext-command)))
  :config
  (projectile-mode 1)
  (advice-add 'projectile-get-repo-files :override #'modi/advice-projectile-no-sub-project-files)
)

(use-package ruby-mode
  :preface
  (defun my-dap-debug-rspec-at-line ()
    "Debug RSpec at line"
    (interactive)
    (dap-debug
     (list :type "Ruby"
	   :cwd (projectile-project-root)
	   :request "launch"
	   :program (concat (projectile-project-root) "bin/rspec")
	   :args (list (concat (buffer-file-name) (format ":%d" (line-number-at-pos))))
	   :name "Debug Rspec - open spec file on a certain line"
	   :useBundler "true"
	   :debuggerPort "1235")))
  :bind (:map ruby-mode-map
;              ("C-c f" . lsp-format-buffer)
              ("C-c d t" . dap-debug-edit-template)
              ("C-c d d" . dap-debug)
	      ("C-c d h" . dap-hydra)
	      ("C-c , , s" . my-dap-debug-rspec-at-line))
  :custom (ruby-insert-encoding-magic-comment nil))

(use-package typescript-mode
  :hook ((typescript-mode . lsp))
  :custom (typescript-indent-level 2)
  :config (electric-indent-mode 0))

;;; Finalization

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)



;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(if (file-exists-p "~/.emacs.d/opam-user-setup.el")
    (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
  )
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide '.emacs)
;;; .emacs ends here
