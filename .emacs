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
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
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
  
  ;; (setq indent-tabs-mode nil
  ;;       standard-indent 2
  ;;       indicate-empty-lines t)
  ;; (menu-bar-mode 0) ; No Menu bar
  ;; (scroll-bar-mode 0) ; No scroll bar
  ;; (show-paren-mode 1) ; Show parens
  ;; (tool-bar-mode 0)   ; No tool bar
  
  ;; (add-hook 'prog-mode-hook #'linum-mode)
  )

;; Ensure system packages with use-package
;; (use-package use-package-ensure-system-package)

;; Libraries
;; (use-package ag :ensure-system-package ag)
;; (use-package diminish :demand t)
;; (use-package nvm :commands (nvm-use nvm-use-for-buffer))
;; (use-package flx)
;; (use-package system-packages
;;   :demand t
;;   :config
;;   (setq system-packages-use-sudo nil))

;; ;; Package Configuration

;; (use-package add-node-modules-path
;;   :commands add-node-modules-path
;;   :hook (prettier-js-mode . add-node-modules-path))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package company
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :after company
  :commands company-lsp
  :config (add-to-list 'company-backends 'company-lsp))

(use-package dap-hydra
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

(use-package dap-mode
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(use-package dap-ruby
  :custom (dap-ruby-debug-program '("node" "/Users/carl/.emacs.d/.extension/vscode/rebornix.Ruby/extension/dist/debugger/main.js"))
  :config
  (dap-ruby-setup))

(use-package env
  :after exec-path-from-shell
  :config
  (setenv "NPM_TOKEN" "ce95665a-d42b-4956-90e0-f18af37c1667")
  ;; In order to use ssh-agent with git and gpg, set the ssh auth sock
  (setenv "SSH_AUTH_SOCK"
          (substring
           (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")
           0 -1)))

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(ns))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package faces
  :config
  ;; Set my preferred font.
  (set-face-font 'default "Fira Code-14"))

(use-package fira-code-mode
  :diminish
  :load-path "lisp"
  :hook (prog-mode . fira-code-mode))

;; (use-package flycheck
;;   :hook ((emacs-lisp-mode . flycheck-mode)
;;          (lsp-mode . flycheck-mode)))

(use-package graphql-mode
  :mode "\\.graphqls\\'")

(use-package helm
  :demand t
  :commands (helm-M-x)
  :bind ("M-x" . helm-M-x)
  :config (helm-mode 1))

(use-package helm-ls-git
  :after helm
  :bind ("C-x C-d" . 'helm-browse-project))
  
(use-package helm-projectile
  :after helm
  :config (helm-projectile-on))
  

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter))
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-s" . inf-ruby-console-auto)))

;; (use-package isolate
;;   :hook (activate-mark . my-isolate-set-transient-map)
;;   :preface
;;   (defun my-isolate-set-transient-map ()
;;     (set-transient-map
;;      (let ((map (make-sparse-keymap)))
;;        (define-key map "s" #'isolate-quick-add)
;;        (define-key map "S" #'isolate-long-add)
;;        (define-key map "d" #'isolate-quick-delete)
;;        (define-key map "D" #'isolate-long-delete)
;;        (define-key map "c" #'isolate-quick-change)
;;        (define-key map "C" #'isolate-long-change)
;;        map)
;;      #'region-active-p))
;;   :commands (isolate-quick-add isolate-long-add isolate-quick-delete isolate-long-delete isolate-quick-change isolate-long-change))

;; (use-package ivy
;;   :diminish
;;   :demand t
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (enable-recursive-minibuffers t)
;;   (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
;;   :config
;;   (ivy-mode 1))

;; (use-package js
;;   :custom
;;   (js-indent-level 2))


(use-package lsp-mode
  :commands lsp
  :custom (lsp-enable-snippet nil)
  :hook (ruby-mode . lsp))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package minibuffer
  :custom (completion-styles '(flex)))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode)))

;; (use-package prettier-js
;;   :after add-node-modules-path
;;   :hook (typescript-mode . prettier-js-mode))

(use-package linum
  :hook (prog-mode . linum-mode))

(use-package paren
  :demand t
  :config (show-paren-mode 1))

(use-package projectile
  :defer 5
  :diminish
  :hook (prog-mode . projectile-mode)
  :bind-keymap (("s-p" . projectile-command-map))
  :custom (projectile-completion-system 'helm)
  (projectile-enable-caching t)
  (projectile-mode 1))

;; (use-package rspec-mode :hook ruby-mode)

(use-package ruby-mode
  :after (lsp-mode dap-ruby dap-hydra)
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
              ("C-c f" . lsp-format-buffer)
              ("C-c d t" . dap-debug-edit-template)
              ("C-c d d" . dap-debug)
	      ("C-c , , s" . my-dap-debug-rspec-at-line)
              ("C-c , , h" . dap-hydra))
  :custom (ruby-insert-encoding-magic-comment nil))

;; (use-package smart-mode-line
;;   :config
;;   ;; See https://github.com/Malabarba/smart-mode-line/issues/217
;;   (setq mode-line-format (delq 'mode-line-position mode-line-format))
;;   (sml/setup)
;;   (remove-hook 'display-time-hook 'sml/propertize-time-string))

;; (use-package string-inflection
;;   :bind (("C-c <SPC>" . string-inflection-all-cycle)
;;          :map ruby-mode-map
;;               ("C-c <SPC>" . string-inflection-ruby-style-cycle)))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t))

;; (use-package swiper
;;   :after ivy
;;   :bind (:map swiper-map
;;               ("M-y" . yank)
;;               ("M-%" . swiper-query-replace)
;;               ("C-." . swiper-avy)
;;               ("M-c" . swiper-mc))
;;   :bind (:map isearch-mode-map
;;               ("C-o" . swiper-from-isearch)))

;; (use-package treemacs
;;   :bind (("M-0" . treemacs-select-window)))

(use-package typescript-mode
  :hook ((typescript-mode . lsp))
  :custom (typescript-indent-level 2))

(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))



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



(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-styles '(flex))
 '(dap-ruby-debug-program
   '("node" "/Users/carl/.emacs.d/.extension/vscode/rebornix.Ruby/extension/dist/debugger/main.js"))
 '(helm-completion-style 'emacs)
 '(lsp-enable-snippet nil)
 '(package-selected-packages
   '(prettier-js typescript-mode flycheck lsp-ui graphql-mode yaml-mode inf-ruby helm-ag expand-region company-lsp company rspec-mode gnu-elpa-keyring-update dap-mode markdown-mode dockerfile-mode magit exec-path-from-shell solarized-theme helm-projectile projectile helm-ls-git helm which-key use-package))
 '(projectile-completion-system 'helm)
 '(projectile-enable-caching t)
 '(projectile-mode 1 nil (projectile))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((whitespace-line-column . 80)
     (eval add-to-list 'projectile-globally-ignored-directories "*node_modules" t)
     (eval add-to-list 'projectile-globally-ignored-directories "*repos" t)
     (prettier-js-args "--single-quote" "--trailing-comma" "all" "--no-semi")))
 '(typescript-indent-level 2 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
