;; General Config
;; ---------------
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/"))))
;; disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil) ; turns off that blasted auto-save shit
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 170))
(global-linum-mode 1)
(delete-selection-mode 1)
(blink-cursor-mode t)
(electric-pair-mode t)
(tool-bar-mode -1); turn off toolbar
(hl-line-mode 1) ; turn on highlight line mode
(setq inhibit-startup-message t)
(global-set-key "\C-x\C-k" 'kill-this-buffer)
(add-hook 'ruby-mode-hook
      (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-face-attribute 'default nil :height 160)
;; Move windows by using M+arrowkeyleft/right/whatever
(windmove-default-keybindings 'meta)
;; making ruby mode take effect in our odd Rails project files
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile*" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb\\'" . rhtml-mode) auto-mode-alist))
;;; <M-x> to comment-or-uncomment-region (TextMate compatibility)
;; (global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "\"") 'insert-pair)
(global-set-key (kbd "C-S-m") 'magit-status)



;; el-get config
;; -------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-packages
       '(
	 autopair
	 haml-mode
	 magit
	 rinari
	 ruby-mode
	 rvm
	 ruby-end
	 ruby-compilation
	 rhtml-mode
	 rspec-mode
	 yaml-mode
	 yasnippet
	 zencoding-mode))

(el-get 'sync el-get-packages)

;; el-get Individual Recipe configs
;; --------------------------------
;; Color-Theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
 (load-theme 'wombat)
;; (load-theme 'zenburn t)
;; ido mode for file finding
(require 'ido)
(ido-mode 1)
(setq ido-enable-prefix -1
ido-enable-flex-matching 1
ido-use-filename-at-point 'guess
ido-max-prospects 10)
;;; additional keys (copied from source file)
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map " " 'ido-next-match))

;;Rhtml mode for erb files
(require 'rhtml-mode)
;; Haml mode stuff and Haml mode was not loading hope it does now.
(require 'haml-mode)
(add-hook 'haml-mode-hook
                  '(lambda ()
                         (setq indent-tabs-mode nil)
                         (define-key haml-mode-map "\C-m" 'newline-and-indent)))
;; Yasnippet config
;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(yas/global-mode 1)
(setq yas/root-directory '("~/.emacs.d/snippets"
			 "~/.emacs.d/el-get/yasnippet/snippets"))
;; Load the snippets
(mapc 'yas/load-directory yas/root-directory)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
