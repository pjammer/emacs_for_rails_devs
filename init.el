;; General Config
;; ---------------
;;(setq backup-directory-alist (list (cons ".*" (expand-file-name "~/.emacsbackup/"))))
;; (setq version-control t ;; Use version numbers for backups
;;       kept-new-versions 16 ;; Number of newest versions to keep
;;       kept-old-versions 2 ;; Number of oldest versions to keep
;;       delete-old-versions t ;; Ask to delete excess backup versions?
;;       backup-by-copying-when-linked t) ;; Copy linked files, don't rename.
;;   (defun force-backup-of-buffer ()
;;     (let ((buffer-backed-up nil))
;;    (backup-buffer)))
;;   (add-hook 'before-save-hook  'force-backup-of-buffer)
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
;; making ruby mode take effect in rake files
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb\\'" . rhtml-mode) auto-mode-alist))
;;; <M-x> to comment-or-uncomment-region (TextMate compatibility)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "\"") 'insert-pair)
(global-set-key (kbd "C-S-m") 'magit-status)
;; el-get sourcing
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

;; Color-Theme
;;(color-theme-chocolate-rain)
(load-theme 'wombat)
;;(load-theme 'naquadah)
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
(set-face-attribute 'default nil :height 160)
(windmove-default-keybindings 'meta)
;; Cucumber Mode
(add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;; Haml mode stuff and Haml mode was not loading hope it does now.
(require 'haml-mode)
(add-hook 'haml-mode-hook
                  '(lambda ()
                         (setq indent-tabs-mode nil)
                         (define-key haml-mode-map "\C-m" 'newline-and-indent)))
;; ;; optional configurations
;; ;; default language if .feature doesn't have "# language: fi"
;; ;(setq feature-default-language "fi")
;; ;; point to cucumber languages.yml or gherkin i18n.yml to use
;; ;; exactly the same localization your cucumber uses
;; ;(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
;; ;; and load feature-mode
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(yas/global-mode 1)
(setq yas/root-directory '("~/.emacs.d/snippets"
			 "~/.emacs.d/el-get/yasnippet/snippets"))
;; Load the snippets
(mapc 'yas/load-directory yas/root-directory)
;; Load Magit, for some reason didn't load automatically
