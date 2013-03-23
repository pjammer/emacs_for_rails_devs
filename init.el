;; General Config
;; ---------------
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/"))))
(setq make-backup-files nil) ;; disable backup files
(setq auto-save-default nil) ; turns off that blasted auto-save shit
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 170))
(global-linum-mode t) ;; line numbers on the side.
(delete-selection-mode t) ;; highlight a word and start typing, and it will delete the word and put your typed characters in it's place. highly annoying if not there.
(blink-cursor-mode t) ;; blinking cursor easier to find, trust.
(electric-pair-mode t) ;; allows for matching "" and () and []
(tool-bar-mode -1); turn off toolbar
(hl-line-mode t) ; turn on highlight line mode
(setq inhibit-startup-message t) ;; meh, not needed on your 300+ day of using
(global-set-key "\C-x\C-k" 'kill-this-buffer) ;; Kill the buffer you no longer want in your frame.
(fset 'yes-or-no-p 'y-or-n-p) ;; uses y instead of yes. less typing
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; deletes all whitespace that isn't needed.
(set-face-attribute 'default nil :height 160)
(windmove-default-keybindings 'meta) ;; Move windows by using M+arrowkeyleft/right/whatever
(global-set-key (kbd "C-S-m") 'magit-status) ;; if you use git, great way to see wtf is going on

;; el-get config
;; -------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-packages
       '(
	 haml-mode
	 magit
	 rinari
	 rvm
	 ruby-end
	 ruby-compilation
	 rhtml-mode
	 rspec-mode
	 yaml-mode
	 yasnippet
	 zencoding-mode))

(el-get 'sync el-get-packages)

;; Built In Theme
;; adwaita 	deeper-blue 	dichromacy 	leuven 	light-blue 	manoj-dark
;; misterioso 	tango 	tango-dark 	tsdh-dark 	tsdh-light 	wheatgrass
;; whiteboard 	wombat
;; --------------
 (load-theme 'wombat)
;; ido mode for file finding
;; -------------------------
(require 'ido)
(ido-mode 1)
(setq ido-enable-prefix nil
ido-enable-flex-matching t
ido-use-filename-at-point 'guess
ido-max-prospects 10)
;;; additional keys (copied from source file)
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map " " 'ido-next-match))
;; Ruby Mode Adjustments
;; --------------------
;; making ruby mode take effect in our odd Rails project files
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile*" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.erb\\'" . rhtml-mode) auto-mode-alist))
(add-hook 'ruby-mode-hook
     (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))) ;; hitting enter will indent.
(global-set-key (kbd "\C-c\C-c") 'comment-or-uncomment-region) ;; highlight region and comment
;;Rhtml mode for erb files
;; --------------------
(require 'rhtml-mode)
;; Haml mode stuff and Haml mode was not loading hope it does now.
;; --------------------
(require 'haml-mode)
(add-hook 'haml-mode-hook
                  '(lambda ()
                         (setq indent-tabs-mode nil)
                         (define-key haml-mode-map "\C-m" 'newline-and-indent)))
;; Yasnippet config
;; --------------------
;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(yas/global-mode t)
(setq yas/root-directory '("~/.emacs.d/snippets"
			 "~/.emacs.d/el-get/yasnippet/snippets"))
;; Load the snippets
(mapc 'yas/load-directory yas/root-directory)
(custom-set-variables
;; No clue but leaving it config
;; --------------------
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
