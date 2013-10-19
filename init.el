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
;; Move windows by using M+arrowkeyleft/right/whatever
(windmove-default-keybindings 'meta)
;;(global-set-key (kbd "\"") 'insert-pair)
(global-set-key (kbd "C-S-m") 'magit-status) ;; if you use git, great way to see wtf is going on

;; el-get config
;; -------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(setq el-get-packages
       '(
         haml-mode
         magit
         rvm
         ruby-end
         rspec-mode
	 web-mode
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
ido-max-prospects 10)
;;; additional keys (copied from source file)
;; (defun ido-my-keys ()
;;   "Add my keybindings for ido."
;;   (define-key ido-completion-map " " 'ido-next-match))
;; Ruby Mode Adjustments
;; --------------------
;; making ruby mode take effect in our odd Rails project files
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Gemfile*" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.erb\\'" . rhtml-mode) auto-mode-alist))
(add-hook 'ruby-mode-hook
     (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))) ;; hitting enter will indent.
(global-set-key (kbd "\C-c\C-c") 'comment-or-uncomment-region) ;; highlight region and comment
;;Rhtml mode for erb files
;; --------------------
;;(require 'rhtml-mode)

;;Web mode for erb files
;; --------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("erb"    . "\\.erb\\'"))
)
(setq web-mode-code-indent-offset 2)
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(local-set-key (kbd "RET") 'newline-and-indent)
(add-to-list 'web-mode-snippets '("lt" "<%= link_to " " %>"))
(add-to-list 'web-mode-snippets '("ff" "<%= form_for " " %>"))

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1fa76c8d9baaeb5dc498d2a4baaf71be6d3a8b6412af994d691fbe7a7b167321" "c0fa80157a78c95f37f16e82e0479bbd7d905eba0e5ec9c11c6669ccacaa4717" "8948ef9fc2c5c36dfa7d8871daf93c60fbfe94f3a6536d29d87caec3494d81d8" "df02e94bd77a653c40e912bc662b5882032fda12f07605f695d213647b348063" "6cf815500ed493b3a6e3cf0da10c18492182e52ba46ea763c239006998aa1fa2" "6222dae5eef5e6d0c6b2621f833e9d4cbf8be1cad9c39d31785eb65004c63867" "cefe2a6c2f261051fbf3e4c47635297162d406d6063e58f6d8a1c93b6592d2f7" "60a122f20208b8cc2bec16d13db19acd73cc537e39e4efbfc1c7f8ac9a5fe83d" "8b62bc1784de8006f0e9209b430b1306a6584cc97e19fc601b2ee7674ea5164e" "332753e523beea889f24b46137e2797aafae36b5d29503a7eb8cf21ea4c810d0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
