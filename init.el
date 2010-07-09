;; EMACS config file
;; Should contain everything a brother needs to get his emacs on
;; anything more is beyond what is needed and you are greedy.
;; -----------------------------

;; General Config 
;; ---------------

(setq auto-save-default nil) ; turns off that blasted auto-save shit
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 170))
(require 'ido)
(ido-mode t)
(global-linum-mode 1)
(delete-selection-mode t)
(blink-cursor-mode t)
;; making ruby mode take effect in rake files
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
;; Show syntax highlighting when in ruby mode
(add-hook 'ruby-mode-hook '(lambda () (font-lock-mode 1)))

;; Color-Theme setup
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
(add-to-list 'color-themes '(color-theme-red-knight \"red-knight\" \"Philip Ingram\"))
(color-theme-red-knight)

;; ECB Dependencies and ECB : this gives you the textmate side bar thing
;; ------------------------
;;(load-file "~/.emacs.d/cedet-1.0pre7/common/cedet.el")
;;(global-ede-mode 1)                      ; Enable the Project management system
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu
(add-to-list 'load-path "~/.emacs.d/ecb") ; Add ECB to load path
(require 'ecb)							 ; require the ECB

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.21693121693121692 . 0.2909090909090909) (ecb-sources-buffer-name 0.21693121693121692 . 0.23636363636363636) (ecb-methods-buffer-name 0.21693121693121692 . 0.2727272727272727) (ecb-history-buffer-name 0.21693121693121692 . 0.18181818181818182)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default))))))
(setq ecb-auto-activate t) ; Sets ecb to appear all the time

;; Window Numbering - Used with M + window# e.g M+1 goes to window labeled w-0
;; -----------------
(add-to-list 'load-path "~/.emacs.d/windows-numbering") 
(require 'window-numbering)
(window-numbering-mode 1)

;; Textmate ; again
;; -----------------
(add-to-list 'load-path "~/.emacs.d/textmate")
(require 'textmate)
(tm/initialize)
;; YASnippets
;; -----------------
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs (list
                       ;; this first entry is where snippets tweaked
                       ;; to your personal prefs go, can/should start
                       ;; out empty
                       "~/.emacs.d/snippets/"
                       ;; the imported library snippets
                       "~/.emacs.d/yasnippet/extras/imported/"
                       ;; the standard library snippets (hopefully
                       ;; soon to be deprecated) in favor of full
                       ;; importation))
                       "~/.emacs.d/yasnippet/snippets"))
;; turn on yasnippet everywhere applicable
(yas/global-mode 1)
;; I even think you don't need this but leave it for good measure and
;; for reevaluation
(yas/reload-all)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)

;;; rhtml mode - used with rinari
;;(add-to-list 'load-path "~/.emacs.d/rhtml-mode")
;;(require 'rhtml-mode)

;; ERC
(add-to-list 'load-path "~/.emacs.d/erc")
(require 'erc)

;; Twittering Mode a Twitter client in your fricking editor!
(add-to-list 'load-path "~/.emacs.d/twittering-mode") ;; if you need
(require 'twittering-mode)
(setq twittering-username "namegoeshere")

;; Revive
(add-to-list 'load-path "~/.emacs.d/revive")
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

;; RVM in emacs
(add-to-list 'load-path "~/.emacs.d/rvm")
(require 'rvm)
(rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

;; Move Line Region
(add-to-list 'load-path "~/.emacs.d/move-line-region")
(require 'move-line-region)
