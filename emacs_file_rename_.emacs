;; EMACS config file
;; Should contain everything a brother needs to get his emacs on
;; anything more is beyond what is needed and you are greedy.
;; -----------------------------

;; General Config 
;; ---------------

(setq auto-save-default nil) ; turns off that blasted auto-save shit

;; ECB Dependencies and ECB : this gives you the textmate side bar thing
;; ------------------------
(load-file "~/.emacs.d/cedet-1.0pre7/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(add-to-list 'load-path
                     "~/.emacs.d/ecb-2.40") ; Enable the ECB i guess
(require 'ecb)							 ; Enable the ECB i guess
(setq ecb-auto-activate t) ; Sets ecb to appear all the time

;; Window Numbering - Used with M + window# e.g M+1 goes to window labeled w-0
;; -----------------
(add-to-list 'load-path "~/.emacs.d/windows-numbering") 
(require 'window-numbering)
(window-numbering-mode 1)

;; Textmate - supposed to add something cool, forget what
;; -----------------
(load-file "~/.emacs.d/textmate/textmate.el") ; textmate stuff i hear
(require 'textmate-mode)
(defun textmate-mode-enable-hook ()
(textmate-mode t))
(add-hook 'ruby-mode-hook 'textmate-mode-enable-hook)
(add-hook 'emacs-lisp-mode-hook 'textmate-mode-enable-hook) ; end of the textmate stuff

;; YASnippets
;; -----------------
(add-to-list 'load-path
              "~/.emacs.d/yasnippet/yasnippet-0.6.1c") ; textmate snippets supposedly
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/yasnippet-0.6.1c/snippets")

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)


;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(require 'rinari)
