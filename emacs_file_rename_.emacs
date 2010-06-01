(setq auto-save-default nil) ; turns off that blasted auto-save shit
(load-file "~/.emacs.d/cedet-1.0pre7/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(add-to-list 'load-path
                     "~/.emacs.d/ecb-2.40") ; Enable the ECB i guess
(require 'ecb)							 ; Enable the ECB i guess

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-source-path (quote (("/Users/nerb/current_project/wigify" "wigify") ("/Users/nerb/current_project/notiprice" "notiprice")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "~/.emacs.d/windows-numbering") ; supposed to be working with meta + window number, but ain't
(require 'window-numbering)
(window-numbering-mode 1)
(load-file "~/.emacs.d/textmate/textmate.el") ; textmate stuff i hear
(require 'textmate-mode)
(defun textmate-mode-enable-hook ()
(textmate-mode t))
(add-hook 'ruby-mode-hook 'textmate-mode-enable-hook)
(add-hook 'emacs-lisp-mode-hook 'textmate-mode-enable-hook) ; end of the textmate stuff

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
