(eval-when-compile
  (require 'color-theme))

(defun color-theme-red-knight ()
  "Red Based Theme brought to you by Reademacs."
  (interactive)
  (color-theme-install
   '(color-theme-example
     ((foreground-color . "goldenrod1")
      (background-color . "#60181F")
      (background-mode . dark))
     (default ((t (nil))))
     (region ((t (:foreground "wheat" :background "chocolate4"))))
     (underline ((t (:foreground "yellow" :underline t))))
     (modeline ((t (:foreground "firebrick4" :background "wheat"))))
     (modeline-buffer-id ((t (:foreground "DarkGoldenrod4" :background "wheat"))))
     (modeline-mousable ((t (:foreground "dark cyan" :background "wheat"))))
     (modeline-mousable-minor-mode ((t (:foreground "dark cyan" :background "wheat"))))
     (italic ((t (:foreground "BlanchedAlmond" :italic t))))
     (bold-italic ((t (:foreground "BlanchedAlmond" :bold t :italic t))))
     (ecb-default-highlight-face ((t (:background "firebrick3"))))
     (font-lock-builtin-face ((t (:foreground "red3"))))
     (font-lock-doc-string-face ((t (:foreground "turquoise"))))
     (font-lock-keyword-face ((t (:foreground "chartreuse1"))))
     (font-lock-preprocessor-face ((t (:foreground "green3"))))
     (font-lock-reference-face ((t (:foreground "chartreuse1"))))
     (font-lock-type-face ((t (:foreground "CornflowerBlue"))))
     (font-lock-variable-name-face ((t (:foreground "SandyBrown"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red"))))
     (font-lock-comment-face ((t (:foreground "wheat" :background "firebrick3"))))
     (font-lock-function-name-face ((t (:foreground "LemonChiffon"))))
     (font-lock-constant-face((t (:foreground "DarkOrange1"))))
     (font-lock-string-face((t (:foreground "MintCream"))))
     (bold ((t (:bold)))))))
     ;;#AC1A28
;;(font-lock-builtin-face			 ((t (:foreground ""))))
;;(font-lock-comment-delimiter-face	 ((t (:foreground ""))))

;;(font-lock-doc-face			 ((t (:foreground ""))))
;;(font-lock-keyword-face			 ((t (:foreground ""))))
;;(font-lock-negation-char-face		 ((t (:foreground ""))))
;;(font-lock-preprocessor-face		 ((t (:foreground ""))))
;;(font-lock-regexp-grouping-backslash	 ((t (:foreground ""))))
;;(font-lock-regexp-grouping-construct	 ((t (:foreground ""))))

;;(font-lock-type-face			 ((t (:foreground ""))))
;;(font-lock-variable-name-face		 ((t (:foreground ""))))
;;(font-lock-warning-face			 ((t (:foreground ""))))
	    
