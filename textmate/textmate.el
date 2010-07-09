;; textmate.el --- TextMate behaviour in Emacs
;; Copyright 2009 Alex Duller

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;;

;;; Commentary:

;;
;; Basic steps to setup:
;;   1. Place `textmate.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'textmate)
;;        (tm/initialize)
;;
;; You can file issues, send comments and get the latest version at:
;; http://code.google.com/p/emacs-textmate/
;;
;; Contributions welcome!

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup textmate ()
  "Textmate minor mode"
  :group 'editor)

(setq skeleton-pair-alist
      '((?\( _ ?\))
	(?[  _ ?])
	(?{  _ ?})
	(?\" _ ?\")
        (?\' _ ?\')))

(defcustom tm/non-insert-alist '((emacs-lisp-mode . '(?\'))
                                 (lisp-mode . '(?\'))
                                 (lisp-interaction-mode . '(?\')))
  "The format of this list is '(major-mode . '(chars)) where the given list of
chars are not auto-inserted in major-mode"
  :type '(alist :key-type symbol :value-type alist)
  :group 'textmate)

(defcustom tm/use-goto-line nil
  "If set to t, use M-l to go to line"
  :type 'boolean
  :group 'textmate)

(defcustom tm/backspace-delete-column nil
  "If set to t, backspace will delete a block os spaces based on tab-width"
  :type 'boolean
  :group 'textmate)

(defcustom tm/use-open-next-line t
  "If set to t, use M-\r to start a new line"
  :type 'boolean
  :group 'textmate)

(defcustom tm/dont-activate nil
  "If set to t, don't activate tm/minor-mode automatically."
  :type 'boolean
  :group 'textmate)
(make-variable-buffer-local 'tm/dont-activate)

(defun tm/initialize ()
  "Do the necessary initialization"
  (setq skeleton-pair t)
  (tm/set-keymap)
  (add-hook 'after-change-major-mode-hook
            'tm/minor-mode-auto-on))

(defun tm/minor-mode-auto-on ()
  "Turn on TM minor mode unless `tm/dont-activate' is set to t."
  (unless tm/dont-activate
    (tm/minor-mode-on)))

(defun tm/minor-mode-on ()
  (interactive)
  (tm/minor-mode 1))

(defun tm/minor-mode-off ()
  (interactive)
  (tm/minor-mode nil))

(defvar tm/minor-mode-map (make-sparse-keymap)
  "Keymap for tm/minor-mode bindings")

(defun tm/set-keymap ()
  "Automatically determine the appropriate key bindings"
  (define-key tm/minor-mode-map [backspace] 'tm/pair-backspace)
	(define-key tm/minor-mode-map [return] 'reindent-then-newline-and-indent)
  (dolist (arg skeleton-pair-alist)
    (define-key tm/minor-mode-map (string (car arg)) 'tm/pair-insert)
    (define-key tm/minor-mode-map (string (car (last arg))) 'tm/pair-insert))
  (tm/goto-line)
  (tm/open-next-line-binding)
  (add-to-list 'minor-mode-map-alist (cons 'tm/minor-mode tm/minor-mode-map)))

(define-minor-mode tm/minor-mode
  "Toggle Textmate mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " TM"
  ;; The minor mode bindings.
  :group 'textmate)

(defun tm/goto-line ()
  "Enable users to decide whether or not to use M-l as goto-line"
  (let ((tm/goto-line-map (make-sparse-keymap)))
    (define-key tm/goto-line-map "\M-l" 'goto-line)
    (add-to-list 'minor-mode-map-alist
		 (cons 'tm/use-goto-line tm/goto-line-map))))

(defun tm/open-next-line-binding ()
  "Enable users to decide whether or not to use M-\r to start a new line"
  (let ((tm/open-next-line-map (make-sparse-keymap)))
    (define-key tm/open-next-line-map "\M-\r" 'tm/open-next-line)
    (add-to-list 'minor-mode-map-alist
                 (cons 'tm/use-open-next-line tm/open-next-line-map))))

(defun tm/open-next-line()
  "Function to open and goto indented next line"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; The following set of functions are adapted from
;; http://www.emacswiki.org/emacs/AutoPairs#toc2
(defun tm/pair-insert (arg)
  (interactive "P")
  (let ((ignore-list (car (last (assoc major-mode tm/non-insert-alist))))
        (keys (recent-keys)))
    (cond
     ((member last-command-event ignore-list)
      (insert-char last-command-event 1))
     ((assq last-command-event skeleton-pair-alist)
      (tm/pair-open arg))
     ((assq (elt keys (- (length keys) 2)) skeleton-pair-alist)
      (forward-char))
     (t
      (tm/pair-close arg)))
    (indent-according-to-mode)))

(defun tm/pair-open (arg)
  (interactive "P")
  (let ((pair (assq last-command-event
		    skeleton-pair-alist)))
    (cond
     ((and (not mark-active)
	   (eq (car pair) (car (last pair)))
	   (eq (car pair) (char-after)))
      (tm/pair-close arg))
     (t
      (skeleton-pair-insert-maybe arg)))))

(defun tm/pair-close (arg)
  (interactive "P")
  (cond
   (mark-active
    (let (pair open)
      (dolist (pair skeleton-pair-alist)
	(when (eq last-command-event (car (last pair)))
	  (setq open (car pair))))
      (setq last-command-event open)
      (skeleton-pair-insert-maybe arg)))
   (t
    (self-insert-command (prefix-numeric-value arg))
    (indent-according-to-mode))))

(defun tm/pair-backspace (arg)
  (interactive "p")
  (if (eq (char-after)
	  (car (last (assq (char-before) skeleton-pair-alist))))
      (and (char-after) (delete-char 1)))
  (if (eq tm/backspace-delete-column t)
      (tm/backward-delete-whitespace-to-column)
    (delete-backward-char 1)))

;; Thanks to Trey Jackson
;; http://stackoverflow.com/questions/1450169/how-do-i-emulate-vims-softtabstop-in-emacs/1450454#1450454
(defun tm/backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
	  (call-interactively 'backward-delete-char-untabify))))))

(provide 'textmate)