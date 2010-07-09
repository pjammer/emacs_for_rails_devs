;;; yasnippet-bundle.el --- Yet another snippet extension (Auto compiled bundle)
;;; Yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;;           2009 pluskid, joaotavora

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.7.0
;; Package-version: 0.7.0
;; X-URL: http://code.google.com/p/yasnippet/
;; Keywords: convenience, emulation
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;
;;   1. In your .emacs file:
;;        (add-to-list 'load-path "/dir/to/yasnippet.el")
;;        (require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;        (setq yas/snippet-dirs "~/.emacs/snippets")
;;        (yas/load-directory yas/snippet-dirs)
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
;;
;;   Interesting variables are:
;;
;;       `yas/snippet-dirs'
;;
;;           The directory where user-created snippets are to be
;;           stored. Can also be a list of directories. In that case,
;;           when used for bulk (re)loading of snippets (at startup or
;;           via `yas/reload-all'), directories appearing earlier in
;;           the list shadow other dir's snippets. Also, the first
;;           directory is taken as the default for storing the user's
;;           new snippets.
;;
;;           The deprecated `yas/root-directory' aliases this variable
;;           for backward-compatibility.
;;
;;       `yas/extra-modes'
;;
;;           A local variable that you can set in a hook to override
;;           snippet-lookup based on major mode. It is a a symbol (or
;;           list of symbols) that correspond to subdirectories of
;;           `yas/snippet-dirs' and is used for deciding which
;;           snippets to consider for the active buffer.
;;
;;           Deprecated `yas/mode-symbol' aliases this variable for
;;           backward-compatibility.
;;
;;   Major commands are:
;;
;;       M-x yas/expand
;;
;;           Try to expand snippets before point.  In `yas/minor-mode',
;;           this is bound to `yas/trigger-key' which you can customize.
;;
;;       M-x yas/load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas/insert-snippet
;;
;;           Prompts you for possible snippet expansion if that is
;;           possible according to buffer-local and snippet-local
;;           expansion conditions.  With prefix argument, ignore these
;;           conditions.
;;
;;       M-x yas/find-snippets
;;
;;           Lets you find the snippet files in the correct
;;           subdirectory of `yas/snippet-dirs', according to the
;;           active major mode (if it exists) like
;;           `find-file-other-window'.
;;
;;       M-x yas/visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas/insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas/new-snippet
;;
;;           Lets you create a new snippet file in the correct
;;           subdirectory of `yas/snippet-dirs', according to the
;;           active major mode.
;;
;;       M-x yas/load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas/tryout-snippet
;;
;;           When editing a snippet, this opens a new empty buffer,
;;           sets it to the appropriate major mode and inserts the
;;           snippet there, so you can see what it looks like.  This is
;;           bound to "C-c C-t" while in `snippet-mode'.
;;
;;       M-x yas/describe-tables
;;
;;           Lists known snippets in a separate buffer. User is
;;           prompted as to whether only the currently active tables
;;           are to be displayed, or all the tables for all major
;;           modes.
;;
;;   The `dropdown-list.el' extension is bundled with YASnippet, you
;;   can optionally use it the preferred "prompting method", puting in
;;   your .emacs file, for example:
;;
;;       (require 'dropdown-list)
;;       (setq yas/prompt-functions '(yas/dropdown-prompt
;;                                    yas/ido-prompt
;;                                    yas/completing-prompt))
;;
;;   Also check out the customization group
;;
;;        M-x customize-group RET yasnippet RET
;;
;;   If you use the customization group to set variables
;;   `yas/snippet-dirs' or `yas/global-mode', make sure the path to
;;   "yasnippet.el" is present in the `load-path' *before* the
;;   `custom-set-variables' is executed in your .emacs file.
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(require 'cl)
(require 'assoc)
(require 'easymenu)
(require 'help-mode)


;;; User customizable variables

(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

;;;###autoload
(defcustom yas/snippet-dirs nil
  "Directory or list of snippet dirs for each major mode.

The directory where user-created snippets are to be stored. Can
also be a list of directories. In that case, when used for
bulk (re)loading of snippets (at startup or via
`yas/reload-all'), directories appearing earlier in the list
shadow other dir's snippets. Also, the first directory is taken
as the default for storing the user's new snippets."
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'yasnippet
  :require 'yasnippet
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             (unless (or (not (fboundp 'yas/reload-all))
                         (equal old new))
               (yas/reload-all)))))
(defun yas/snippet-dirs ()
  (if (listp yas/snippet-dirs) yas/snippet-dirs (list yas/snippet-dirs)))
(defvaralias 'yas/root-directory 'yas/snippet-dirs)

(defcustom yas/prompt-functions '(yas/x-prompt
                                  yas/dropdown-prompt
                                  yas/completing-prompt
                                  yas/ido-prompt
                                  yas/no-prompt)
  "Functions to prompt for keys, templates, etc interactively.

These functions are called with the following arguments:

- PROMPT: A string to prompt the user

- CHOICES: a list of strings or objects.

- optional DISPLAY-FN : A function that, when applied to each of
the objects in CHOICES will return a string.

The return value of any function you put here should be one of
the objects in CHOICES, properly formatted with DISPLAY-FN (if
that is passed).

- To signal that your particular style of prompting is
unavailable at the moment, you can also have the function return
nil.

- To signal that the user quit the prompting process, you can
signal `quit' with

  (signal 'quit \"user quit!\")."
  :type '(repeat function)
  :group 'yasnippet)

(defcustom yas/indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

- `fixed' Indent the snippet to the current column;

- `auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indendation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto))
  :group 'yasnippet)

(defcustom yas/also-auto-indent-first-line nil
  "Non-nil means also auto indent first line according to mode.

Naturally this is only valid when `yas/indent-line' is `auto'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/snippet-revival t
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key bound to `yas/expand' when function `yas/minor-mode' is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet
  :set #'(lambda (symbol key)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol key)
             ;; On very first loading of this defcustom,
             ;; `yas/trigger-key' is *not* loaded.
             (if (fboundp 'yas/trigger-key-reload)
                 (yas/trigger-key-reload old)))))

(defcustom yas/next-field-key '("TAB" "<tab>")
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))


(defcustom yas/prev-field-key '("<backtab>" "<S-tab>")
  "The key to navigate to previous field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/skip-and-clear-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/triggers-in-field nil
  "If non-nil, `yas/next-field-key' can trigger stacked expansions.

Otherwise, `yas/next-field-key' just tries to move on to the next
field"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/fallback-behavior 'call-other-command
  "How to act when `yas/trigger-key' does *not* expand a snippet.

- `call-other-command' means try to temporarily disable YASnippet
    and call the next command bound to `yas/trigger-key'.

- nil or the symbol `return-nil' mean do nothing. (and
  `yas/expand-returns' nil)

- A lisp form (apply COMMAND . ARGS) means interactively call
  COMMAND, if ARGS is non-nil, call COMMAND non-interactively
  with ARGS as arguments."
  :type '(choice (const :tag "Call previous command"  call-other-command)
                 (const :tag "Do nothing"             return-nil))
  :group 'yasnippet)

(defcustom yas/choose-keys-first nil
  "If non-nil, prompt for snippet key first, then for template.

Otherwise prompts for all possible snippet names.

This affects `yas/insert-snippet' and `yas/visit-snippet-file'."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/choose-tables-first nil
  "If non-nil, and multiple eligible snippet tables, prompts user for tables first.

Otherwise, user chooses between the merging together of all
eligible tables.

This affects `yas/insert-snippet', `yas/visit-snippet-file'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu 'abbreviate
  "Display a YASnippet menu in the menu bar.

When non-nil, submenus for each snippet table will be listed
under the menu \"Yasnippet\".

- If set to `real-modes' only submenus whose name more or less
corresponds to a major mode are listed.

- If set to `abbreviate', only the current major-mode
menu and the modes set in `yas/extra-modes' are listed.

Any other non-nil value, every submenu is listed."
  :type '(choice (const :tag "Full"  t)
                 (const :tag "Real modes only" real-modes)
                 (const :tag "Abbreviate" abbreviate))
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/wrap-around-region nil
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker.  This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace t
  "If non-nil, don't raise errors in inline elisp evaluation.

An error string \"[yas] error\" is returned instead."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/ignore-filenames-as-triggers nil
  "If non-nil, don't derive tab triggers from filenames.

This means a snippet without a \"# key:'\ directive won't have a
tab trigger."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/visit-from-menu nil
  "If non-nil visit snippets's files from menu, instead of expanding them.

This cafn only work when snippets are loaded from files."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/expand-only-for-last-commands nil
  "List of `last-command' values to restrict tab-triggering to, or nil.

Leave this set at nil (the default) to be able to trigger an
expansion simply by placing the cursor after a valid tab trigger,
using whichever commands.

Optionallly, set this to something like '(self-insert-command) if
you to wish restrict expansion to only happen when the last
letter of the snippet tab trigger was typed immediately before
the trigger key itself."
  :type '(repeat function)
  :group 'yasnippet)

;; Only two faces, and one of them shouldn't even be used...
;;
(defface yas/field-highlight-face
  '((t (:inherit 'region)))
  "The face used to highlight the currently active field of a snippet"
  :group 'yasnippet)

(defface yas/field-debug-face
  '()
  "The face used for debugging some overlays normally hidden"
  :group 'yasnippet)


;;; User can also customize the next defvars
(defun yas/define-some-keys (keys keymap definition)
  "Bind KEYS to DEFINITION in KEYMAP, read with `read-kbd-macro'."
  (let ((keys (or (and (listp keys) keys)
                  (list keys))))
    (dolist (key keys)
      (define-key keymap (read-kbd-macro key) definition))))

(defvar yas/keymap
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (yas/define-some-keys (car binding) map (cdr binding)))
          `((,yas/next-field-key     . yas/next-field-or-maybe-expand)
            (,yas/prev-field-key     . yas/prev-field)
            ("C-g"                   . yas/abort-snippet)
            (,yas/skip-and-clear-key . yas/skip-and-clear-or-delete-char)))
    map)
  "The keymap active while a snippet expansion is in progress.")

(defvar yas/key-syntaxes (list "w" "w_" "w_.()" "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.

The hooks will be run in an environment where some variables bound to
proper values:

`yas/snippet-beg' : The beginning of the region of the snippet.

`yas/snippet-end' : Similar to beg.

Attention: These hooks are not run when exiting nested/stackd snippet expansion!")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run just before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (or (fourth (syntax-ppss))
                (fifth (syntax-ppss)))
            (eq (symbol-function this-command) 'yas/expand-from-trigger-key))
       '(require-snippet-condition . force-in-comment)
     t)
  "Snippet expanding condition.

This variable is a lisp form:

    * If it evaluates to nil, no snippets can be expanded.

    * If it evaluates to the a cons (require-snippet-condition
      . REQUIREMENT)

       * Snippets bearing no \"# condition:\" directive are not
         considered

       * Snippets bearing conditions that evaluate to nil (or
         produce an error) won't be onsidered.

       * If the snippet has a condition that evaluates to non-nil
         RESULT:

          * If REQUIREMENT is t, the snippet is considered

          * If REQUIREMENT is `eq' RESULT, the snippet is
            considered

          * Otherwise, the snippet is not considered.

    * If it evaluates to the symbol 'always, all snippets are
      considered for expansion, regardless of any conditions.

    * If it evaluates to t or some other non-nil value

       * Snippet bearing no conditions, or conditions that
         evaluate to non-nil, are considered for expansion.

       * Otherwise, the snippet is not considered.

Here's an example preventing snippets from being expanded from
inside comments, in `python-mode' only, with the exception of
snippets returning the symbol 'force-in-comment in their
conditions.

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))

The default value is similar, it filters out potential snippet
expansions inside comments and string literals, unless the
snippet itself contains a condition that returns the symbol
`force-in-comment'.")


;;; Internal variables

(defvar yas/version "0.7.0")

(defvar yas/menu-table (make-hash-table)
  "A hash table of MAJOR-MODE symbols to menu keymaps.")

(defun teste ()
  (interactive)
  (message "AHAHA!"))

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} ?\( ?\))
  "List of characters which *might* need to be escaped.")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`lisp-expression`\" expression." )

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([ \t\n]*([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))


;;; Minor mode stuff

;; XXX: `last-buffer-undo-list' is somehow needed in Carbon Emacs for MacOSX
(defvar last-buffer-undo-list nil)

(defvar yas/minor-mode-menu nil
  "Holds the YASnippet menu")

(defun yas/init-minor-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define yas/minor-mode-menu
      map
      "Menu used when YAS/minor-mode is active."
      '("YASnippet"
        "----"
        ["Expand trigger" yas/expand
         :help "Possibly expand tab trigger before point"]
        ["Insert at point..." yas/insert-snippet
         :help "Prompt for an expandable snippet and expand it at point"]
        ["New snippet..." yas/new-snippet
         :help "Create a new snippet in an appropriate directory"]
        ["Visit snippet file..." yas/visit-snippet-file
         :help "Prompt for an expandable snippet and find its file"]
        ["Find snippets..." yas/find-snippets
         :help "Invoke `find-file' in the appropriate snippet directory"]
        "----"
        ("Snippet menu behaviour"
         ["Visit snippets" (setq yas/visit-from-menu t)
          :help "Visit snippets from the menu"
          :active t :style radio   :selected yas/visit-from-menu]
         ["Expand snippets" (setq yas/visit-from-menu nil)
          :help "Expand snippets from the menu"
          :active t :style radio :selected (not yas/visit-from-menu)]
         "----"
         ["Show \"Real\" modes only" (setq yas/use-menu 'real-modes)
          :help "Show snippet submenus for modes that appear to be real major modes"
          :active t :style radio   :selected (eq yas/use-menu 'real-modes)]
         ["Show all modes" (setq yas/use-menu 't)
          :help "Show one snippet submenu for each loaded table"
          :active t :style radio   :selected (eq yas/use-menu 't)]
         ["Abbreviate according to current mode" (setq yas/use-menu 'abbreviate)
          :help "Show only snippet submenus for the current active modes"
          :active t :style radio   :selected (eq yas/use-menu 'abbreviate)])
        ("Indenting"
         ["Auto" (setq yas/indent-line 'auto)
          :help "Indent each line of the snippet with `indent-according-to-mode'"
          :active t :style radio   :selected (eq yas/indent-line 'auto)]
         ["Fixed" (setq yas/indent-line 'fixed)
          :help "Indent the snippet to the current column"
          :active t :style radio   :selected (eq yas/indent-line 'fixed)]
         ["None" (setq yas/indent-line 'none)
          :help "Don't apply any particular snippet indentation after expansion"
          :active t :style radio   :selected (not (member yas/indent-line '(fixed auto)))]
         "----"
         ["Also auto indent first line" (setq yas/also-auto-indent-first-line
                                              (not yas/also-auto-indent-first-line))
          :help "When auto-indenting also, auto indent the first line menu"
          :active (eq yas/indent-line 'auto)
          :style toggle :selected yas/also-auto-indent-first-line]
         )
        ("Prompting method"
         ["System X-widget" (setq yas/prompt-functions
                                  (cons 'yas/x-prompt
                                        (remove 'yas/x-prompt
                                                yas/prompt-functions)))
          :help "Use your windowing system's (gtk, mac, windows, etc...) default menu"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/x-prompt)]
         ["Dropdown-list" (setq yas/prompt-functions
                                (cons 'yas/dropdown-prompt
                                      (remove 'yas/dropdown-prompt
                                              yas/prompt-functions)))
          :help "Use a special dropdown list"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/dropdown-prompt)]
         ["Ido" (setq yas/prompt-functions
                      (cons 'yas/ido-prompt
                            (remove 'yas/ido-prompt
                                    yas/prompt-functions)))
          :help "Use an ido-style minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/ido-prompt)]
         ["Completing read" (setq yas/prompt-functions
                                  (cons 'yas/completing-prompt
                                        (remove 'yas/completing-prompt-prompt
                                                yas/prompt-functions)))
          :help "Use a normal minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/completing-prompt-prompt)]
         )
        ("Misc"
         ["Wrap region in exit marker"
          (setq yas/wrap-around-region
                (not yas/wrap-around-region))
          :help "If non-nil automatically wrap the selected text in the $0 snippet exit"
          :style toggle :selected yas/wrap-around-region]
         ["Allow stacked expansions "
          (setq yas/triggers-in-field
                (not yas/triggers-in-field))
          :help "If non-nil allow snippets to be triggered inside other snippet fields"
          :style toggle :selected yas/triggers-in-field]
         ["Revive snippets on undo "
          (setq yas/snippet-revival
                (not yas/snippet-revival))
          :help "If non-nil allow snippets to become active again after undo"
          :style toggle :selected yas/snippet-revival]
         ["Good grace "
          (setq yas/good-grace
                (not yas/good-grace))
          :help "If non-nil don't raise errors in bad embedded eslip in snippets"
          :style toggle :selected yas/good-grace]
         ["Ignore filenames as triggers"
          (setq yas/ignore-filenames-as-triggers
                (not yas/ignore-filenames-as-triggers))
          :help "If non-nil don't derive tab triggers from filenames"
          :style toggle :selected yas/ignore-filenames-as-triggers]
         )
        "----"
        ["Load snippets..."  yas/load-directory
         :help "Load snippets from a specific directory"]
        ["Reload everything" yas/reload-all
         :help "Cleanup stuff, reload snippets, rebuild menus"]
        ["About"            yas/about
         :help "Display some information about YASsnippet"]))
    ;; Now for the stuff that has direct keybindings
    ;;
    (define-key map "\C-c&\C-s" 'yas/insert-snippet)
    (define-key map "\C-c&\C-n" 'yas/new-snippet)
    (define-key map "\C-c&\C-v" 'yas/visit-snippet-file)
    (define-key map "\C-c&\C-f" 'yas/find-snippets)
    map))

(defvar yas/minor-mode-map (yas/init-minor-keymap)
  "The keymap used when `yas/minor-mode' is active.")

(defun yas/trigger-key-reload (&optional unbind-key)
  "Rebind `yas/expand' to the new value of `yas/trigger-key'.

With optional UNBIND-KEY, try to unbind that key from
`yas/minor-mode-map'."
  (when (and unbind-key
             (stringp unbind-key)
             (not (string= unbind-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro unbind-key) nil))
  (when  (and yas/trigger-key
              (stringp yas/trigger-key)
              (not (string= yas/trigger-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro yas/trigger-key) 'yas/expand)))

(defvar yas/tables (make-hash-table)
  "A hash table of MAJOR-MODE symbols to `yas/table' objects.")

(defvar yas/direct-keymaps (list)
  "Keymap alist supporting direct snippet keybindings.

This variable is is placed `emulation-mode-map-alists'.

Its elements looks like (TABLE-NAME . KEYMAP) and are
calculated when loading snippets. TABLE-NAME is a variable
set buffer-locally when entering `yas/minor-mode'. KEYMAP binds
all defined direct keybindings to the command
`yas/expand-from-keymap', which acts similarly to `yas/expand'")

(defun yas/direct-keymaps-reload ()
  "Force reload the direct keybinding for active snippet tables."
  (interactive)
  (setq yas/direct-keymaps nil)
  (maphash #'(lambda (name table)
               (mapc #'(lambda (table)
                         (push (cons (intern (format "yas//direct-%s" name))
                                     (yas/table-direct-keymap table))
                               yas/direct-keymaps))
                     (cons table (yas/table-get-all-parents table))))
           yas/tables))

(defun yas/direct-keymaps-set-vars ()
  (let ((modes-to-activate (list major-mode))
        (mode major-mode))
    (while (setq mode (get mode 'derived-mode-parent))
      (push mode modes-to-activate))
    (dolist (mode (yas/extra-modes))
      (push mode modes-to-activate))
    (dolist (mode modes-to-activate)
      (let ((name (intern (format "yas//direct-%s" mode))))
        (set-default name nil)
        (set (make-local-variable name) t)))))

(defvar yas/minor-mode-hook nil
  "Hook run when yas/minor-mode is turned on")

;;;###autoload
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}"
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet
  (cond (yas/minor-mode
         ;; Reload the trigger key
         ;;
         (yas/trigger-key-reload)
         ;; Load all snippets definitions unless we still don't have a
         ;; root-directory or some snippets have already been loaded.
         ;;
         (unless (or (null yas/snippet-dirs)
                     (> (hash-table-count yas/tables) 0))
           (yas/reload-all))
         ;; Install the direct keymaps in `emulation-mode-map-alists'
         ;; (we use `add-hook' even though it's not technically a hook,
         ;; but it works). Then define variables named after modes to
         ;; index `yas/direct-keymaps'.
         ;;
         (add-hook 'emulation-mode-map-alists 'yas/direct-keymaps)
         (add-hook 'yas/minor-mode-hook 'yas/direct-keymaps-set-vars-runonce 'append))
        (t
         ;; Uninstall the direct keymaps.
         ;;
         (remove-hook 'emulation-mode-map-alists 'yas/direct-keymaps))))

(defun yas/direct-keymaps-set-vars-runonce ()
  (yas/direct-keymaps-set-vars)
  (remove-hook 'yas/minor-mode-hook 'yas/direct-keymaps-set-vars-runonce))

(defvar yas/dont-activate #'(lambda ()
                              (and yas/snippet-dirs
                                   (null (yas/get-snippet-tables))))
  "If non-nil don't let `yas/minor-mode-on' active yas for this buffer.

`yas/minor-mode-on' is usually called by `yas/global-mode' so
this effectively lets you define exceptions to the \"global\"
behaviour.")
(make-variable-buffer-local 'yas/dont-activate)

(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode.

Do this unless `yas/dont-activate' is t or the function
`yas/get-snippet-tables' (which see), returns an empty list."
  (interactive)
  (unless (or (and (functionp yas/dont-activate)
                   (funcall yas/dont-activate))
              (and (not (functionp yas/dont-activate))
                   yas/dont-activate))
    (yas/minor-mode 1)))

(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))

(define-globalized-minor-mode yas/global-mode yas/minor-mode yas/minor-mode-on
  :group 'yasnippet
  :require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode stuff
;;
(defvar yas/font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
          lisp-font-lock-keywords
          lisp-font-lock-keywords-1
          lisp-font-lock-keywords-2
          '(("$\\([0-9]+\\)"
             (0 font-lock-keyword-face)
             (1 font-lock-string-face t))
            ("${\\([0-9]+\\):?"
             (0 font-lock-keyword-face)
             (1 font-lock-warning-face t))
            ("${" font-lock-keyword-face)
            ("$[0-9]+?" font-lock-preprocessor-face)
            ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
            ("}"
             (0 font-lock-keyword-face)))))

(defun yas/init-major-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil
      map
      "Menu used when snippet-mode is active."
      (cons "Snippet"
            (mapcar #'(lambda (ent)
                        (when (third ent)
                          (define-key map (third ent) (second ent)))
                        (vector (first ent) (second ent) t))
                    (list
                     (list "Load this snippet" 'yas/load-snippet-buffer "\C-c\C-c")
                     (list "Try out this snippet" 'yas/tryout-snippet "\C-c\C-t")))))
    map))

(defvar snippet-mode-map
  (yas/init-major-keymap)
  "The keymap used when `snippet-mode' is active")


(define-derived-mode snippet-mode text-mode "Snippet"
  "A mode for editing yasnippets"
  (set-syntax-table (standard-syntax-table))
  (setq font-lock-defaults '(yas/font-lock-keywords))
  (set (make-local-variable 'require-final-newline) nil)
  (use-local-map snippet-mode-map))



;;; Internal structs for template management

(defstruct (yas/template (:constructor yas/make-blank-template))
  "A template for a snippet."
  table
  key
  content
  name
  condition
  expand-env
  file
  keybinding
  uuid
  menu-binding-pair
  group      ;; as dictated by the #group: directive or .yas-make-groups 
  perm-group ;; as dictated by `yas/define-menu'
  )

(defun yas/populate-template (template &rest args)
  "Helper function to populate a template with properties"
  (let (p v)
    (while args
      (aset template
            (position (intern (substring (symbol-name (car args)) 1))
                      (mapcar #'car (get 'yas/template 'cl-struct-slots)))
            (second args))
      (setq args (cddr args)))
    template))

(defstruct (yas/table (:constructor yas/make-snippet-table (name)))
  "A table to store snippets for a particular mode.

Has the following fields:

`yas/table-name'

  A symbol name normally corresponding to a major mode, but can
  also be a pseudo major-mode to be referenced in
  `yas/extra-modes', for example.

`yas/table-hash'

  A hash table (KEY . NAMEHASH), known as the \"keyhash\". KEY is
  a string or a vector, where the former is the snippet's trigger
  and the latter means it's a direct keybinding. NAMEHASH is yet
  another hash of (NAME . TEMPLATE) where NAME is the snippet's
  name and TEMPLATE is a `yas/template' object.

`yas/table-parents'

  A list of tables considered parents of this table: i.e. when
  searching for expansions they are searched as well.

`yas/table-direct-keymap'

  A keymap for the snippets in this table that have direct
  keybindings. This is kept in sync with the keyhash, i.e., all
  the elements of the keyhash that are vectors appear here as
  bindings to `yas/expand-from-keymap'.

`yas/table-uuidhash'

  A hash table mapping snippets uuid's to the same `yas/template'
  objects. A snippet uuid defaults to the snippet's name.
"
  name
  (hash (make-hash-table :test 'equal))
  (uuidhash (make-hash-table :test 'equal))
  (parents nil)
  (direct-keymap (make-sparse-keymap)))

(defun yas/get-template-by-uuid (mode uuid)
  "Find the snippet template in MODE by its UUID."
  (let* ((table (gethash mode yas/tables mode)))
    (when table
      (gethash uuid (yas/table-uuidhash table)))))

;; Apropos storing/updating, this works with two steps:
;;
;; 1. `yas/remove-template-by-uuid' to remove any existing mappings by
;;    snippet uuid
;;
;; 2. `yas/add-template' to add the mappings again:
;;
;;    Create or index the entry in TABLES's `yas/table-hash'
;;    linking KEY to a namehash. That namehash links NAME to
;;    TEMPLATE, and is also created a new namehash inside that
;;    entry.
;;
(defun yas/remove-template-by-uuid (table uuid)
  "Remove from TABLE a template identified by UUID."
  (let ((template (gethash uuid (yas/table-uuidhash table))))
    (when template
      (let* ((name                (yas/template-name template))
             (empty-keys          nil))
        ;; Remove the name from each of the targeted namehashes
        ;;
        (maphash #'(lambda (k v)
                     (let ((template (gethash name v)))
                       (when (and template
                                  (eq uuid (yas/template-uuid template)))
                         (remhash name v)
                         (when (zerop (hash-table-count v))
                           (push k empty-keys)))))
                 (yas/table-hash table))
        ;; Remove the namehashed themselves if they've become empty
        ;;
        (dolist (key empty-keys)
          (remhash key (yas/table-hash table)))

        ;; Finally, remove the uuid from the uuidhash
        ;;
        (remhash uuid (yas/table-uuidhash table))))))


(defun yas/add-template (table template)
  "Store in TABLE the snippet template TEMPLATE.

KEY can be a string (trigger key) of a vector (direct
keybinding)."
  (let ((name (yas/template-name template))
        (key (yas/template-key template))
        (keybinding (yas/template-keybinding template))
        (menu-binding (car (yas/template-menu-binding-pair template))))
    (dolist (k (remove nil (list key keybinding)))
      (puthash name
               template
               (or (gethash k
                            (yas/table-hash table))
                   (puthash k
                            (make-hash-table :test 'equal)
                            (yas/table-hash table))))
      (when (vectorp k)
        (define-key (yas/table-direct-keymap table) k 'yas/expand-from-keymap)))

    (when menu-binding
      (setf (getf (cdr menu-binding) :keys)
            (or (and keybinding (key-description keybinding))
                (and key (concat key yas/trigger-symbol))))
      (setcar (cdr menu-binding)
              name))

    (puthash (yas/template-uuid template) template (yas/table-uuidhash table))))

(defun yas/update-template (snippet-table template)
  "Add or update TEMPLATE in SNIPPET-TABLE.

Also takes care of adding and updaring to the associated menu."
  ;; Remove from table by uuid
  ;;
  (yas/remove-template-by-uuid snippet-table (yas/template-uuid template))
  ;; Add to table again
  ;;
  (yas/add-template snippet-table template)
  ;; Take care of the menu
  ;;
  (let ((keymap (yas/menu-keymap-get-create snippet-table))
        (group (yas/template-group template)))
    (when (and yas/use-menu
               keymap
               (not (cdr (yas/template-menu-binding-pair template))))
      ;; Remove from menu keymap
      ;;
      (yas/delete-from-keymap keymap (yas/template-uuid template))

      ;; Add necessary subgroups as necessary.
      ;; 
      (dolist (subgroup group)
        (let ((subgroup-keymap (lookup-key keymap (vector (make-symbol subgroup)))))
          (unless (and subgroup-keymap
                       (keymapp subgroup-keymap))
            (setq subgroup-keymap (make-sparse-keymap))
            (define-key keymap (vector (make-symbol subgroup))
              `(menu-item ,subgroup ,subgroup-keymap)))
            (setq keymap subgroup-keymap)))
      
      ;; Add this entry to the keymap
      ;; 
      (let ((menu-binding-pair (yas/snippet-menu-binding-pair-get-create template)))
        (define-key keymap (vector (make-symbol (yas/template-uuid template))) (car menu-binding-pair))))))

(defun yas/fetch (table key)
  "Fetch templates in TABLE by KEY.

Return a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas/template' structure."
  (let* ((keyhash (yas/table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas/filter-templates-by-condition
       (let (alist)
         (maphash #'(lambda (k v)
                      (push (cons k v) alist))
                  namehash)
         alist)))))


;;; Filtering/condition logic

(defun yas/eval-condition (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas] error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))


(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas/template' structure.

This function implements the rules described in
`yas/buffer-local-condition'.  See that variables documentation."
  (let ((requirement (yas/require-template-specific-condition-p)))
    (if (eq requirement 'always)
        templates
      (remove-if-not #'(lambda (pair)
                         (yas/template-can-expand-p
                          (yas/template-condition (cdr pair)) requirement))
                     templates))))

(defun yas/require-template-specific-condition-p ()
  "Decides if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas/buffer-local-condition)
      'always
    (let ((local-condition (or (and (consp yas/buffer-local-condition)
                                    (yas/eval-condition yas/buffer-local-condition))
                               yas/buffer-local-condition)))
      (when local-condition
        (if (eq local-condition t)
            t
          (and (consp local-condition)
               (eq 'require-snippet-condition (car local-condition))
               (symbolp (cdr local-condition))
               (cdr local-condition)))))))

(defun yas/template-can-expand-p (condition requirement)
  "Evaluates CONDITION and REQUIREMENT and returns a boolean"
  (let* ((result (or (null condition)
                     (yas/eval-condition condition))))
    (cond ((eq requirement t)
           result)
          (t
           (eq requirement result)))))

(defun yas/table-get-all-parents (table)
  "Returns a list of all parent tables of TABLE"
  (let ((parents (yas/table-parents table)))
    (when parents
      (append (copy-list parents)
              (mapcan #'yas/table-get-all-parents parents)))))

(defun yas/table-templates (table)
  (when table
    (let ((acc (list)))
      (maphash #'(lambda (key namehash)
                   (maphash #'(lambda (name template)
                                (push (cons name template) acc))
                            namehash))
               (yas/table-hash table))
      (yas/filter-templates-by-condition acc))))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax
        done
        templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (mapcan #'(lambda (table)
                        (yas/fetch table (buffer-substring-no-properties start end)))
                    (yas/get-snippet-tables)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))


(defun yas/table-all-keys (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
                   (when (yas/filter-templates-by-condition templates)
                     (push key acc)))
               (yas/table-hash table))
      acc)))


;;; Internal functions

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-lisp (form)
  "Evaluate FORM and convert the result to string."
  (let ((retval (catch 'yas/exception
                  (condition-case err
                      (save-excursion
                        (save-restriction
                          (save-match-data
                            (widen)
                            (let ((result (eval form)))
                              (when result
                                (format "%s" result))))))
                    (error (if yas/good-grace
                               (format "[yas] elisp error! %s" (error-message-string err))
                             (error (format "[yas] elisp error: %s"
                                            (error-message-string err)))))))))
    (when (and (consp retval)
               (eq 'yas/exception (car retval)))
      (error (cdr retval)))
    retval))

(defun yas/eval-lisp-no-saves (form)
  (condition-case err
      (eval form)
    (error (if yas/good-grace
               (format "[yas] elisp error! %s" (error-message-string err))
             (error (format "[yas] elisp error: %s"
                            (error-message-string err)))))))

(defun yas/read-lisp (string &optional nil-on-error)
  "Read STRING as a elisp expression and return it.

In case STRING in an invalid expression and NIL-ON-ERROR is nil,
return an expression that when evaluated will issue an error."
  (condition-case err
      (read string)
    (error (and (not nil-on-error)
                `(error (error-message-string err))))))

(defun yas/read-keybinding (keybinding)
  "Read KEYBINDING as a snippet keybinding, return a vector."
  (when (and keybinding
             (not (string-match "keybinding" keybinding)))
    (condition-case err
        (let ((keybinding-string (or (and (string-match "\".*\"" keybinding)
                                          (read keybinding))
                                     ;; "KEY-DESC" with quotes is deprecated..., but supported
                                     keybinding)))
          (read-kbd-macro keybinding-string 'need-vector))
      (error
       (message "[yas] warning: keybinding \"%s\" invalid since %s."
                keybinding (error-message-string err))
       nil))))

(defvar yas/extra-modes nil
  "If non-nil, also lookup snippets for this/these modes.

Can be a symbol or a list of symbols.

This variable probably makes more sense as buffer-local, so
ensure your use `make-local-variable' when you set it.")
(defun yas/extra-modes ()
  (if (listp yas/extra-modes) yas/extra-modes (list yas/extra-modes)))
(defvaralias 'yas/mode-symbol 'yas/extra-modes)

(defun yas/table-get-create (mode)
  "Get the snippet table corresponding to MODE.

Optional DIRECTORY gets recorded as the default directory to
search for snippet files if the retrieved/created table didn't
already have such a property."
  (let ((table (gethash mode
                        yas/tables)))
    (unless table
      (setq table (yas/make-snippet-table (symbol-name mode)))
      (puthash mode table yas/tables)
      (aput 'yas/direct-keymaps (intern (format "yas//direct-%s" mode))
            (yas/table-direct-keymap table)))
    table))

(defun yas/get-snippet-tables (&optional mode-symbol dont-search-parents)
  "Get snippet tables for current buffer.

Return a list of 'yas/table' objects indexed by mode.

The modes are tried in this order: optional MODE-SYMBOL, then
`yas/extra-modes', then `major-mode' then, unless
DONT-SEARCH-PARENTS is non-nil, the guessed parent mode of either
MODE-SYMBOL or `major-mode'.

Guessing is done by looking up the MODE-SYMBOL's
`derived-mode-parent' property, see also `derived-mode-p'."
  (let ((mode-tables
         (remove nil
                 (mapcar #'(lambda (mode)
                             (gethash mode yas/tables))
                 (remove nil (append (list mode-symbol)
                                     (yas/extra-modes)
                                     (list major-mode
                                           (and (not dont-search-parents)
                                                (get major-mode
                                                     'derived-mode-parent)))))))))
    (remove-duplicates 
     (append mode-tables
             (mapcan #'yas/table-get-all-parents mode-tables)))))

(defun yas/menu-keymap-get-create (table)
  "Get or create the main menu keymap correspondong to MODE.

This may very well create a plethora of menu keymaps and arrange
them in all `yas/menu-table'"
  (let* ((mode (intern (yas/table-name table)))
         (menu-keymap (or (gethash mode yas/menu-table)
                          (puthash mode (make-sparse-keymap) yas/menu-table)))
        (parents (yas/table-parents table)))
    (mapc #'yas/menu-keymap-get-create parents)
    (define-key yas/minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,menu-keymap
                    :visible (yas/show-menu-p ',mode)))
    menu-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template-related and snippet loading functions

(defun yas/parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Optional GROUP is the group where the template is to go,
otherwise we attempt to calculate it from FILE.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP VARS FILE KEYBINDING UUID)

If the buffer contains a line of \"# --\" then the contents above
this line are ignored. Directives can set most of these with the syntax:

# directive-name : directive-value

Here's a list of currently recognized directives:

 * type
 * name
 * contributor
 * condition
 * group
 * key
 * expand-env
 * binding
 * uuid"
  (goto-char (point-min))
  (let* ((type 'snippet)
         (name (and file
                    (file-name-nondirectory file)))
         (key (unless yas/ignore-filenames-as-triggers
                (and name
                     (file-name-sans-extension name))))
         template
         bound
         condition
         (group (and file
                     (yas/calculate-group file)))
         expand-env
         binding
         uuid)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
                 (when (string= "uuid" (match-string-no-properties 1))
                   (setq uuid (match-string-no-properties 2)))
                 (when (string= "type" (match-string-no-properties 1))
                   (setq type (if (string= "command" (match-string-no-properties 2))
                                  'command
                                'snippet)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (yas/read-lisp (match-string-no-properties 2))))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "expand-env" (match-string-no-properties 1))
                   (setq expand-env (yas/read-lisp (match-string-no-properties 2)
                                                   'nil-on-error)))
                 (when (string= "binding" (match-string-no-properties 1))
                   (setq binding (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (when (eq type 'command)
      (setq template (yas/read-lisp (concat "(progn" template ")"))))
    (when group
      (setq group (split-string group "\\.")))
    (list key template name condition group expand-env file binding uuid)))

(defun yas/calculate-group (file)
  "Calculate the group for snippet file path FILE."
  (let* ((dominating-dir (locate-dominating-file file
                                                 ".yas-make-groups"))
         (extra-path (and dominating-dir
                          (replace-regexp-in-string (concat "^"
                                                            (expand-file-name dominating-dir))
                                                    ""
                                                    (expand-file-name file))))
         (extra-dir (and extra-path
                         (file-name-directory extra-path)))
         (group (and extra-dir
                     (replace-regexp-in-string "/"
                                               "."
                                               (directory-file-name extra-dir)))))
    group))

(defun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
                   (string-match "^#.*#$"
                                 (file-name-nondirectory file))
                   (string-match "~$"
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  (let ((mode (intern (yas/table-name (yas/template-table template)))))
    `(lambda () (interactive) (yas/expand-or-visit-from-menu ',mode ,(yas/template-uuid template)))))

(defun yas/expand-or-visit-from-menu (mode uuid)
  (let* ((table (yas/table-get-create mode))
         (yas/current-template (and table
                                    (gethash uuid (yas/table-uuidhash table)))))
    (when yas/current-template
      (if yas/visit-from-menu
          (yas/visit-snippet-file-1 yas/current-template)
        (let ((where (if (region-active-p)
                         (cons (region-beginning) (region-end))
                       (cons (point) (point)))))
          (yas/expand-snippet (yas/template-content yas/current-template)
                              (car where)
                              (cdr where)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popping up for keys and templates
;;
(defun yas/prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas/template'."
  (when templates
    (setq templates
          (sort templates #'(lambda (t1 t2)
                              (< (length (yas/template-name t1))
                                 (length (yas/template-name t2))))))
    (if yas/x-pretty-prompt-templates
        (yas/x-pretty-prompt-templates "Choose a snippet" templates)
      (some #'(lambda (fn)
                (funcall fn (or prompt "Choose a snippet: ")
                         templates
                         #'yas/template-name))
            yas/prompt-functions))))

(defun yas/prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS."
  (when keys
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet key: ") keys))
          yas/prompt-functions)))

(defun yas/prompt-for-table (tables &optional prompt)
  (when tables
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet table: ")
                       tables
                       #'yas/table-name))
          yas/prompt-functions)))

(defun yas/x-prompt (prompt choices &optional display-fn)
  "Display choices in a x-window prompt."
  ;; FIXME: HACK: if we notice that one of the objects in choices is
  ;; actually a `yas/template', defer to `yas/x-prompt-pretty-templates'
  ;;
  ;; This would be better implemented by passing CHOICES as a
  ;; strucutred tree rather than a list. Modifications would go as far
  ;; up as `yas/all-templates' I think.
  ;;
  (when (and window-system choices)
    (let ((chosen
           (let (menu d) ;; d for display
             (dolist (c choices)
               (setq d (or (and display-fn (funcall display-fn c))
                           c))
               (cond ((stringp d)
                      (push (cons (concat "   " d) c) menu))
                     ((listp d)
                      (push (car d) menu))))
             (setq menu (list prompt (push "title" menu)))
             (x-popup-menu (if (fboundp 'posn-at-point)
                               (let ((x-y (posn-x-y (posn-at-point (point)))))
                                 (list (list (+ (car x-y) 10)
                                             (+ (cdr x-y) 20))
                                       (selected-window)))
                             t)
                           menu))))
      (or chosen
          (keyboard-quit)))))

(defvar yas/x-pretty-prompt-templates nil
  "If non-nil, attempt to prompt for templates like TextMate.")
(defun yas/x-pretty-prompt-templates (prompt templates)
  "Display TEMPLATES, grouping neatly by table name."
  (let ((pretty-alist (list))
        menu
        more-than-one-table
        prefix)
    (dolist (tl templates)
      (aput 'pretty-alist (yas/template-table tl) (cons tl (aget pretty-alist (yas/template-table tl)))))
    (setq more-than-one-table (> (length pretty-alist) 1))
    (setq prefix (if more-than-one-table
                     "   " ""))
    (dolist (table-and-templates pretty-alist)
      (when (cdr table-and-templates)
        (if more-than-one-table
            (push (yas/table-name (car table-and-templates)) menu))
        (dolist (template (cdr table-and-templates))
          (push (cons (concat prefix (yas/template-name template))
                      template) menu))))
    (setq menu (nreverse menu))
    (or (x-popup-menu (if (fboundp 'posn-at-point)
                          (let ((x-y (posn-x-y (posn-at-point (point)))))
                            (list (list (+ (car x-y) 10)
                                        (+ (cdr x-y) 20))
                                  (selected-window)))
                        t)
                      (list prompt (push "title" menu)))
        (keyboard-quit))))

(defun yas/ido-prompt (prompt choices &optional display-fn)
  (when (and (featurep 'ido)
             ido-mode)
    (yas/completing-prompt prompt choices display-fn #'ido-completing-read)))

(eval-when-compile (require 'dropdown-list nil t))
(defun yas/dropdown-prompt (prompt choices &optional display-fn)
  (when (featurep 'dropdown-list)
    (let (formatted-choices
          filtered-choices
          d
          n)
      (dolist (choice choices)
        (setq d (or (and display-fn (funcall display-fn choice))
                      choice))
        (when (stringp d)
          (push d formatted-choices)
          (push choice filtered-choices)))

      (setq n (and formatted-choices (dropdown-list formatted-choices)))
      (if n
          (nth n filtered-choices)
        (keyboard-quit)))))

(defun yas/completing-prompt (prompt choices &optional display-fn completion-fn)
  (let (formatted-choices
        filtered-choices
        chosen
        d
        (completion-fn (or completion-fn
                           #'completing-read)))
    (dolist (choice choices)
      (setq d (or (and display-fn (funcall display-fn choice))
                    choice))
      (when (stringp d)
        (push d formatted-choices)
        (push choice filtered-choices)))
    (setq chosen (and formatted-choices
                      (funcall completion-fn prompt
                               formatted-choices
                               nil
                               'require-match
                               nil
                               nil)))
    (when chosen
      (nth (position chosen formatted-choices :test #'string=) filtered-choices))))

(defun yas/no-prompt (prompt choices &optional display-fn)
  (first choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;;
(defun yas/load-directory-1 (directory &optional mode-sym parents)
  "Recursively load snippet templates from DIRECTORY."

  ;; Load .yas-setup.el files wherever we find them
  ;;
  (let ((file (concat directory "/" ".yas-setup")))
    (when (or (file-readable-p (concat file ".el"))
              (file-readable-p (concat file ".elc")))
      (load file)))

  ;;
  ;;
  (unless (file-exists-p (concat directory "/" ".yas-skip"))
    (let* ((major-mode-and-parents (if mode-sym
                                       (cons mode-sym parents)
                                     (yas/compute-major-mode-and-parents (concat directory
                                                                                 "/dummy"))))
           (yas/ignore-filenames-as-triggers
            (or yas/ignore-filenames-as-triggers
                (file-exists-p (concat directory "/"
                                       ".yas-ignore-filenames-as-triggers"))))
           (snippet-defs nil))
      ;; load the snippet files
      ;;
      (with-temp-buffer
        (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
          (when (file-readable-p file)
            (insert-file-contents file nil nil nil t)
            (push (yas/parse-template file)
                  snippet-defs))))
      (when snippet-defs
        (yas/define-snippets (car major-mode-and-parents)
                             snippet-defs
                             (cdr major-mode-and-parents)))
      ;; now recurse to a lower level
      ;;
      (dolist (subdir (yas/subdirs directory))
        (yas/load-directory-1 subdir
                              (car major-mode-and-parents)
                              (cdr major-mode-and-parents))))))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.

Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (unless yas/snippet-dirs
    (setq yas/snippet-dirs directory))
  (dolist (dir (yas/subdirs directory))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "[yas] Loaded snippets from %s." directory)))

(defun yas/load-snippet-dirs ()
  "Reload the directories listed in `yas/snippet-dirs' or
   prompt the user to select one."
  (if yas/snippet-dirs
      (dolist (directory (reverse (yas/snippet-dirs)))
        (yas/load-directory directory))
    (call-interactively 'yas/load-directory)))

(defun yas/reload-all (&optional reset-root-directory)
  "Reload all snippets and rebuild the YASnippet menu. "
  (interactive "P")
  ;; Turn off global modes and minor modes, save their state though
  ;;
  (let ((restore-global-mode (prog1 yas/global-mode
                               (yas/global-mode -1)))
        (restore-minor-mode (prog1 yas/minor-mode
                              (yas/minor-mode -1))))
    ;; Empty all snippet tables and all menu tables
    ;;
    (setq yas/tables (make-hash-table))
    (setq yas/menu-table (make-hash-table))

    ;; Init the `yas/minor-mode-map', taking care not to break the
    ;; menu....
    ;;
    (setf (cdr yas/minor-mode-map)
          (cdr (yas/init-minor-keymap)))

    (when reset-root-directory
      (setq yas/snippet-dirs nil))

    ;; Reload the directories listed in `yas/snippet-dirs' or prompt
    ;; the user to select one.
    ;;
    (yas/load-snippet-dirs)
    ;; Reload the direct keybindings
    ;;
    (yas/direct-keymaps-reload)
    ;; Restore the mode configuration
    ;;
    (when restore-minor-mode
      (yas/minor-mode 1))
    (when restore-global-mode
      (yas/global-mode 1))

    (message "[yas] Reloading everything... Done.")))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet Bundle

(defun yas/initialize ()
  "For backward compatibility, enable `yas/minor-mode' globally"
  (yas/global-mode 1))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code dropdown)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.

YASNIPPET is the yasnippet.el file path.

YASNIPPET-BUNDLE is the output file of the compile result.

SNIPPET-ROOTS is a list of root directories that contains the
snippets definition.

CODE is the code to be placed at the end of the generated file
and that can initialize the YASnippet bundle.

Last optional argument DROPDOWN is the filename of the
dropdown-list.el library.

Here's the default value for all the parameters:

  (yas/compile-bundle \"yasnippet.el\"
                      \"yasnippet-bundle.el\"
                      \"snippets\")
                      \"(yas/initialize-bundle)
                        ### autoload
                        (require 'yasnippet-bundle)`\"
                      \"dropdown-list.el\")
"
  (interactive (concat "ffind the yasnippet.el file: \nFTarget bundle file: "
                       "\nDSnippet directory to bundle: \nMExtra code? \nfdropdown-library: "))

  (let* ((yasnippet (or yasnippet
                        "yasnippet.el"))
         (yasnippet-bundle (or yasnippet-bundle
                               "./yasnippet-bundle.el"))
         (snippet-roots (or snippet-roots
                            "snippets"))
         (dropdown (or dropdown
                       "dropdown-list.el"))
         (code (or (and code
                        (condition-case err (read code) (error nil))
                        code)
                   (concat "(yas/initialize-bundle)"
                           "\n;;;###autoload" ; break through so that won't
                           "(require 'yasnippet-bundle)")))
         (dirs (or (and (listp snippet-roots) snippet-roots)
                   (list snippet-roots)))
         (bundle-buffer nil))
    (with-temp-file yasnippet-bundle
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert "\n")
      (when dropdown
        (insert-file-contents dropdown))
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\"")
      (flet ((yas/define-snippets
              (mode snippets &optional parent-or-parents)
              (insert ";;; snippets for " (symbol-name mode) "\n")
              (let ((literal-snippets (list)))
                (dolist (snippet snippets)
                  (let ((key                    (first   snippet))
                        (template-content       (second  snippet))
                        (name                   (third   snippet))
                        (condition              (fourth  snippet))
                        (group                  (fifth   snippet))
                        (expand-env             (sixth   snippet))
                        (file                   nil) ;; (seventh snippet)) ;; omit on purpose
                        (binding                (eighth  snippet))
                        (uuid                    (ninth   snippet)))
                    (push `(,key
                            ,template-content
                            ,name
                            ,condition
                            ,group
                            ,expand-env
                            ,file
                            ,binding
                            ,uuid)
                          literal-snippets)))
                (insert (pp-to-string `(yas/define-snippets ',mode ',literal-snippets ',parent-or-parents)))
                (insert "\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/subdirs dir))
            (let ((file (concat subdir "/.yas-setup.el")))
              (when (file-readable-p file)
                (insert ";; Supporting elisp for subdir " (file-name-nondirectory subdir) "\n\n")
                (goto-char (+ (point)
                              (second (insert-file-contents file))))))
            (yas/load-directory-1 subdir nil))))

      (insert (pp-to-string `(yas/global-mode 1)))
      (insert ")\n\n" code "\n")

      ;; bundle-specific provide and value for yas/dont-activate
      (let ((bundle-feature-name (file-name-nondirectory
                                  (file-name-sans-extension
                                   yasnippet-bundle))))
        (insert (pp-to-string `(set-default 'yas/dont-activate
                                            #'(lambda ()
                                                (and (or yas/snippet-dirs
                                                         (featurep ',(make-symbol bundle-feature-name)))
                                                     (null (yas/get-snippet-tables)))))))
        (insert (pp-to-string `(provide ',(make-symbol bundle-feature-name)))))

      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n"))))

(defun yas/compile-textmate-bundle ()
  (interactive)
  (yas/compile-bundle "yasnippet.el"
                      "./yasnippet-textmate-bundle.el"
                      "extras/imported/"
                      (concat "(yas/initialize-bundle)"
                              "\n;;;###autoload" ; break through so that won't
                              "(require 'yasnippet-textmate-bundle)")
                      "dropdown-list.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some user level functions
;;;

(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>/joaotavora <joaotavora@gmail.com>")))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define SNIPPETS for MODE.

SNIPPETS is a list of snippet definitions, each taking the
following form

 (KEY TEMPLATE NAME CONDITION GROUP EXPAND-ENV FILE KEYBINDING UUID)

Within these, only KEY and TEMPLATE are actually mandatory.

TEMPLATE might be a lisp form or a string, depending on whether
this is a snippet or a snippet-command.

CONDITION, EXPAND-ENV and KEYBINDING are lisp forms, they have
been `yas/read-lisp'-ed and will eventually be
`yas/eval-lisp'-ed.

The remaining elements are strings.

FILE is probably of very little use if you're programatically
defining snippets.

UUID is the snippets \"unique-id\". Loading a second snippet file
with the same uuid replaced the previous snippet.

You can use `yas/parse-template' to return such lists based on
the current buffers contents.

Optional PARENT-MODE can be used to specify the parent tables of
MODE. It can be a mode symbol of a list of mode symbols. It does
not need to be a real mode."
  ;; X) `snippet-table' is created or retrieved for MODE, same goes
  ;;    for the list of snippet tables `parent-tables'.
  ;;
  (let ((snippet-table (yas/table-get-create mode))
        (parent-tables (mapcar #'yas/table-get-create
                               (if (listp parent-mode)
                                   parent-mode
                                 (list parent-mode))))
        (template nil))
    ;; X) Connect `snippet-table' with `parent-tables'.
    ;;
    ;; TODO: this should be a remove-duplicates of the concatenation
    ;; of `snippet-table's existings parents with the new parents...
    ;;
    (dolist (parent parent-tables)
      (unless (find parent (yas/table-parents snippet-table))
        (push parent
              (yas/table-parents snippet-table))))

    ;; X) Now, iterate for evey snippet def list
    ;;
    (dolist (snippet snippets)
      (setq template (yas/define-snippets-1 snippet
                                            snippet-table)))
    template))

(defun yas/define-snippets-1 (snippet snippet-table)
  "Helper for `yas/define-snippets'."
  ;; X) Calculate some more defaults on the values returned by
  ;; `yas/parse-template'.
  ;;
  (let* ((file (seventh snippet))
         (key (or (car snippet)
                  (unless yas/ignore-filenames-as-triggers
                    (and file
                         (file-name-sans-extension (file-name-nondirectory file))))))
         (name (or (third snippet)
                   (and file
                        (file-name-directory file))))
         (condition (fourth snippet))
         (group (fifth snippet))
         (keybinding (yas/read-keybinding (eighth snippet)))
         (uuid (or (ninth snippet)
                  name))
         (template (or (gethash uuid (yas/table-uuidhash snippet-table))
                       (yas/make-blank-template))))
    ;; X) populate the template object
    ;;
    (yas/populate-template template
                           :table       snippet-table
                           :key         key
                           :content     (second snippet)
                           :name        (or name key)
                           :group       group
                           :condition   condition
                           :expand-env  (sixth snippet)
                           :file        (seventh snippet)
                           :keybinding  keybinding
                           :uuid         uuid)
    ;; X) Update this template in the appropriate table. This step
    ;;    also will take care of adding the key indicators in the
    ;;    templates menu entry, if any
    ;;
    (yas/update-template snippet-table template)
    ;; X) Return the template
    ;;
    ;;
    template))

(defun yas/snippet-menu-binding-pair-get-create (template &optional type)
  "Get TEMPLATE's menu binding or assign it a new one."
  (or (yas/template-menu-binding-pair template)
      (let ((key (yas/template-key template))
            (keybinding (yas/template-keybinding template)))
        (setf (yas/template-menu-binding-pair template)
              (cons `(menu-item ,(or (yas/template-name template)
                                     (yas/template-uuid template))
                                ,(yas/make-menu-binding template)
                                :keys ,nil)
                    type)))))

(defun yas/show-menu-p (mode)
  (cond ((eq yas/use-menu 'abbreviate)
         (find mode
               (mapcar #'(lambda (table)
                           (intern (yas/table-name table)))
                       (yas/get-snippet-tables))))
        ((eq yas/use-menu 'real-modes)
         (yas/real-mode? mode))
        (t
         t)))

(defun yas/delete-from-keymap (keymap uuid)
  "Recursively delete items with UUID from KEYMAP and its submenus."

  ;; XXX: This used to skip any submenus named \"parent mode\"
  ;; 
  ;; First of all, recursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;;
  (mapc #'(lambda (item)
            (when (and (listp (cdr item))
                       (keymapp (third (cdr item))))
              (yas/delete-from-keymap (third (cdr item)) uuid)))
        (rest keymap))
  ;; Set the uuid entry to nil
  ;; 
  (define-key keymap (vector (make-symbol uuid)) nil)
  ;; Destructively modify keymap
  ;; 
  (setcdr keymap (delete-if #'(lambda (item)
                                (or (null (cdr item))
                                    (and (keymapp (third (cdr item)))
                                         (null (cdr (third (cdr item)))))))
                            (rest keymap))))

(defun yas/define-menu (mode menu omit-items)
  "Define a snippet menu for MODE according to MENU, ommitting OMIT-ITEMS.

MENU is a list, its elements can be:

- (yas/item UUID) : Creates an entry the snippet identified with
  UUID. The menu entry for a snippet thus identified is
  permanent, i.e. it will never move in the menu.

- (yas/separator) : Creates a separator

- (yas/submenu NAME SUBMENU) : Creates a submenu with NAME,
  SUBMENU has the same form as MENU. NAME is also added to the
  list of groups of the snippets defined thereafter.

OMIT-ITEMS is a list of snippet uuid's that will always be
ommited from MODE's menu, even if they're manually loaded.
"
  (let* ((table (yas/table-get-create mode))
         (hash (yas/table-uuidhash table)))
    (yas/define-menu-1 table
                       (yas/menu-keymap-get-create table)
                       menu
                       hash)
    (dolist (uuid omit-items)
      (let ((template (or (gethash uuid hash)
                          (yas/populate-template (puthash uuid
                                                          (yas/make-blank-template)
                                                          hash)
                                                 :table table
                                                 :uuid uuid))))
        (setf (yas/template-menu-binding-pair template) (cons nil :none))))))

(defun yas/define-menu-1 (table keymap menu uuidhash &optional group-list)
  (dolist (e (reverse menu))
    (cond ((eq (first e) 'yas/item)
           (let ((template (or (gethash (second e) uuidhash)
                               (yas/populate-template (puthash (second e)
                                                               (yas/make-blank-template)
                                                               uuidhash)
                                                      :table table
                                                      :perm-group group-list
                                                      :uuid (second e)))))
             (define-key keymap (vector (make-symbol (second e)))
               (car (yas/snippet-menu-binding-pair-get-create template :stay)))))
          ((eq (first e) 'yas/submenu)
           (let ((subkeymap (make-sparse-keymap)))
             (define-key keymap (vector (make-symbol(second e)))
               `(menu-item ,(second e) ,subkeymap))
             (yas/define-menu-1 table
                                subkeymap
                                (third e)
                                uuidhash
                                (append group-list (list (second e))))))
          ((eq (first e) 'yas/separator)
           (define-key keymap (vector (gensym))
             '(menu-item "----")))
          (t
           (message "[yas] don't know anything about menu entry %s" (first e))))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.

NAME is a description to this template.  Also update the menu if
`yas/use-menu' is `t'.  CONDITION is the condition attached to
this snippet.  If you attach a condition to a snippet, then it
will only be expanded when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (undo 1)
    nil))


;;; Apropos condition-cache:
;;;
;;;
;;;
;;;
(defvar yas/condition-cache-timestamp nil)
(defmacro yas/define-condition-cache (func doc &rest body)
  "Define a function FUNC with doc DOC and body BODY, BODY is
executed at most once every snippet expansion attempt, to check
expansion conditions.

It doesn't make any sense to call FUNC programatically."
  `(defun ,func () ,(if (and doc
                             (stringp doc))
                        (concat doc
"\n\nFor use in snippets' conditions. Within each
snippet-expansion routine like `yas/expand', computes actual
value for the first time then always returns a cached value.")
                      (setq body (cons doc body))
                      nil)
     (let ((timestamp-and-value (get ',func 'yas/condition-cache)))
       (if (equal (car timestamp-and-value) yas/condition-cache-timestamp)
           (cdr timestamp-and-value)
         (let ((new-value (progn
                            ,@body
                            )))
           (put ',func 'yas/condition-cache (cons yas/condition-cache-timestamp new-value))
           new-value)))))

(defalias 'yas/expand 'yas/expand-from-trigger-key)
(defun yas/expand-from-trigger-key (&optional field)
  "Expand a snippet before point.

If no snippet expansion is possible, fall back to the behaviour
defined in `yas/fallback-behavior'.

Optional argument FIELD is for non-interactive use and is an
object satisfying `yas/field-p' to restrict the expansion to."
  (interactive)
  (setq yas/condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas/expand-only-for-last-commands
                 (not (member last-command yas/expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas/field-start field)
                                                      (yas/field-end field))
                                    (yas/current-key))
                                (yas/current-key))))
    (if (and templates-and-pos
             (first templates-and-pos))
        (yas/expand-or-prompt-for-template (first templates-and-pos)
                                           (second templates-and-pos)
                                           (third templates-and-pos))
      (yas/fallback 'trigger-key))))

(defun yas/expand-from-keymap ()
  "Directly expand some snippets, searching `yas/direct-keymaps'.

If expansion fails, execute the previous binding for this key"
  (interactive)
  (setq yas/condition-cache-timestamp (current-time))
  (let* ((vec (this-command-keys-vector))
         (templates (mapcan #'(lambda (table)
                                (yas/fetch table vec))
                            (yas/get-snippet-tables))))
    (if templates
        (yas/expand-or-prompt-for-template templates)
      (let ((yas/fallback-behavior 'call-other-command))
        (yas/fallback)))))

(defun yas/expand-or-prompt-for-template (templates &optional start end)
  "Expand one of TEMPLATES from START to END.

Prompt the user if TEMPLATES has more than one element, else
expand immediately. Common gateway for
`yas/expand-from-trigger-key' and `yas/expand-from-keymap'."
  (let ((yas/current-template (or (and (rest templates) ;; more than one
                                       (yas/prompt-for-template (mapcar #'cdr templates)))
                                  (cdar templates))))
    (when yas/current-template
      (yas/expand-snippet (yas/template-content yas/current-template)
                          start
                          end
                          (yas/template-expand-env yas/current-template)))))

(defun yas/fallback (&optional from-trigger-key-p)
  "Fallback after expansion has failed.

Common gateway for `yas/expand-from-trigger-key' and
`yas/expand-from-keymap'."
  (cond ((eq yas/fallback-behavior 'return-nil)
         ;; return nil
         nil)
        ((eq yas/fallback-behavior 'call-other-command)
         (let* ((yas/minor-mode nil)
                (yas/direct-keymaps nil)
                (keys-1 (this-command-keys-vector))
                (keys-2 (and yas/trigger-key
                             from-trigger-key-p
                             (stringp yas/trigger-key)
                             (read-kbd-macro yas/trigger-key)))
                (command-1 (and keys-1 (key-binding keys-1)))
                (command-2 (and keys-2 (key-binding keys-2)))
                ;; An (ugly) safety: prevents infinite recursion of
                ;; yas/expand* calls.
                (command (or (and (symbolp command-1)
                                  (not (string-match "yas/expand" (symbol-name command-1)))
                                  command-1)
                             (and (symbolp command-2)
                                  command-2))))
           (when (and (commandp command)
                      (not (string-match "yas/expand" (symbol-name command))))
             (setq this-command command)
             (call-interactively command))))
        ((and (listp yas/fallback-behavior)
              (cdr yas/fallback-behavior)
              (eq 'apply (car yas/fallback-behavior)))
         (if (cddr yas/fallback-behavior)
             (apply (cadr yas/fallback-behavior)
                    (cddr yas/fallback-behavior))
           (when (commandp (cadr yas/fallback-behavior))
             (setq this-command (cadr yas/fallback-behavior))
             (call-interactively (cadr yas/fallback-behavior)))))
        (t
         ;; also return nil if all the other fallbacks have failed
         nil)))



;;; Snippet development

(defun yas/all-templates (tables)
  "Return all snippet tables applicable for the current buffer.

Honours `yas/choose-tables-first', `yas/choose-keys-first' and
`yas/buffer-local-condition'"
  (when yas/choose-tables-first
    (setq tables (list (yas/prompt-for-table tables))))
  (mapcar #'cdr
          (if yas/choose-keys-first
              (let ((key (yas/prompt-for-keys
                          (mapcan #'yas/table-all-keys tables))))
                (when key
                  (mapcan #'(lambda (table)
                              (yas/fetch table key))
                          tables)))
            (remove-duplicates (mapcan #'yas/table-templates tables)
                               :test #'equal))))

(defun yas/insert-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas/prompt-function'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (setq yas/condition-cache-timestamp (current-time))
  (let* ((yas/buffer-local-condition (or (and no-condition
                                              'always)
                                         yas/buffer-local-condition))
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (yas/current-template (and templates
                                    (or (and (rest templates) ;; more than one template for same key
                                             (yas/prompt-for-template templates))
                                        (car templates))))
         (where (if (region-active-p)
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (if yas/current-template
        (yas/expand-snippet (yas/template-content yas/current-template)
                            (car where)
                            (cdr where)
                            (yas/template-expand-env yas/current-template))
      (message "[yas] No snippets can be inserted here!"))))

(defun yas/visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas/insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas/buffer-local-condition 'always)
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
         (template (and templates
                        (or (yas/prompt-for-template templates
                                                     "Choose a snippet template to edit: ")
                            (car templates)))))

    (if template
        (yas/visit-snippet-file-1 template)
      (message "No snippets tables active!"))))

(defun yas/visit-snippet-file-1 (template)
  (let ((file (yas/template-file template)))
    (cond ((and file (file-readable-p file))
           (find-file-other-window file)
           (snippet-mode)
           (set (make-local-variable 'yas/editing-template) template))
          (file
           (message "Original file %s no longer exists!" file))
          (t
           (switch-to-buffer (format "*%s*"(yas/template-name template)))
           (let ((type 'snippet))
             (when (listp (yas/template-content template))
               (insert (format "# type: command\n"))
               (setq type 'command))
             (insert (format "# key: %s\n" (yas/template-key template)))
             (insert (format "# name: %s\n" (yas/template-name template)))
             (when (yas/template-keybinding template)
               (insert (format "# binding: %s\n" (yas/template-keybinding template))))
             (when (yas/template-expand-env template)
               (insert (format "# expand-env: %s\n" (yas/template-expand-env template))))
             (when (yas/template-condition template)
               (insert (format "# condition: %s\n" (yas/template-condition template))))
             (insert "# --\n")
             (insert (if (eq type 'command)
                         (pp-to-string (yas/template-content template))
                       (yas/template-content template))))
           (snippet-mode)
           (set (make-local-variable 'yas/editing-template) template)))))

(defun yas/guess-snippet-directories-1 (table)
  "Guesses possible snippet subdirectories for TABLE."
  (cons (yas/table-name table)
        (mapcan #'(lambda (parent)
                    (yas/guess-snippet-directories-1
                     parent))
                (yas/table-parents table))))

(defun yas/guess-snippet-directories (&optional table)
  "Try to guess suitable directories based on the current active
tables (or optional TABLE).

Returns a list of elemts (TABLE . DIRS) where TABLE is a
`yas/table' object and DIRS is a list of all possible directories
where snippets of table might exist."
  (let ((main-dir (replace-regexp-in-string
                   "/+$" ""
                   (or (first (or (yas/snippet-dirs)
                                  (setq yas/snippet-dirs '("~/.emacs.d/snippets")))))))
        (tables (or (and table
                         (list table))
                    (yas/get-snippet-tables))))
    ;; HACK! the snippet table created here is actually registered!
    ;;
    (unless (or table (gethash major-mode yas/tables))
      (push (yas/table-get-create major-mode)
            tables))

    (mapcar #'(lambda (table)
                (cons table
                      (mapcar #'(lambda (subdir)
                                  (concat main-dir "/" subdir))
                              (yas/guess-snippet-directories-1 table))))
            tables)))

(defun yas/make-directory-maybe (table-and-dirs &optional main-table-string)
  "Returns a dir inside  TABLE-AND-DIRS, prompts for creation if none exists."
  (or (some #'(lambda (dir) (when (file-directory-p dir) dir)) (cdr table-and-dirs))
      (let ((candidate (first (cdr table-and-dirs))))
        (unless (file-writable-p (file-name-directory candidate))
          (error "[yas] %s is not writable." candidate))
        (if (y-or-n-p (format "Guessed directory (%s) for%s%s table \"%s\" does not exist! Create? "
                              candidate
                              (if (gethash (intern (yas/table-name (car table-and-dirs)))
                                           yas/tables)
                                  ""
                                " brand new")
                              (or main-table-string
                                  "")
                              (yas/table-name (car table-and-dirs))))
            (progn
              (make-directory candidate 'also-make-parents)
              ;; create the .yas-parents file here...
              candidate)))))

(defun yas/new-snippet (&optional choose-instead-of-guess)
  ""
  (interactive "P")
  (let ((guessed-directories (yas/guess-snippet-directories)))

    (switch-to-buffer "*new snippet*")
    (erase-buffer)
    (kill-all-local-variables)
    (snippet-mode)
    (set (make-local-variable 'yas/guessed-modes) (mapcar #'(lambda (d)
                                                              (intern (yas/table-name (car d))))
                                                          guessed-directories))
    (unless (and choose-instead-of-guess
                 (not (y-or-n-p "Insert a snippet with useful headers? ")))
      (yas/expand-snippet "\
# -*- mode: snippet -*-
# name: $1
# key: $2${3:
# binding: ${4:direct-keybinding}}${5:
# expand-env: ((${6:some-var} ${7:some-value}))}${8:
# type: command}
# --
$0"))))

(defun yas/find-snippets (&optional same-window )
  "Find snippet file in guessed current mode's directory.

Calls `find-file' interactively in the guessed directory.

With prefix arg SAME-WINDOW opens the buffer in the same window.

Because snippets can be loaded from many different locations,
this has to guess the correct directory using
`yas/guess-snippet-directories', which returns a list of
options.

If any one of these exists, it is taken and `find-file' is called
there, otherwise, proposes to create the first option returned by
`yas/guess-snippet-directories'."
  (interactive "P")
  (let* ((guessed-directories (yas/guess-snippet-directories))
         (chosen)
         (buffer))
    (setq chosen (yas/make-directory-maybe (first guessed-directories) " main"))
    (unless chosen
      (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                            (mapcar #'(lambda (table-and-dirs)
                                        (yas/table-name (car table-and-dirs)))
                                    (rest guessed-directories))))
          (setq chosen (some #'yas/make-directory-maybe
                             (rest guessed-directories)))))
    (unless chosen
      (when (y-or-n-p "Having trouble... go to snippet root dir? ")
        (setq chosen (first (yas/snippet-dirs)))))
    (if chosen
        (let ((default-directory chosen))
          (setq buffer (call-interactively (if same-window
                                               'find-file
                                             'find-file-other-window)))
          (when buffer
            (save-excursion
              (set-buffer buffer)
              (when (eq major-mode 'fundamental-mode)
                (snippet-mode)))))
      (message "Could not guess snippet dir!"))))

(defun yas/compute-major-mode-and-parents (file &optional prompt-if-failed)
  (let* ((file-dir (and file
                        (directory-file-name (or (some #'(lambda (special)
                                                           (locate-dominating-file file special))
                                                       '(".yas-setup.el"
                                                         ".yas-make-groups"
                                                         ".yas-parents"))
                                                 (directory-file-name (file-name-directory file))))))
         (parents-file-name (concat file-dir "/.yas-parents"))
         (major-mode-name (and file-dir
                               (file-name-nondirectory file-dir)))
         (major-mode-sym (or (and major-mode-name
                                  (intern major-mode-name))
                             (when prompt-if-failed
                               (read-from-minibuffer
                                "[yas] Cannot auto-detect major mode! Enter a major mode: "))))
         (parents (when (file-readable-p parents-file-name)
                         (mapcar #'intern
                                 (split-string
                                  (with-temp-buffer
                                    (insert-file-contents parents-file-name)
                                    (buffer-substring-no-properties (point-min)
                                                                    (point-max))))))))
    (when major-mode-sym
      (cons major-mode-sym parents))))

(defvar yas/editing-template nil
  "Supporting variable for `yas/load-snippet-buffer' and `yas/visit-snippet'")

(defvar yas/current-template nil
  "Holds the current template being expanded into a snippet.")

(defvar yas/guessed-modes nil
  "List of guessed modes supporting `yas/load-snippet-buffer'.")

(defun yas/load-snippet-buffer (&optional kill)
  "Parse and load current buffer's snippet definition.

With optional prefix argument KILL quit the window and buffer."
  (interactive "P")
  (let ((yas/ignore-filenames-as-triggers
         (or yas/ignore-filenames-as-triggers
             (and buffer-file-name
                  (locate-dominating-file
                   buffer-file-name
                   ".yas-ignore-filenames-as-triggers")))))
    (cond
     ;;  We have `yas/editing-template', this buffer's
     ;;  content comes from a template which is already loaded and
     ;;  neatly positioned,...
     ;;
     (yas/editing-template
      (yas/define-snippets-1 (yas/parse-template (yas/template-file yas/editing-template)) 
                             (yas/template-table yas/editing-template)))
     ;; Try to use `yas/guessed-modes'. If we don't have that use the
     ;; value from `yas/compute-major-mode-and-parents'
     ;;
     (t
      (unless yas/guessed-modes
        (set (make-local-variable 'yas/guessed-modes) (or (yas/compute-major-mode-and-parents buffer-file-name))))
      (let* ((prompt (if (and (featurep 'ido)
                              ido-mode)
                         'ido-completing-read 'completing-read))
             (table (yas/table-get-create
                     (intern
                      (funcall prompt (format "Choose or enter a table (yas guesses %s): "
                                              (if yas/guessed-modes
                                                  (first yas/guessed-modes)
                                                "nothing"))
                               (mapcar #'symbol-name yas/guessed-modes)
                               nil
                               nil
                               nil
                               nil
                               (if (first yas/guessed-modes)
                                   (symbol-name (first yas/guessed-modes))))))))
        (set (make-local-variable 'yas/editing-template) 
             (yas/define-snippets-1 (yas/parse-template buffer-file-name)
                                    table))))))
  ;; Now, offer to save this shit
  ;;
  ;; 1) if `yas/snippet-dirs' is a list and its first element does not
  ;; match this template's file (i.e. this is a library snippet, not
  ;; a user snippet).
  ;;
  ;; 2) yas/editing-template comes from a file that we cannot write to...
  ;;
  (when (or (not (yas/template-file yas/editing-template))
            (not (file-writable-p (yas/template-file yas/editing-template)))
            (and (listp yas/snippet-dirs)
                 (second yas/snippet-dirs)
                 (not (string-match (expand-file-name (first yas/snippet-dirs))
                                    (yas/template-file yas/editing-template)))))
    
    (when (y-or-n-p "[yas] Looks like a library or new snippet. Save to new file? ")
      (let* ((option (first (yas/guess-snippet-directories (yas/template-table yas/editing-template))))
             (chosen (and option
                          (yas/make-directory-maybe option))))
        (when chosen
          (let ((default-file-name (or (and (yas/template-file yas/editing-template)
                                            (file-name-nondirectory (yas/template-file yas/editing-template)))
                                       (yas/template-name yas/editing-template))))
            (write-file (concat chosen "/"
                                (read-from-minibuffer (format "File name to create in %s? " chosen)
                                                      default-file-name)))
            (setf (yas/template-file yas/editing-template) buffer-file-name))))))
  (when kill
    (quit-window kill))
  (message "[yas] Snippet \"%s\" loaded for %s."
           (yas/template-name yas/editing-template)
           (yas/table-name (yas/template-table yas/editing-template))))


(defun yas/tryout-snippet (&optional debug)
  "Test current buffers's snippet template in other buffer."
  (interactive "P")
  (let* ((major-mode-and-parent (yas/compute-major-mode-and-parents buffer-file-name))
         (parsed (yas/parse-template))
         (test-mode (or (and (car major-mode-and-parent)
                             (fboundp (car major-mode-and-parent))
                             (car major-mode-and-parent))
                        (first yas/guessed-modes)
                        (intern (read-from-minibuffer "[yas] please input a mode: "))))
         (yas/current-template
          (and parsed
               (fboundp test-mode)
               (yas/populate-template (yas/make-blank-template)
                                      :table       nil ;; no tables for ephemeral snippets
                                      :key         (first parsed)
                                      :content     (second parsed)
                                      :name        (third parsed)
                                      :expand-env  (sixth parsed)))))
    (cond (yas/current-template
           (let ((buffer-name (format "*testing snippet: %s*" (yas/template-name yas/current-template))))
             (kill-buffer (get-buffer-create buffer-name))
             (switch-to-buffer (get-buffer-create buffer-name))
             (setq buffer-undo-list nil)
             (condition-case nil (funcall test-mode) (error nil))
             (yas/expand-snippet (yas/template-content yas/current-template)
                                 (point-min)
                                 (point-max)
                                 (yas/template-expand-env yas/current-template))
             (when (and debug
                        (require 'yasnippet-debug nil t))
               (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local))))
          (t
           (message "[yas] Cannot test snippet for unknown major mode")))))

(defun yas/template-fine-group (template)
  (car (last (or (yas/template-group template)
                 (yas/template-perm-group template)))))

(defun yas/describe-tables (&optional choose)
  "Display snippets for each table."
  (interactive "P")
  (let* ((by-name-hash (and choose
                            (y-or-n-p "Show by namehash? ")))
         (buffer (get-buffer-create "*YASnippet tables*"))
         (active-tables (yas/get-snippet-tables))
         (remain-tables (let ((all))
                          (maphash #'(lambda (k v)
                                       (unless (find v active-tables)
                                         (push v all)))
                                   yas/tables)
                          all))
         (table-lists (list active-tables remain-tables))
         (original-buffer (current-buffer))
         (continue t)
         (yas/condition-cache-timestamp (current-time)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (cond ((not by-name-hash)
             (insert "YASnippet tables: \n")
             (while (and table-lists
                         continue)
               (dolist (table (car table-lists))
                 (yas/describe-pretty-table table original-buffer))
               (setq table-lists (cdr table-lists))
               (when table-lists
                 (yas/create-snippet-xrefs)
                 (display-buffer buffer)
                 (setq continue (and choose (y-or-n-p "Show also non-active tables? ")))))
             (yas/create-snippet-xrefs)
             (help-mode)
             (goto-char 1))
            (t
             (insert "\n\nYASnippet tables by NAMEHASH: \n")
             (dolist (table (append active-tables remain-tables))
               (insert (format "\nSnippet table `%s':\n\n" (yas/table-name table)))
               (let ((keys))
                 (maphash #'(lambda (k v)
                              (push k keys))
                          (yas/table-hash table))
                 (dolist (key keys)
                   (insert (format "   key %s maps snippets: %s\n" key
                                   (let ((names))
                                     (maphash #'(lambda (k v)
                                                  (push k names))
                                              (gethash key (yas/table-hash table)))
                                     names))))))))
      (goto-char 1)
      (setq buffer-read-only t))
    (display-buffer buffer)))

(defun yas/describe-pretty-table (table &optional original-buffer)
  (insert (format "\nSnippet table `%s'"
                  (yas/table-name table)))
  (if (yas/table-parents table)
      (insert (format " parents: %s\n"
                      (mapcar #'yas/table-name
                              (yas/table-parents table))))
    (insert "\n"))
  (insert (make-string 100 ?-) "\n")
  (insert "group                   state name                                    key             binding\n")
  (let ((groups-alist (list))
        group)
    (maphash #'(lambda (k v)
                 (setq group (or (yas/template-fine-group v)
                                 "(top level)"))
                 (when (yas/template-name v)
                   
                   (aput 'groups-alist group (cons v (aget groups-alist group)))))
             (yas/table-uuidhash table))
    (dolist (group-and-templates groups-alist)
      (when (rest group-and-templates)
        (setq group (truncate-string-to-width (car group-and-templates) 25 0 ?  "..."))
        (insert (make-string 100 ?-) "\n")
        (dolist (p (cdr group-and-templates))
          (let ((name (truncate-string-to-width (propertize (format "\\\\snippet `%s'" (yas/template-name p))
                                                            'yasnippet p)
                                                50 0 ? "..."))
                (group (prog1 group 
                         (setq group (make-string (length group) ? ))))
                (condition-string (let ((condition (yas/template-condition p)))
                                    (if (and condition
                                             original-buffer)
                                        (with-current-buffer original-buffer
                                          (if (yas/eval-condition condition)
                                              "(y)"
                                            "(s)"))
                                      "(a)"))))
            (insert group " ")
            (insert condition-string " ")
            (insert name 
                    (if (string-match "\\.\\.\\.$" name)
                        "'"
                      " ")
                    " ")
            (insert (truncate-string-to-width (or (yas/template-key p) "")
                                              15 0 ?  "...") " ")
            (insert (truncate-string-to-width (key-description (yas/template-keybinding p))
                                              15 0 ?  "...") " ")
            (insert "\n")))))))





;;; User convenience functions, for using in snippet definitions

(defvar yas/modified-p nil
  "Non-nil if field has been modified by user or transformation.")

(defvar yas/moving-away-p nil
  "Non-nil if user is about to exit field.")

(defvar yas/text nil
  "Contains current field text.")

(defun yas/substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas/choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (some #'(lambda (fn)
              (funcall fn "Choose: " possibilities))
          yas/prompt-functions)))

(defun yas/key-to-value (alist)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
        (or (cdr (find key alist :key #'car :test #'string=))
            key)))))

(defun yas/throw (text)
  "Throw a yas/exception with TEXT as the reason."
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away-p (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
    (yas/throw (format "[yas] field only allows %s" possibilities))))

(defun yas/field-value (number)
  "Get the string for field with NUMBER.

Use this in primary and mirror transformations to tget."
  (let* ((snippet (car (yas/snippets-at-point)))
         (field (and snippet
                     (yas/snippet-find-field snippet number))))
    (when field
      (yas/field-text-for-display field))))

(defun yas/text ()
  "Return `yas/text' if that exists and is non-empty, else nil."
  (if (and yas/text
           (not (string= "" yas/text)))
      yas/text))

;; (defun yas/selected-text ()
;;   "Return `yas/selected-text' if that exists and is non-empty, else nil."
;;   (if (and yas/selected-text
;;            (not (string= "" yas/selected-text)))
;;       yas/selected-text))

(defun yas/get-field-once (number &optional transform-fn)
  (unless yas/modified-p
    (if transform-fn
        (funcall transform-fn (yas/field-value number))
      (yas/field-value number))))

(defun yas/default-from-field (number)
  (unless yas/modified-p
    (yas/field-value number)))

(defun yas/inside-string ()
  (equal 'font-lock-string-face (get-char-property (1- (point)) 'face)))

(defun yas/unimplemented ()
  (if yas/current-template
      (if (y-or-n-p "This snippet is unimplemented. Visit the snippet definition? ")
          (yas/visit-snippet-file-1 yas/current-template))
    (message "No implementation.")))


;;; Snippet expansion and field management

(defvar yas/active-field-overlay nil
  "Overlays the currently active field.")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current active field ")

(defconst yas/prefix nil
  "A prefix argument for expansion direct from keybindings")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted on the last snippet expansion.")

(defvar yas/start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas/active-field-overlay)
(make-variable-buffer-local 'yas/field-protection-overlays)
(make-variable-buffer-local 'yas/deleted-text)

(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (fields '())
  (exit nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field
  force-exit)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)

(defstruct (yas/mirror (:constructor yas/make-mirror (start end transform)))
  "A mirror."
  start end
  (transform nil)
  parent-field
  next)

(defstruct (yas/exit (:constructor yas/make-exit (marker)))
  marker
  next)

(defun yas/apply-transform (field-or-mirror field &optional empty-on-nil-p)
  "Calculate transformed string for FIELD-OR-MIRROR from FIELD.

If there is no transform for ht field, return nil.

If there is a transform but it returns nil, return the empty
string iff EMPTY-ON-NIL-P is true."
  (let* ((yas/text (yas/field-text-for-display field))
         (text yas/text)
         (yas/modified-p (yas/field-modified-p field))
         (yas/moving-away-p nil)
         (transform (if (yas/mirror-p field-or-mirror)
                        (yas/mirror-transform field-or-mirror)
                      (yas/field-transform field-or-mirror)))
         (start-point (if (yas/mirror-p field-or-mirror)
                          (yas/mirror-start field-or-mirror)
                        (yas/field-start field-or-mirror)))
         (transformed (and transform
                           (save-excursion
                             (goto-char start-point)
                             (let ((ret (yas/eval-lisp transform)))
                               (or ret (and empty-on-nil-p "")))))))
    transformed))

(defsubst yas/replace-all (from to &optional text)
  "Replace all occurance from FROM to TO.

With optional string TEXT do it in that string."
  (if text
      (replace-regexp-in-string (regexp-quote from) to text t t)
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to t t text))))

(defun yas/snippet-find-field (snippet number)
  (find-if #'(lambda (field)
               (eq number (yas/field-number field)))
           (yas/snippet-fields snippet)))

(defun yas/snippet-sort-fields (snippet)
  "Sort the fields of SNIPPET in navigation order."
  (setf (yas/snippet-fields snippet)
        (sort (yas/snippet-fields snippet)
              #'yas/snippet-field-compare)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the field's start point"
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (or (zerop n2) (and (not (zerop n1))
                                (< n1 n2)))
          (not (zerop n1)))
      (if n2
          (zerop n2)
        (< (yas/field-start field1)
           (yas/field-start field2))))))

(defun yas/field-probably-deleted-p (snippet field)
  "Guess if SNIPPET's FIELD should be skipped."
  (and (zerop (- (yas/field-start field) (yas/field-end field)))
       (or (yas/field-parent-field field)
           (and (eq field (car (last (yas/snippet-fields snippet))))
                (= (yas/field-start field) (overlay-end (yas/snippet-control-overlay snippet)))))
       ;; the field numbered 0, just before the exit marker, should
       ;; never be skipped
       (not (zerop (yas/field-number field)))))

(defun yas/snippets-at-point (&optional all-snippets)
  "Return a sorted list of snippets at point, most recently
inserted first."
  (sort
   (remove nil (remove-duplicates (mapcar #'(lambda (ov)
                                              (overlay-get ov 'yas/snippet))
                                          (if all-snippets
                                              (overlays-in (point-min) (point-max))
                                            (overlays-at (point))))))
   #'(lambda (s1 s2)
       (<= (yas/snippet-id s2) (yas/snippet-id s1)))))

(defun yas/next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point, otherwise
delegate to `yas/next-field'."
  (interactive)
  (if yas/triggers-in-field
      (let ((yas/fallback-behavior 'return-nil)
            (active-field (overlay-get yas/active-field-overlay 'yas/field)))
        (when active-field
          (unless (yas/expand-from-trigger-key active-field)
            (yas/next-field))))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
         (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'(lambda (field)
                                     (and (not (eq field active-field))
                                          (yas/field-probably-deleted-p snippet field)))
                                 (yas/snippet-fields snippet)))
         (active-field-pos (position active-field live-fields))
         (target-pos (and active-field-pos (+ arg active-field-pos)))
         (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;;
    (when (and active-field
               (yas/field-transform active-field))
      (let* ((yas/moving-away-p t)
             (yas/text (yas/field-text-for-display active-field))
             (text yas/text)
             (yas/modified-p (yas/field-modified-p active-field)))
        ;; primary field transform: exit call to field-transform
        (yas/eval-lisp (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD"
  (yas/make-move-field-protection-overlays snippet field)
  (yas/make-move-active-field-overlay snippet field))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (yas/place-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  (let ((number (yas/field-number field)))
    ;; check for the special ${0: ...} field 
    (if (and number (zerop number))
        (progn
          (set-mark (yas/field-end field))
          (setf (yas/snippet-force-exit snippet)
                (or (yas/field-transform field)
                    t)))
      ;; make this field active
      (setf (yas/snippet-active-field snippet) field)
      ;; primary field transform: first call to snippet transform
      (unless (yas/field-modified-p field)
        (if (yas/field-update-display field snippet)
            (let ((inhibit-modification-hooks t))
              (yas/update-mirrors snippet))
          (setf (yas/field-modified-p field) nil))))))

(defun yas/prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field -1))

(defun yas/abort-snippet (&optional snippet)
  (interactive)
  (let ((snippet (or snippet
                     (car (yas/snippets-at-point)))))
    (when snippet
      (setf (yas/snippet-force-exit snippet) t))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET."
  (interactive)
  (setf (yas/snippet-force-exit snippet) t)
  (goto-char (if (yas/snippet-exit snippet)
                 (yas/exit-marker (yas/snippet-exit snippet))
               (overlay-end (yas/snippet-control-overlay snippet)))))

(defun yas/exit-all-snippets ()
  "Exit all snippets."
  (interactive)
  (mapc #'(lambda (snippet)
            (yas/exit-snippet snippet)
            (yas/check-commit-snippet))
        (yas/snippets-at-point)))


;;; Some low level snippet-routines

(defun yas/commit-snippet (snippet)
  "Commit SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text.

Return a buffer position where the point should be placed if
exiting the snippet.

NO-HOOKS means don't run the `yas/after-exit-snippet-hook' hooks."

  (let ((control-overlay (yas/snippet-control-overlay snippet))
        yas/snippet-beg
        yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay))

    (let ((inhibit-modification-hooks t))
      (when yas/active-field-overlay
        (delete-overlay yas/active-field-overlay))
      (when yas/field-protection-overlays
        (mapc #'delete-overlay yas/field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas/snippet-end'...
    ;;
    (let ((previous-field (yas/snippet-previous-active-field snippet)))
      (when (and yas/snippet-end previous-field)
        (yas/advance-end-maybe previous-field yas/snippet-end)))

    ;; Convert all markers to points,
    ;;
    (yas/markers-to-points snippet)

    ;; Take care of snippet revival
    ;;
    (if yas/snippet-revival
        (push `(apply yas/snippet-revive ,yas/snippet-beg ,yas/snippet-end ,snippet)
              buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas/take-care-of-redo'....
      (setf (yas/snippet-fields snippet) nil)))

  (message "[yas] snippet %s exited." (yas/snippet-id snippet)))

(defun yas/check-commit-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up."
  (let* ((snippets (yas/snippets-at-point 'all-snippets))
         (snippets-left snippets)
         (snippet-exit-transform))
    (dolist (snippet snippets)
      (let ((active-field (yas/snippet-active-field snippet)))
        (setq snippet-exit-transform (yas/snippet-force-exit snippet)) 
        (cond ((or snippet-exit-transform
                   (not (and active-field (yas/field-contains-point-p active-field))))
               (setq snippets-left (delete snippet snippets-left))
               (setf (yas/snippet-force-exit snippet) nil)
               (yas/commit-snippet snippet))
              ((and active-field
                    (or (not yas/active-field-overlay)
                        (not (overlay-buffer yas/active-field-overlay))))
               ;;
               ;; stacked expansion: this case is mainly for recent
               ;; snippet exits that place us back int the field of
               ;; another snippet
               ;;
               (save-excursion
                 (yas/move-to-field snippet active-field)
                 (yas/update-mirrors snippet)))
              (t
               nil))))
    (unless snippets-left
      (remove-hook 'post-command-hook 'yas/post-command-handler 'local)
      (remove-hook 'pre-command-hook 'yas/pre-command-handler 'local)
      (if snippet-exit-transform
          (yas/eval-lisp-no-saves snippet-exit-transform)
        (run-hooks 'yas/after-exit-snippet-hook)))))

;; Apropos markers-to-points:
;;
;; This was found useful for performance reasons, so that an
;; excessive number of live markers aren't kept around in the
;; `buffer-undo-list'. However, in `markers-to-points', the
;; set-to-nil markers can't simply be discarded and replaced with
;; fresh ones in `points-to-markers'. The original marker that was
;; just set to nil has to be reused.
;;
;; This shouldn't bring horrible problems with undo/redo, but it
;; you never know
;;
(defun yas/markers-to-points (snippet)
  "Convert all markers in SNIPPET to a cons (POINT . MARKER)
where POINT is the original position of the marker and MARKER is
the original marker object with the position set to nil."
  (dolist (field (yas/snippet-fields snippet))
    (let ((start (marker-position (yas/field-start field)))
          (end (marker-position (yas/field-end field))))
      (set-marker (yas/field-start field) nil)
      (set-marker (yas/field-end field) nil)
      (setf (yas/field-start field) (cons start (yas/field-start field)))
      (setf (yas/field-end field) (cons end (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (let ((start (marker-position (yas/mirror-start mirror)))
            (end (marker-position (yas/mirror-end mirror))))
        (set-marker (yas/mirror-start mirror) nil)
        (set-marker (yas/mirror-end mirror) nil)
        (setf (yas/mirror-start mirror) (cons start (yas/mirror-start mirror)))
        (setf (yas/mirror-end mirror) (cons end (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (let ((exit (marker-position (yas/exit-marker snippet-exit))))
        (set-marker (yas/exit-marker snippet-exit) nil)
        (setf (yas/exit-marker snippet-exit) (cons exit (yas/exit-marker snippet-exit)))))))

(defun yas/points-to-markers (snippet)
  "Convert all cons (POINT . MARKER) in SNIPPET to markers. This
is done by setting MARKER to POINT with `set-marker'."
  (dolist (field (yas/snippet-fields snippet))
    (setf (yas/field-start field) (set-marker (cdr (yas/field-start field))
                                              (car (yas/field-start field))))
    (setf (yas/field-end field) (set-marker (cdr (yas/field-end field))
                                            (car (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (setf (yas/mirror-start mirror) (set-marker (cdr (yas/mirror-start mirror))
                                                  (car (yas/mirror-start mirror))))
      (setf (yas/mirror-end mirror) (set-marker (cdr (yas/mirror-end mirror))
                                                (car (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (setf (yas/exit-marker snippet-exit) (set-marker (cdr (yas/exit-marker snippet-exit))
                                                       (car (yas/exit-marker snippet-exit)))))))

(defun yas/field-contains-point-p (field &optional point)
  (let ((point (or point
                   (point))))
    (and (>= point (yas/field-start field))
         (<= point (yas/field-end field)))))

(defun yas/field-text-for-display (field)
  "Return the propertized display text for field FIELD.  "
  (buffer-substring (yas/field-start field) (yas/field-end field)))

(defun yas/undo-in-progress ()
  "True if some kind of undo is in progress"
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas/make-control-overlay (snippet start end)
  "Creates the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil
                               t)))
    (overlay-put overlay 'keymap yas/keymap)
    (overlay-put overlay 'yas/snippet snippet)
    overlay))

(defun yas/skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (interactive)
  (let ((field (or field
                   (and yas/active-field-overlay
                        (overlay-buffer yas/active-field-overlay)
                        (overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
                (not (yas/field-modified-p field))
                (eq (point) (marker-position (yas/field-start field))))
           (yas/skip-and-clear field)
           (yas/next-field 1))
          (t
           (call-interactively 'delete-char)))))

(defun yas/skip-and-clear (field)
  "Deletes the region of FIELD and sets it modified state to t"
  ;; Just before skipping-and-clearing the field, mark its children
  ;; fields as modified, too. If the childen have mirrors-in-fields
  ;; this prevents them from updating erroneously (we're skipping and
  ;; deleting!).
  ;;
  (yas/mark-this-and-children-modified field)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/mark-this-and-children-modified (field)
  (setf (yas/field-modified-p field) t)
  (let ((fom (yas/field-next field)))
    (while (and fom
                (yas/fom-parent-field fom))
      (when (and (eq (yas/fom-parent-field fom) field)
                 (yas/field-p fom))
        (yas/mark-this-and-children-modified fom))
      (setq fom (yas/fom-next fom)))))

(defun yas/make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas/active-field-overlay
           (overlay-buffer yas/active-field-overlay))
      (move-overlay yas/active-field-overlay
                    (yas/field-start field)
                    (yas/field-end field))
    (setq yas/active-field-overlay
          (make-overlay (yas/field-start field)
                        (yas/field-end field)
                        nil nil t))
    (overlay-put yas/active-field-overlay 'priority 100)
    (overlay-put yas/active-field-overlay 'face 'yas/field-highlight-face)
    (overlay-put yas/active-field-overlay 'yas/snippet snippet)
    (overlay-put yas/active-field-overlay 'modification-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-in-front-hooks
                 '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-behind-hooks
                 '(yas/on-field-overlay-modification))))

(defun yas/on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and it point it
at field start. This hook doesn't do anything if an undo is in
progress."
  (unless (yas/undo-in-progress)
    (let* ((field (overlay-get yas/active-field-overlay 'yas/field))
           (number (and field (yas/field-number field)))
           (snippet (overlay-get yas/active-field-overlay 'yas/snippet)))
      (cond (after?
             (yas/advance-end-maybe field (overlay-end overlay))
             (let ((saved-point (point)))
               (yas/field-update-display field (car (yas/snippets-at-point)))
               (goto-char saved-point))
             (yas/update-mirrors (car (yas/snippets-at-point))))
            (field
             (when (and (not after?)
                        (not (yas/field-modified-p field))
                        (eq (point) (if (markerp (yas/field-start field))
                                        (marker-position (yas/field-start field))
                                      (yas/field-start field))))
               (yas/skip-and-clear field))
             (setf (yas/field-modified-p field) t))))))

;;; Apropos protection overlays:
;;
;; These exist for nasty users who will try to delete parts of the
;; snippet outside the active field. Actual protection happens in
;; `yas/on-protection-overlay-modification'.
;;
;; Currently this signals an error which inhibits the command. For
;; commands that move point (like `kill-line'), point is restored in
;; the `yas/post-command-handler' using a global
;; `yas/protection-violation' variable.
;;
;; Alternatively, I've experimented with an implementation that
;; commits the snippet before actually calling `this-command'
;; interactively, and then signals an eror, which is ignored. but
;; blocks all other million modification hooks. This presented some
;; problems with stacked expansion.
;;

(defun yas/make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas/field-start field))
        (end (yas/field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;;
    (when (< (buffer-size) end)
      (save-excursion
        (let ((inhibit-modification-hooks t))
          (goto-char (point-max))
          (newline))))
    ;; go on to normal overlay creation/moving
    ;;
    (cond ((and yas/field-protection-overlays
                (every #'overlay-buffer yas/field-protection-overlays))
           (move-overlay (first yas/field-protection-overlays) (1- start) start)
           (move-overlay (second yas/field-protection-overlays) end (1+ end)))
          (t
           (setq yas/field-protection-overlays
                 (list (make-overlay (1- start) start nil t nil)
                       (make-overlay end (1+ end) nil t nil)))
           (dolist (ov yas/field-protection-overlays)
             (overlay-put ov 'face 'yas/field-debug-face)
             (overlay-put ov 'yas/snippet snippet)
             ;; (overlay-put ov 'evaporate t)
             (overlay-put ov 'modification-hooks '(yas/on-protection-overlay-modification)))))))

(defvar yas/protection-violation nil
  "When non-nil, signals attempts to erronesly exit or modify the snippet.

Functions in the `post-command-hook', for example
`yas/post-command-handler' can check it and reset its value to
nil. The variables value is the point where the violation
originated")

(defun yas/on-protection-overlay-modification (overlay after? beg end &optional length)
  "Signals a snippet violation, then issues error.

The error should be ignored in `debug-ignored-errors'"
  (cond ((not (or after?
                  (yas/undo-in-progress)))
         (setq yas/protection-violation (point))
         (error "Exit the snippet first!"))))

(add-to-list 'debug-ignored-errors "^Exit the snippet first!$")


;;; Apropos stacked expansion:
;;
;; the parent snippet does not run its fields modification hooks
;; (`yas/on-field-overlay-modification' and
;; `yas/on-protection-overlay-modification') while the child snippet
;; is active. This means, among other things, that the mirrors of the
;; parent snippet are not updated, this only happening when one exits
;; the child snippet.
;;
;; Unfortunately, this also puts some ugly (and not fully-tested)
;; bits of code in `yas/expand-snippet' and
;; `yas/commit-snippet'. I've tried to mark them with "stacked
;; expansion:".
;;
;; This was thought to be safer in in an undo/redo perpective, but
;; maybe the correct implementation is to make the globals
;; `yas/active-field-overlay' and `yas/field-protection-overlays' be
;; snippet-local and be active even while the child snippet is
;; running. This would mean a lot of overlay modification hooks
;; running, but if managed correctly (including overlay priorities)
;; they should account for all situations...
;;

(defun yas/expand-snippet (content &optional start end expand-env)
  "Expand snippet CONTENT at current point.

Text between START and END will be deleted before inserting
template. EXPAND-ENV is are let-style variable to value bindings
considered when expanding the snippet."
  (run-hooks 'yas/before-expand-snippet-hook)

  ;; If a region is active, set `yas/selected-text'
  (setq yas/selected-text
        (when (region-active-p)
          (prog1 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
            (unless start (setq start (region-beginning))
                    (unless end (setq end (region-end)))))))

  (when start
    (goto-char start))

  ;;
  (let ((to-delete (and start end (buffer-substring-no-properties start end)))
        (start (or start (point)))
        (end (or end (point)))
        snippet)
    (setq yas/indent-original-column (current-column))
    ;; Delete the region to delete, this *does* get undo-recorded.
    ;;
    (when (and to-delete
               (> end start))
      (delete-region start end)
      (setq yas/deleted-text to-delete))

    (cond ((listp content)
           ;; x) This is a snippet-command
           ;;
           (yas/eval-lisp-no-saves content))
          (t
           ;; x) This is a snippet-snippet :-)
           ;;
           ;;    Narrow the region down to the content, shoosh the
           ;;    `buffer-undo-list', and create the snippet, the new
           ;;    snippet updates its mirrors once, so we are left with
           ;;    some plain text.  The undo action for deleting this
           ;;    plain text will get recorded at the end.
           ;;
           ;;    stacked expansion: also shoosh the overlay modification hooks
           (save-restriction
             (narrow-to-region start start)
             (let ((inhibit-modification-hooks t)
                   (buffer-undo-list t))
               ;; snippet creation might evaluate users elisp, which
               ;; might generate errors, so we have to be ready to catch
               ;; them mostly to make the undo information
               ;;
               (setq yas/start-column (save-restriction (widen) (current-column)))

               (setq snippet
                     (if expand-env
                         (eval `(let ,expand-env
                                  (insert content)
                                  (yas/snippet-create (point-min) (point-max))))
                       (insert content)
                       (yas/snippet-create (point-min) (point-max))))))

           ;; stacked-expansion: This checks for stacked expansion, save the
           ;; `yas/previous-active-field' and advance its boudary.
           ;;
           (let ((existing-field (and yas/active-field-overlay
                                      (overlay-buffer yas/active-field-overlay)
                                      (overlay-get yas/active-field-overlay 'yas/field))))
             (when existing-field
               (setf (yas/snippet-previous-active-field snippet) existing-field)
               (yas/advance-end-maybe existing-field (overlay-end yas/active-field-overlay))))

           ;; Exit the snippet immediately if no fields
           ;;
           (unless (yas/snippet-fields snippet)
             (yas/exit-snippet snippet))

           ;; Push two undo actions: the deletion of the inserted contents of
           ;; the new snippet (without the "key") followed by an apply of
           ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
           ;;
           ;; A small exception, if `yas/also-auto-indent-first-line'
           ;; is t and `yas/indent' decides to indent the line to a
           ;; point before the actual expansion point, undo would be
           ;; messed up. We call the early point "newstart"".  case,
           ;; and attempt to fix undo.
           ;;
           (let ((newstart (overlay-start (yas/snippet-control-overlay snippet)))
                 (end (overlay-end (yas/snippet-control-overlay snippet))))
             (when (< newstart start)
               (push (cons (make-string (- start newstart) ? ) newstart) buffer-undo-list))
             (push (cons newstart end) buffer-undo-list)
             (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
                   buffer-undo-list))
           ;; Now, schedule a move to the first field
           ;;
           (let ((first-field (car (yas/snippet-fields snippet))))
             (when first-field
               (sit-for 0) ;; fix issue 125
               (yas/move-to-field snippet first-field)))
           (message "[yas] snippet expanded.")
           t))))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas/snippet-fields snippet)
    (yas/commit-snippet snippet)))

(defun yas/snippet-revive (beg end snippet)
  "Revives the SNIPPET and creates a control overlay from BEG to
END.

BEG and END are, we hope, the original snippets boudaries. All
the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas/take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  ;;
  (yas/points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  ;;
  (let ((target-field (or (yas/snippet-active-field snippet)
                          (car (yas/snippet-fields snippet)))))
    (when target-field
      (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet beg end))
      (overlay-put (yas/snippet-control-overlay snippet) 'yas/snippet snippet)

      (yas/move-to-field snippet target-field)

      (add-hook 'post-command-hook 'yas/post-command-handler nil t)
      (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

      (push `(apply yas/take-care-of-redo ,beg ,end ,snippet)
            buffer-undo-list))))

(defun yas/snippet-create (begin end)
  "Creates a snippet from an template inserted between BEGIN and END.

Returns the newly created snippet."
  (let ((snippet (yas/make-snippet)))
    (goto-char begin)
    (yas/snippet-parse-create snippet)

    ;; Sort and link each field
    (yas/snippet-sort-fields snippet)

    ;; Create keymap overlay for snippet
    (setf (yas/snippet-control-overlay snippet)
          (yas/make-control-overlay snippet (point-min) (point-max)))

    ;; Move to end
    (goto-char (point-max))

    ;; Setup hooks
    (add-hook 'post-command-hook 'yas/post-command-handler nil t)
    (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

    snippet))


;;; Apropos adjacencies and "fom's":
;;
;; Once the $-constructs bits like "$n" and "${:n" are deleted in the
;; recently expanded snippet, we might actually have many fields,
;; mirrors (and the snippet exit) in the very same position in the
;; buffer. Therefore we need to single-link the
;; fields-or-mirrors-or-exit, which I have called "fom", according to
;; their original positions in the buffer.
;;
;; Then we have operation `yas/advance-end-maybe' and
;; `yas/advance-start-maybe', which conditionally push the starts and
;; ends of these foms down the chain.
;;
;; This allows for like the printf with the magic ",":
;;
;;   printf ("${1:%s}\\n"${1:$(if (string-match "%" text) "," "\);")}  \
;;   $2${1:$(if (string-match "%" text) "\);" "")}$0
;;
(defun yas/fom-start (fom)
  (cond ((yas/field-p fom)
         (yas/field-start fom))
        ((yas/mirror-p fom)
         (yas/mirror-start fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-end (fom)
  (cond ((yas/field-p fom)
         (yas/field-end fom))
        ((yas/mirror-p fom)
         (yas/mirror-end fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-next (fom)
  (cond ((yas/field-p fom)
         (yas/field-next fom))
        ((yas/mirror-p fom)
         (yas/mirror-next fom))
        (t
         (yas/exit-next fom))))

(defun yas/fom-parent-field (fom)
  (cond ((yas/field-p fom)
         (yas/field-parent-field fom))
        ((yas/mirror-p fom)
         (yas/mirror-parent-field fom))
        (t
         nil)))

(defun yas/calculate-adjacencies (snippet)
  "Calculate adjacencies for fields or mirrors of SNIPPET.

This is according to their relative positions in the buffer, and
has to be called before the $-constructs are deleted."
  (flet ((yas/fom-set-next-fom (fom nextfom)
                               (cond ((yas/field-p fom)
                                      (setf (yas/field-next fom) nextfom))
                                     ((yas/mirror-p fom)
                                      (setf (yas/mirror-next fom) nextfom))
                                     (t
                                      (setf (yas/exit-next fom) nextfom))))
         (yas/compare-fom-begs (fom1 fom2)
                               (if (= (yas/fom-start fom2) (yas/fom-start fom1))
                                   (yas/mirror-p fom2)
                                 (>= (yas/fom-start fom2) (yas/fom-start fom1))))
         (yas/link-foms (fom1 fom2)
                        (yas/fom-set-next-fom fom1 fom2)))
    ;; make some yas/field, yas/mirror and yas/exit soup
    (let ((soup))
      (when (yas/snippet-exit snippet)
        (push (yas/snippet-exit snippet) soup))
      (dolist (field (yas/snippet-fields snippet))
        (push field soup)
        (dolist (mirror (yas/field-mirrors field))
          (push mirror soup)))
      (setq soup
            (sort soup
                  #'yas/compare-fom-begs))
      (when soup
        (reduce #'yas/link-foms soup)))))

(defun yas/calculate-mirrors-in-fields (snippet mirror)
  "Attempt to assign a parent field of SNIPPET to the mirror MIRROR.

Use the tighest containing field if more than one field contains
the mirror. Intended to be called *before* the dollar-regions are
deleted."
  (let ((min (point-min))
        (max (point-max)))
    (dolist (field (yas/snippet-fields snippet))
      (when (and (<= (yas/field-start field) (yas/mirror-start mirror))
                 (<= (yas/mirror-end mirror) (yas/field-end field))
               (< min (yas/field-start field))
               (< (yas/field-end field) max))
          (setq min (yas/field-start field)
                max (yas/field-end field))
          (setf (yas/mirror-parent-field mirror) field)))))

(defun yas/advance-end-maybe (fom newend)
  "Maybe advance FOM's end to NEWEND if it needs it.

If it does, also:

* call `yas/advance-start-maybe' on FOM's next fom.

* in case FOM is field call `yas/advance-end-maybe' on its parent
  field

Also, if FOM is an exit-marker, always call
`yas/advance-start-maybe' on its next fom. This is beacuse
exit-marker have identical start and end markers.

"
  (cond ((and fom (< (yas/fom-end fom) newend))
         (set-marker (yas/fom-end fom) newend)
         (yas/advance-start-maybe (yas/fom-next fom) newend)
         (let ((parent (yas/fom-parent-field fom)))
           (when parent
             (yas/advance-end-maybe parent newend))))
        ((yas/exit-p fom)
         (yas/advance-start-maybe (yas/fom-next fom) newend))))

(defun yas/advance-start-maybe (fom newstart)
  "Maybe advance FOM's start to NEWSTART if it needs it.

If it does, also call `yas/advance-end-maybe' on FOM."
  (when (and fom (< (yas/fom-start fom) newstart))
    (set-marker (yas/fom-start fom) newstart)
    (yas/advance-end-maybe fom newstart)))

(defun yas/advance-end-of-parents-maybe (field newend)
  "Like `yas/advance-end-maybe' but for parents."
  (when (and field
             (< (yas/field-end field) newend))
    (set-marker (yas/field-end field) newend)
    (yas/advance-end-of-parents-maybe (yas/field-parent-field field) newend)))

(defvar yas/dollar-regions nil
  "When expanding the snippet the \"parse-create\" functions add
  cons cells to this var")

(defun yas/snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((parse-start (point)))
    ;; Reset the yas/dollar-regions
    ;;
    (setq yas/dollar-regions nil)
    ;; protect escaped quote, backquotes and backslashes
    ;;
    (yas/protect-escapes nil '(?\\ ?` ?'))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas/replace-backquotes)
    ;; protect escapes again since previous steps might have generated
    ;; more characters needing escaping
    ;;
    (goto-char parse-start)
    (yas/protect-escapes)
    ;; parse fields with {}
    ;;
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors and fields
    ;;
    (goto-char parse-start)
    (yas/simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas/transform-mirror-parse-create snippet)
    ;; calculate adjacencies of fields and mirrors
    ;;
    (yas/calculate-adjacencies snippet)
    ;; Delete $-constructs
    ;;
    (yas/delete-regions yas/dollar-regions)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas/restore-escapes)
    ;; update mirrors for the first time
    ;;
    (yas/update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent-according-to-mode (snippet-markers)
  "Indent current line according to mode, preserving
SNIPPET-MARKERS."
  ;;; Apropos indenting problems....
  ;;
  ;; `indent-according-to-mode' uses whatever `indent-line-function'
  ;; is available. Some implementations of these functions delete text
  ;; before they insert. If there happens to be a marker just after
  ;; the text being deleted, the insertion actually happens after the
  ;; marker, which misplaces it.
  ;;
  ;; This would also happen if we had used overlays with the
  ;; `front-advance' property set to nil.
  ;;
  ;; This is why I have these `trouble-markers', they are the ones at
  ;; they are the ones at the first non-whitespace char at the line
  ;; (i.e. at `yas/real-line-beginning'. After indentation takes place
  ;; we should be at the correct to restore them to. All other
  ;; non-trouble-markers have been *pushed* and don't need special
  ;; attention.
  ;;
  (goto-char (yas/real-line-beginning))
  (let ((trouble-markers (remove-if-not #'(lambda (marker)
                                            (= marker (point)))
                                        snippet-markers)))
    (save-restriction
      (widen)
      (condition-case err
          (indent-according-to-mode)
        (error (message "[yas] warning: yas/indent-according-to-mode habing problems running %s" indent-line-function)
               nil)))
    (mapc #'(lambda (marker)
              (set-marker marker (point)))
          trouble-markers)))

(defvar yas/indent-original-column nil)
(defun yas/indent (snippet)
  (let ((snippet-markers (yas/collect-snippet-markers snippet)))
    ;; Look for those $>
    (save-excursion
      (while (re-search-forward "$>" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (when (not (eq yas/indent-line 'auto))
          (yas/indent-according-to-mode snippet-markers))))
    ;; Now do stuff for 'fixed and 'auto
    (save-excursion
      (cond ((eq yas/indent-line 'fixed)
             (while (and (zerop (forward-line))
                         (zerop (current-column)))
               (indent-to-column yas/indent-original-column)))
            ((eq yas/indent-line 'auto)
             (let ((end (set-marker (make-marker) (point-max)))
                   (indent-first-line-p yas/also-auto-indent-first-line))
               (while (and (zerop (if indent-first-line-p
                                      (prog1
                                          (forward-line 0)
                                        (setq indent-first-line-p nil))
                                    (forward-line 1)))
                           (not (eobp))
                           (<= (point) end))
                 (yas/indent-according-to-mode snippet-markers))))
            (t
             nil)))))

(defun yas/collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (dolist (field (yas/snippet-fields snippet))
      (push (yas/field-start field) markers)
      (push (yas/field-end field) markers)
      (dolist (mirror (yas/field-mirrors field))
        (push (yas/mirror-start mirror) markers)
        (push (yas/mirror-end mirror) markers)))
    (let ((snippet-exit (yas/snippet-exit snippet)))
      (when (and snippet-exit
                 (marker-buffer (yas/exit-marker snippet-exit)))
        (push (yas/exit-marker snippet-exit) markers)))
    markers))

(defun yas/real-line-beginning ()
  (let ((c (char-after (line-beginning-position)))
        (n (line-beginning-position)))
    (while (or (eql c ?\ )
               (eql c ?\t))
      (incf n)
      (setq c (char-after n)))
    n))

(defun yas/escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas/protect-escapes (&optional text escaped)
  "Protect all escaped characters with their numeric ASCII value.

With optional string TEXT do it in string instead of buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (concat "\\" (char-to-string escaped))
                                     (yas/escape-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/restore-escapes (&optional text escaped)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead of the buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (yas/escape-string escaped)
                                     (char-to-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
	with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
    (let ((current-string (match-string 1)) transformed)
      (delete-region (match-beginning 0) (match-end 0))
      (setq transformed (yas/eval-lisp (yas/read-lisp (yas/restore-escapes current-string))))
      (goto-char (match-beginning 0))
      (when transformed (insert transformed)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
        (scan-sexps from count))
    (error
     nil)))

(defun yas/make-marker (pos)
  "Create a marker at POS with `nil' `marker-insertion-type'"
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse most field expressions, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;

* \"${n: text$(expression)}, the same with a lisp expression;
  this is caught with the curiously named `yas/multi-dollar-lisp-expression-regexp'

* the same as above but unnumbered, (no N:) and number is calculated automatically.

When multiple expressions are found, only the last one counts."
  ;;
  (save-excursion
    (while (re-search-forward yas/field-regexp nil t)
      (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
             (number (and (match-string-no-properties 1)
                          (string-to-number (match-string-no-properties 1))))
             (brand-new-field (and real-match-end-0
                                   ;; break if on "$(" immediately
                                   ;; after the ":", this will be
                                   ;; caught as a mirror with
                                   ;; transform later.
                                   (not (save-match-data
                                          (eq (string-match "$[ \t\n]*("
                                                            (match-string-no-properties 2)) 0)))
                                   ;; allow ${0: some exit text}
                                   ;; (not (and number (zerop number)))
                                   (yas/make-field number
                                                   (yas/make-marker (match-beginning 2))
                                                   (yas/make-marker (1- real-match-end-0))
                                                   parent-field))))
        (when brand-new-field
          (goto-char real-match-end-0)
          (push (cons (1- real-match-end-0) real-match-end-0)
                yas/dollar-regions)
          (push (cons (match-beginning 0) (match-beginning 2))
                yas/dollar-regions)
          (push brand-new-field (yas/snippet-fields snippet))
          (save-excursion
            (save-restriction
              (narrow-to-region (yas/field-start brand-new-field) (yas/field-end brand-new-field))
              (goto-char (point-min))
              (yas/field-parse-create snippet brand-new-field)))))))
  ;; if we entered from a parent field, now search for the
  ;; `yas/multi-dollar-lisp-expression-regexp'. THis is used for
  ;; primary field transformations
  ;;
  (when parent-field
    (save-excursion
      (while (re-search-forward yas/multi-dollar-lisp-expression-regexp nil t)
        (let* ((real-match-end-1 (yas/scan-sexps (match-beginning 1) 1)))
          ;; commit the primary field transformation if:
          ;;
          ;; 1. we don't find it in yas/dollar-regions (a subnested
          ;; field) might have already caught it.
          ;;
          ;; 2. we really make sure we have either two '$' or some
          ;; text and a '$' after the colon ':'. This is a FIXME: work
          ;; my regular expressions and end these ugly hacks.
          ;;
          (when (and real-match-end-1
                     (not (member (cons (match-beginning 0)
                                        real-match-end-1)
                                  yas/dollar-regions))
                     (not (eq ?:
                              (char-before (1- (match-beginning 1))))))
            (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1)
                                                                          real-match-end-1)))
              (setf (yas/field-transform parent-field)
                    (yas/read-lisp (yas/restore-escapes lisp-expression-string))))
            (push (cons (match-beginning 0) real-match-end-1)
                  yas/dollar-regions)))))))

(defun yas/transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations."
  (while (re-search-forward yas/transform-mirror-regexp nil t)
    (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
           (number (string-to-number (match-string-no-properties 1)))
           (field (and number
                       (not (zerop number))
                       (yas/snippet-find-field snippet number)))
           (brand-new-mirror
            (and real-match-end-0
                 field
                 (yas/make-mirror (yas/make-marker (match-beginning 0))
                                  (yas/make-marker (match-beginning 0))
                                  (yas/read-lisp
                                   (yas/restore-escapes
                                    (buffer-substring-no-properties (match-beginning 2)
                                                                    (1- real-match-end-0))))))))
      (when brand-new-mirror
        (push brand-new-mirror
              (yas/field-mirrors field))
        (yas/calculate-mirrors-in-fields snippet brand-new-mirror)
        (push (cons (match-beginning 0) real-match-end-0) yas/dollar-regions)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" fields/mirrors/exitmarkers."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
    (let ((number (string-to-number (match-string-no-properties 1))))
      (cond ((zerop number)

             (setf (yas/snippet-exit snippet)
                   (yas/make-exit (yas/make-marker (match-end 0))))
             (save-excursion
               (goto-char (match-beginning 0))
               (when yas/wrap-around-region
                 (cond (yas/selected-text
                        (insert yas/selected-text))
                       ((and (eq yas/wrap-around-region 'cua)
                             cua-mode
                             (get-register ?0))
                        (insert (prog1 (get-register ?0)
                                  (set-register ?0 nil))))))
               (push (cons (point) (yas/exit-marker (yas/snippet-exit snippet)))
                     yas/dollar-regions)))
            (t
             (let ((field (yas/snippet-find-field snippet number)))
               (if field
                   (let ((brand-new-mirror (yas/make-mirror
                                            (yas/make-marker (match-beginning 0))
                                            (yas/make-marker (match-beginning 0))
                                            nil)))
                     (push brand-new-mirror
                           (yas/field-mirrors field))
                     (yas/calculate-mirrors-in-fields snippet brand-new-mirror))
                 (push (yas/make-field number
                                       (yas/make-marker (match-beginning 0))
                                       (yas/make-marker (match-beginning 0))
                                       nil)
                       (yas/snippet-fields snippet))))
             (push (cons (match-beginning 0) (match-end 0))
                   yas/dollar-regions))))))

(defun yas/delete-regions (regions)
  "Sort disjuct REGIONS by start point, then delete from the back."
  (mapc #'(lambda (reg)
            (delete-region (car reg) (cdr reg)))
        (sort regions
              #'(lambda (r1 r2)
                  (>= (car r1) (car r2))))))

(defun yas/update-mirrors (snippet)
  "Updates all the mirrors of SNIPPET."
  (save-excursion
    (let* ((fields (copy-list (yas/snippet-fields snippet)))
           (field (car fields)))
      (while field
        (dolist (mirror (yas/field-mirrors field))
          ;; stacked expansion: I added an `inhibit-modification-hooks'
          ;; here, for safety, may need to remove if we the mechanism is
          ;; altered.
          ;;
          (let ((inhibit-modification-hooks t)
                (mirror-parent-field (yas/mirror-parent-field mirror)))
            ;; updatte this mirror
            ;;
            (yas/mirror-update-display mirror field)
            ;; for mirrors-in-fields: schedule a possible
            ;; parent field for reupdting later on
            ;;
            (when mirror-parent-field
              (add-to-list 'fields mirror-parent-field 'append #'eq))
            ;; `yas/place-overlays' is needed if the active field and
            ;; protected overlays have been changed because of insertions
            ;; in `yas/mirror-update-display'
            ;;
            (when (eq field (yas/snippet-active-field snippet))
              (yas/place-overlays snippet field))))
        (setq fields (cdr fields))
        (setq field (car fields))))))

(defun yas/mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."

  (let* ((mirror-parent-field (yas/mirror-parent-field mirror))
         (reflection (and (not (and mirror-parent-field
                                    (yas/field-modified-p mirror-parent-field)))
                          (or (yas/apply-transform mirror field 'empty-on-nil)
                              (yas/field-text-for-display field)))))
    (when (and reflection
               (not (string= reflection (buffer-substring-no-properties (yas/mirror-start mirror)
                                                                        (yas/mirror-end mirror)))))
      (goto-char (yas/mirror-start mirror))
      (insert reflection)
      (if (> (yas/mirror-end mirror) (point))
          (delete-region (point) (yas/mirror-end mirror))
        (set-marker (yas/mirror-end mirror) (point))
        (yas/advance-start-maybe (yas/mirror-next mirror) (point))
        ;; super-special advance
        (yas/advance-end-of-parents-maybe mirror-parent-field (point))))))

(defun yas/field-update-display (field snippet)
  "Much like `yas/mirror-update-display', but for fields"
  (when (yas/field-transform field)
    (let ((inhibit-modification-hooks t)
          (transformed (and (not (eq (yas/field-number field) 0))
                            (yas/apply-transform field field)))
          (point (point)))
      (when (and transformed
                 (not (string= transformed (buffer-substring-no-properties (yas/field-start field)
                                                                           (yas/field-end field)))))
        (setf (yas/field-modified-p field) t)
        (goto-char (yas/field-start field))
        (insert transformed)
        (if (> (yas/field-end field) (point))
            (delete-region (point) (yas/field-end field))
          (set-marker (yas/field-end field) (point))
          (yas/advance-start-maybe (yas/field-next field) (point)))
        t))))


;;; Pre- and post-command hooks:

(defvar yas/post-command-runonce-actions nil
  "List of actions to run once  `post-command-hook'.

Each element of this list looks like (FN . ARGS) where FN is
called with ARGS as its arguments after the currently executing
snippet command.

After all actions have been run, this list is emptied, and after
that the rest of `yas/post-command-handler' runs.")

(defun yas/pre-command-handler () )

(defun yas/post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (when yas/post-command-runonce-actions
    (condition-case err
        (mapc #'(lambda (fn-and-args)
                  (apply (car fn-and-args)
                         (cdr fn-and-args)))
              yas/post-command-runonce-actions)
      (error (message "[yas] problem running `yas/post-command-runonce-actions'!")))
    (setq yas/post-command-runonce-actions nil))
  (cond (yas/protection-violation
         (goto-char yas/protection-violation)
         (setq yas/protection-violation nil))
        ((eq 'undo this-command)
         ;;
         ;; After undo revival the correct field is sometimes not
         ;; restored correctly, this condition handles that
         ;;
         (let* ((snippet (car (yas/snippets-at-point)))
                (target-field (and snippet
                                   (find-if-not #'(lambda (field)
                                                    (yas/field-probably-deleted-p snippet field))
                                                (remove nil
                                                        (cons (yas/snippet-active-field snippet)
                                                              (yas/snippet-fields snippet)))))))
           (when target-field
             (yas/move-to-field snippet target-field))))
        ((not (yas/undo-in-progress))
         ;; When not in an undo, check if we must commit the snippet
         ;; (user exited it).
         (yas/check-commit-snippet))))

;;; Fancy docs:

(put 'yas/expand  'function-documentation
     '(yas/expand-from-trigger-key-doc))
(defun yas/expand-from-trigger-key-doc ()
  "A doc synthethizer for `yas/expand-from-trigger-key-doc'."
  (let ((fallback-description
         (cond ((eq yas/fallback-behavior 'call-other-command)
                (let* ((yas/minor-mode nil)
                       (fallback (key-binding (read-kbd-macro yas/trigger-key))))
                  (or (and fallback
                           (format " call command `%s'." (pp-to-string fallback)))
                      " do nothing.")))
               ((eq yas/fallback-behavior 'return-nil)
                ", do nothing.")
               (t
                ", defer to `yas/fallback-behaviour' :-)"))))
    (concat "Expand a snippet before point. If no snippet
expansion is possible,"
            fallback-description
            "\n\nOptional argument FIELD is for non-interactive use and is an
object satisfying `yas/field-p' to restrict the expansion to.")))

(put 'yas/expand-from-keymap  'function-documentation '(yas/expand-from-keymap-doc))
(defun yas/expand-from-keymap-doc ()
  "A doc synthethizer for `yas/expand-from-keymap-doc'."
  (add-hook 'temp-buffer-show-hook 'yas/snippet-description-finish-runonce)
  (concat "Expand/run snippets from keymaps, possibly falling back to original binding.\n"
          (when (eq this-command 'describe-key)
            (let* ((vec (this-single-command-keys))
                   (templates (mapcan #'(lambda (table)
                                          (yas/fetch table vec))
                                      (yas/get-snippet-tables)))
                   (yas/direct-keymaps nil)
                   (fallback (key-binding vec)))
              (concat "In this case, "
                      (when templates
                        (concat "these snippets are bound to this key:\n"
                                (yas/template-pretty-list templates)
                                "\n\nIf none of these expands, "))
                      (or (and fallback
                               (format "fallback `%s' will be called." (pp-to-string fallback)))
                          "no fallback keybinding is called."))))))

(defun yas/template-pretty-list (templates)
  (let ((acc)
        (yas/buffer-local-condition 'always))
    (dolist (plate templates)
      (setq acc (concat acc "\n*) "
                        (propertize (concat "\\\\snippet `" (car plate) "'")
                                    'yasnippet (cdr plate)))))
    acc))

(define-button-type 'help-snippet-def
  :supertype 'help-xref
  'help-function (lambda (template) (yas/visit-snippet-file-1 template))
  'help-echo (purecopy "mouse-2, RET: find snippets's definition"))

(defun yas/snippet-description-finish-runonce ()
  "Final adjustments for the help buffer when snippets are concerned."
  (yas/create-snippet-xrefs)
  (remove-hook 'temp-buffer-show-hook 'yas/snippet-description-finish-runonce))

(defun yas/create-snippet-xrefs ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\\\\\snippet[ \s\t]+`\\([^']+\\)'" nil t)
      (let ((template (get-text-property (match-beginning 1)
                                         'yasnippet)))
        (when template
          (help-xref-button 1 'help-snippet-def template)
          (kill-region (match-end 1) (match-end 0))
          (kill-region (match-beginning 0) (match-beginning 1)))))))

(defun yas/expand-uuid (mode-symbol uuid &optional start end expand-env)
  "Expand a snippet registered in MODE-SYMBOL's table with UUID.

Remaining args as in `yas/expand-snippet'."
  (let* ((table (gethash mode-symbol yas/tables))
         (yas/current-template (and table
                                    (gethash uuid (yas/table-uuidhash table)))))
    (when yas/current-template
      (yas/expand-snippet (yas/template-content yas/current-template)))))


;;; Some hacks:
;; `locate-dominating-file' is added for compatibility in emacs < 23
(unless (or (eq emacs-major-version 23)
            (fboundp 'locate-dominating-file))
  (defvar locate-dominating-stop-dir-regexp
    "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
    "Regexp of directory names which stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file' which will stop its search
when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which filenames are interpreted as hostnames,
or mount points potentially requiring authentication as a different user.")

  (defun locate-dominating-file (file name)
    "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found."
    ;; We used to use the above locate-dominating-files code, but the
    ;; directory-files call is very costly, so we're much better off doing
    ;; multiple calls using the code in here.
    ;;
    ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
    ;; `name' in /home or in /.
    (setq file (abbreviate-file-name file))
    (let ((root nil)
          (prev-file file)
          ;; `user' is not initialized outside the loop because
          ;; `file' may not exist, so we may have to walk up part of the
          ;; hierarchy before we find the "initial UUID".
          (user nil)
          try)
      (while (not (or root
                      (null file)
                      ;; FIXME: Disabled this heuristic because it is sometimes
                      ;; inappropriate.
                      ;; As a heuristic, we stop looking up the hierarchy of
                      ;; directories as soon as we find a directory belonging
                      ;; to another user.  This should save us from looking in
                      ;; things like /net and /afs.  This assumes that all the
                      ;; files inside a project belong to the same user.
                      ;; (let ((prev-user user))
                      ;;   (setq user (nth 2 (file-attributes file)))
                      ;;   (and prev-user (not (equal user prev-user))))
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq try (file-exists-p (expand-file-name name file)))
        (cond (try (setq root file))
              ((equal file (setq prev-file file
                                 file (file-name-directory
                                       (directory-file-name file))))
               (setq file nil))))
      root)))

;; `c-neutralize-syntax-in-CPP` sometimes fires "End of Buffer" error
;; (when it execute forward-char) and interrupt the after change
;; hook. Thus prevent the insert-behind hook of yasnippet to be
;; invoked. Here's a way to reproduce it:

;; # open a *new* Emacs.
;; # load yasnippet.
;; # open a *new* .cpp file.
;; # input "inc" and press TAB to expand the snippet.
;; # select the `#include <...>` snippet.
;; # type inside `<>`

(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
             (dolist (k '(":" ">" ";" "<" "{" "}"))
               (define-key (symbol-value (make-local-variable 'yas/keymap))
                 k 'self-insert-command))))

(provide 'yasnippet)

;;; yasnippet.el ends here

;;; dropdown-list.el --- Drop-down menu interface
;;
;; Filename: dropdown-list.el
;; Description: Drop-down menu interface
;; Author: Jaeyoun Chung [jay.chung@gmail.com]
;; Maintainer:
;; Copyright (C) 2008 Jaeyoun Chung
;; Created: Sun Mar 16 11:20:45 2008 (Pacific Daylight Time)
;; Version:
;; Last-Updated: Sun Mar 16 12:19:49 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dropdown-list.el
;; Keywords: convenience menu
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  According to Jaeyoun Chung, "overlay code stolen from company-mode.el."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/16 dadams
;;     Clean-up - e.g. use char-to-string for control chars removed by email posting.
;;     Moved example usage code (define-key*, command-selector) inside the library.
;;     Require cl.el at byte-compile time.
;;     Added GPL statement.
;; 2008/01/06 Jaeyoun Chung
;;     Posted to gnu-emacs-sources@gnu.org at 9:10 p.m.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; decf, fourth, incf, loop, mapcar*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dropdown-list-face
  '((t :inherit default :background "lightyellow" :foreground "black"))
  "*Bla." :group 'dropdown-list)

(defface dropdown-list-selection-face
  '((t :inherit dropdown-list-face :background "purple"))
  "*Bla." :group 'dropdown-list)

(defvar dropdown-list-overlays nil)

(defun dropdown-list-hide ()
  (while dropdown-list-overlays
    (delete-overlay (pop dropdown-list-overlays))))

(defun dropdown-list-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2 (overlay-put ov prop2 value2)))
    ov))

(defun dropdown-list-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0) (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string (propertize before-string 'face 'default)))
      (setq after-string (when after-string (propertize after-string 'face 'default))))
    (let ((string (concat before-string replacement after-string)))
      (if no-insert
          string
        (push (dropdown-list-put-overlay beg-point end-point 'invisible t
                                         'after-string string)
              dropdown-list-overlays)))))

(defun dropdown-list-start-column (display-width)
  (let ((column (mod (current-column) (window-width)))
        (width (window-width)))
    (cond ((<= (+ column display-width) width) column)
          ((> column display-width) (- column display-width))
          ((>= width display-width) (- width display-width))
          (t nil))))

(defun dropdown-list-move-to-start-line (candidate-count)
  (decf candidate-count)
  (let ((above-line-count (save-excursion (- (vertical-motion (- candidate-count)))))
        (below-line-count (save-excursion (vertical-motion candidate-count))))
    (cond ((= below-line-count candidate-count)
           t)
          ((= above-line-count candidate-count)
           (vertical-motion (- candidate-count))
           t)
          ((>= (+ below-line-count above-line-count) candidate-count)
           (vertical-motion (- (- candidate-count below-line-count)))
           t)
          (t nil))))

(defun dropdown-list-at-point (candidates &optional selidx)
  (dropdown-list-hide)
  (let* ((lengths (mapcar #'length candidates))
         (max-length (apply #'max lengths))
         (start (dropdown-list-start-column (+ max-length 3)))
         (i -1)
         (candidates (mapcar* (lambda (candidate length)
                                (let ((diff (- max-length length)))
                                  (propertize
                                   (concat (if (> diff 0)
                                               (concat candidate (make-string diff ? ))
                                             (substring candidate 0 max-length))
                                           (format "%3d" (+ 2 i)))
                                   'face (if (eql (incf i) selidx)
                                             'dropdown-list-selection-face
                                           'dropdown-list-face))))
                              candidates
                              lengths)))
    (save-excursion
      (and start
           (dropdown-list-move-to-start-line (length candidates))
           (loop initially (vertical-motion 0)
                 for candidate in candidates
                 do (dropdown-list-line (+ (current-column) start) candidate)
                 while (/= (vertical-motion 1) 0)
                 finally return t)))))

(defun dropdown-list (candidates)
  (let ((selection)
        (temp-buffer))
    (save-window-excursion
      (unwind-protect
          (let ((candidate-count (length candidates))
                done key (selidx 0))
            (while (not done)
              (unless (dropdown-list-at-point candidates selidx)
                (switch-to-buffer (setq temp-buffer (get-buffer-create "*selection*"))
                                  'norecord)
                (delete-other-windows)
                (delete-region (point-min) (point-max))
                (insert (make-string (length candidates) ?\n))
                (goto-char (point-min))
                (dropdown-list-at-point candidates selidx))
              (setq key (read-key-sequence ""))
              (cond ((and (stringp key)
                          (>= (aref key 0) ?1)
                          (<= (aref key 0) (+ ?0 (min 9 candidate-count))))
                     (setq selection (- (aref key 0) ?1)
                           done      t))
                    ((member key `(,(char-to-string ?\C-p) [up] "p"))
                     (setq selidx (mod (+ candidate-count (1- (or selidx 0)))
                                       candidate-count)))
                    ((member key `(,(char-to-string ?\C-n) [down] "n"))
                     (setq selidx (mod (1+ (or selidx -1)) candidate-count)))
                    ((member key `(,(char-to-string ?\f))))
                    ((member key `(,(char-to-string ?\r) [return]))
                     (setq selection selidx
                           done      t))
                    (t (setq done t)))))
        (dropdown-list-hide)
        (and temp-buffer (kill-buffer temp-buffer)))
      ;;     (when selection
      ;;       (message "your selection => %d: %s" selection (nth selection candidates))
      ;;       (sit-for 1))
      selection)))

(defun define-key* (keymap key command)
  "Add COMMAND to the multiple-command binding of KEY in KEYMAP.
Use multiple times to bind different COMMANDs to the same KEY."
  (define-key keymap key (combine-command command (lookup-key keymap key))))

(defun combine-command (command defs)
  "$$$$$ FIXME - no doc string"
  (cond ((null defs) command)
        ((and (listp defs)
              (eq 'lambda (car defs))
              (= (length defs) 4)
              (listp (fourth defs))
              (eq 'command-selector (car (fourth defs))))
         (unless (member `',command (cdr (fourth defs)))
           (setcdr (fourth defs) (nconc (cdr (fourth defs)) `(',command))))
         defs)
        (t
         `(lambda () (interactive) (command-selector ',defs ',command)))))

(defvar command-selector-last-command nil "$$$$$ FIXME - no doc string")

(defun command-selector (&rest candidates)
  "$$$$$ FIXME - no doc string"
  (if (and (eq last-command this-command) command-selector-last-command)
      (call-interactively command-selector-last-command)
    (let* ((candidate-strings
            (mapcar (lambda (candidate)
                      (format "%s" (if (symbolp candidate)
                                       candidate
                                     (let ((s (format "%s" candidate)))
                                       (if (>= (length s) 7)
                                           (concat (substring s 0 7) "...")
                                         s)))))
                    candidates))
           (selection (dropdown-list candidate-strings)))
      (when selection
        (let ((cmd (nth selection candidates)))
          (call-interactively cmd)
          (setq command-selector-last-command cmd))))))

;;;;;;;;;;;;;;;;;;;;

(provide 'dropdown-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el ends here;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/initialize-bundle ()
  "Initialize YASnippet and load snippets in the bundle.";; Supporting elisp for subdir html-mode

;; .yas-setup.el for html-mode

(defvar yas/html-default-tag "p")

(defvar yas/html-xhtml-attr "")

(defvar yas/html-just-like-tm nil
  "Html-mode snippets behave as close to TextMate as possible.")

(defun yas/html-remove-preceding-word ()
  (interactive)
  (let (word-begin
        word-end
        (line-beginning-position (line-beginning-position))
        (orig-point (point))
        retval)
    (save-excursion
      (when (and (forward-word -1)
                 (setq word-begin (point))
                 (forward-word 1)
                 (setq word-end (point))
                 (< word-begin orig-point)
                 (>= word-end orig-point)
                 (<= (line-beginning-position) word-begin)
                 ;; (not (string-match "^[\s\t]+$" "          "))
                 )
      (setq retval
            (cons
             (buffer-substring-no-properties word-begin orig-point)
             (buffer-substring-no-properties word-end orig-point)))
      (delete-region word-begin word-end)
      retval))))


(defun yas/html-first-word (string)
  (replace-regexp-in-string "\\\W.*" "" string))

(defun yas/html-insert-tag-pair-snippet ()
  (let* ((tag-and-suffix (or (and yas/selected-text
                                  (cons yas/selected-text nil))
                             (yas/html-remove-preceding-word)))
         (tag    (car tag-and-suffix))
         (suffix (or (cdr tag-and-suffix) ""))
         (single-no-arg "\\(br\\|hr\\)")
         (single        "\\(img\\|meta\\|link\\|input\\|base\\|area\\|col\\|frame\\|param\\)"))
    (cond ((null tag)
           (yas/expand-snippet (format "<${1:%s}>%s</${1:$(yas/html-first-word yas/text)}>%s"
                                       (or yas/html-default-tag
                                           "p")
                                       (if yas/html-just-like-tm "$2" "$0")
                                       suffix)))
          ((string-match single-no-arg tag)
           (insert (format "<%s%s/>%s" tag yas/html-xhtml-attr suffix)))
          ((string-match single tag)
           (yas/expand-snippet (format "<%s $1%s/>%s" tag yas/html-xhtml-attr suffix)))
          (t
           (yas/expand-snippet (format "<%s>%s</%s>%s"
                                       tag
                                       (if yas/html-just-like-tm "$1" "$0")
                                       (replace-regexp-in-string "\\\W.*" "" tag)
                                       suffix))))))

(defun yas/html-wrap-each-line-in-openclose-tag ()
  (let* ((mirror "${1:$(yas/html-first-word yas/text)}")
         (yas/html-wrap-newline (when (string-match "\n" yas/selected-text) "\n"))
         (template (concat (format "<${1:%s}>" (or yas/html-default-tag "p"))
                           yas/selected-text
                           "</" mirror ">")))
    (setq template (replace-regexp-in-string "\n" (concat "</" mirror ">\n<$1>") template))
    (yas/expand-snippet template)))

(defun yas/html-toggle-wrap (string wrap)
  (or (and string
           (string-match (format "<%s>\\(.*\\)</%s>" wrap wrap)
                         string)
           (match-string 1 string))
      (concat "<em>" string "</em>")))

(defun yas/html-between-tag-pair-p ()
  (save-excursion
    (backward-word)
    (looking-at "\\\w+></\\\w+>")))

(defun yas/html-id-from-string (string)
  (replace-regexp-in-string " " "_" (downcase string)))

(defun yas/html-tidy ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    (shell-command-on-region start end "tidy" (current-buffer) t (get-buffer-create "*tidy errors*") t)
    (goto-char (min (point-max) orig))
    (recenter (1- orig-line))))

(defun yas/html-tag-description ()
  (interactive)
  (let* ((tag-at-point (sgml-beginning-of-tag))
         (fragment (and tag-at-point
                        (aget yas/html-tag-description-urls (upcase tag-at-point)))))
    (if fragment
        (browse-url (concat "http://www.w3.org/TR/html4/index/"
                            fragment))
      (if tag-at-point
          (message "No documentation for " tag-at-point)
        (message "Not on a HTML tag.")))))

(defvar yas/html-tag-description-urls
  '(("A"           . "../struct/links.html#edef-A")
    ("ABBR"        . "../struct/text.html#edef-ABBR")
    ("ACRONYM"     . "../struct/text.html#edef-ACRONYM")
    ("ADDRESS"     . "../struct/global.html#edef-ADDRESS")
    ("APPLET"      . "../struct/objects.html#edef-APPLET")
    ("AREA"        . "../struct/objects.html#edef-AREA")
    ("B"           . "../present/graphics.html#edef-B")
    ("BASE"        . "../struct/links.html#edef-BASE")
    ("BASEFONT"    . "../present/graphics.html#edef-BASEFONT")
    ("BDO"         . "../struct/dirlang.html#edef-BDO")
    ("BIG"         . "../present/graphics.html#edef-BIG")
    ("BLOCKQUOTE"  . "../struct/text.html#edef-BLOCKQUOTE")
    ("BODY"        . "../struct/global.html#edef-BODY")
    ("BR"          . "../struct/text.html#edef-BR")
    ("BUTTON"      . "../interact/forms.html#edef-BUTTON")
    ("CAPTION"     . "../struct/tables.html#edef-CAPTION")
    ("CENTER"      . "../present/graphics.html#edef-CENTER")
    ("CITE"        . "../struct/text.html#edef-CITE")
    ("CODE"        . "../struct/text.html#edef-CODE")
    ("COL"         . "../struct/tables.html#edef-COL")
    ("COLGROUP"    . "../struct/tables.html#edef-COLGROUP")
    ("DD"          . "../struct/lists.html#edef-DD")
    ("DEL"         . "../struct/text.html#edef-del")
    ("DFN"         . "../struct/text.html#edef-DFN")
    ("DIR"         . "../struct/lists.html#edef-DIR")
    ("DIV"         . "../struct/global.html#edef-DIV")
    ("DL"          . "../struct/lists.html#edef-DL")
    ("DT"          . "../struct/lists.html#edef-DT")
    ("EM"          . "../struct/text.html#edef-EM")
    ("FIELDSET"    . "../interact/forms.html#edef-FIELDSET")
    ("FONT"        . "../present/graphics.html#edef-FONT")
    ("FORM"        . "../interact/forms.html#edef-FORM")
    ("FRAME"       . "../present/frames.html#edef-FRAME")
    ("FRAMESET"    . "../present/frames.html#edef-FRAMESET")
    ("H1"          . "../struct/global.html#edef-H1")
    ("H2"          . "../struct/global.html#edef-H2")
    ("H3"          . "../struct/global.html#edef-H3")
    ("H4"          . "../struct/global.html#edef-H4")
    ("H5"          . "../struct/global.html#edef-H5")
    ("H6"          . "../struct/global.html#edef-H6")
    ("HEAD"        . "../struct/global.html#edef-HEAD")
    ("HR"          . "../present/graphics.html#edef-HR")
    ("HTML"        . "../struct/global.html#edef-HTML")
    ("I"           . "../present/graphics.html#edef-I")
    ("IFRAME"      . "../present/frames.html#edef-IFRAME")
    ("IMG"         . "../struct/objects.html#edef-IMG")
    ("INPUT"       . "../interact/forms.html#edef-INPUT")
    ("INS"         . "../struct/text.html#edef-ins")
    ("ISINDEX"     . "../interact/forms.html#edef-ISINDEX")
    ("KBD"         . "../struct/text.html#edef-KBD")
    ("LABEL"       . "../interact/forms.html#edef-LABEL")
    ("LEGEND"      . "../interact/forms.html#edef-LEGEND")
    ("LI"          . "../struct/lists.html#edef-LI")
    ("LINK"        . "../struct/links.html#edef-LINK")
    ("MAP"         . "../struct/objects.html#edef-MAP")
    ("MENU"        . "../struct/lists.html#edef-MENU")
    ("META"        . "../struct/global.html#edef-META")
    ("NOFRAMES"    . "../present/frames.html#edef-NOFRAMES")
    ("NOSCRIPT"    . "../interact/scripts.html#edef-NOSCRIPT")
    ("OBJECT"      . "../struct/objects.html#edef-OBJECT")
    ("OL"          . "../struct/lists.html#edef-OL")
    ("OPTGROUP"    . "../interact/forms.html#edef-OPTGROUP")
    ("OPTION"      . "../interact/forms.html#edef-OPTION")
    ("P"           . "../struct/text.html#edef-P")
    ("PARAM"       . "../struct/objects.html#edef-PARAM")
    ("PRE"         . "../struct/text.html#edef-PRE")
    ("Q"           . "../struct/text.html#edef-Q")
    ("S"           . "../present/graphics.html#edef-S")
    ("SAMP"        . "../struct/text.html#edef-SAMP")
    ("SCRIPT"      . "../interact/scripts.html#edef-SCRIPT")
    ("SELECT"      . "../interact/forms.html#edef-SELECT")
    ("SMALL"       . "../present/graphics.html#edef-SMALL")
    ("SPAN"        . "../struct/global.html#edef-SPAN")
    ("STRIKE"      . "../present/graphics.html#edef-STRIKE")
    ("STRONG"      . "../struct/text.html#edef-STRONG")
    ("STYLE"       . "../present/styles.html#edef-STYLE")
    ("SUB"         . "../struct/text.html#edef-SUB")
    ("SUP"         . "../struct/text.html#edef-SUP")
    ("TABLE"       . "../struct/tables.html#edef-TABLE")
    ("TBODY"       . "../struct/tables.html#edef-TBODY")
    ("TD"          . "../struct/tables.html#edef-TD")
    ("TEXTAREA"    . "../interact/forms.html#edef-TEXTAREA")
    ("TFOOT"       . "../struct/tables.html#edef-TFOOT")
    ("TH"          . "../struct/tables.html#edef-TH")
    ("THEAD"       . "../struct/tables.html#edef-THEAD")
    ("TITLE"       . "../struct/global.html#edef-TITLE")
    ("TR"          . "../struct/tables.html#edef-TR")
    ("TT"          . "../present/graphics.html#edef-TT")
    ("U"           . "../present/graphics.html#edef-U")
    ("UL"          . "../struct/lists.html#edef-UL")
    ("VAR"         . "../struct/text.html#edef-VAR")))

;;
;;
;; Substitutions for: content
;; # as in Snippets/Emphasize.yasnippet
;; ${TM_SELECTED_TEXT/\A<em>(.*)<\/em>\z|.*/(?1:$1:<em>$0<\/em>)/m}                    =yyas> `(yas/html-toggle-wrap yas/selected-text "em")`
;; ${TM_SELECTED_TEXT/\A<strong>(.*)<\/strong>\z|.*/(?1:$1:<strong>$0<\/strong>)/m}    =yyas> `(yas/html-toggle-wrap yas/selected-text "strong")`
;; ${1/\s.*//}                                                                         =yyas> ${1:$(replace-regexp-in-string "^[\s\t\n]*" "" yas/text)}
;; ${1/[[:alpha:]]+|( )/(?1:_:\L$0)/g}                                                 =yyas> ${1:$(replace-regexp-in-string " " "_" (downcase yas/text))}
;; ${TM_XHTML}                                                                         =yyas> `yas/html-xhtml-attr`


;; # as in Commands/Preview in All Active Browsers.yasnippet
;; 970EE6B4-A091-11D9-A5A2-000D93C8BE28                                                       =yyas> (browse-url-of-buffer)
;; 637CEA2B-578C-429C-BB74-30E8D42BFA22                                                       =yyas> (yas/html-tag-description)
;; 2ED44A32-C353-447F-BAE4-E3522DB6944D                                                       =yyas> (yas/html-insert-tag-pair-snippet)
;; 991E7EBD-F3F5-469A-BA01-DC30E04AD472                                                       =yyas> (yas/html-wrap-each-line-in-openclose-tag)

;; Substitutions for: binding
;; 
;; # as in Snippets/Strong.yasnippet
;; @b                                                                                         =yyas> s-b
;; 
;; # as in Snippets/Emphasize.yasnippet
;; @i                                                                                         =yyas> s-i
;; 
;; # as in Snippets/Wrap Selection In Tag.yasnippet
;; ^W                                                                                         =yyas> C-c M-w
;; 
;; # as in Commands/Insert Tag Pair.yasnippet
;; ^<                                                                                         =yyas> C-<
;;
;; # as in Commands/Documentation for Tag.yasnippet
;; ^h                                                                                         =yyas> C-c M-h
;; 
;; # as in Commands/Wrap Each Selected Line in OpenClose Tag.yasnippet
;; ^@W                                                                                        =yyas> C-c M-W
;; 
;; # as in Snippets/XHTML &nbsp NonBreakingSpace.yasnippet
;; ~                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; @&                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; @r                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Persistent Include.yasnippet
;; ^@i                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; ^@u                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; ^~                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; ^H                                                                                         =yyas> (yas/unknown)
;; 
;;
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'html-mode
                 '(;; Documentation for Tag
                   (yas/item "637CEA2B-578C-429C-BB74-30E8D42BFA22")
                   ;; Ignoring Validate Syntax (W3C)
                   (yas/ignore-item "3F26240E-6E4A-11D9-B411-000D93589AF6")
                   
                   ;; Open Document in Running Browser(s)
                   (yas/item "970EE6B4-A091-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Refresh Running Browser(s)
                   (yas/ignore-item "B8651C6E-A05E-11D9-86AC-000D93C8BE28")
                   
                   (yas/submenu "Entities"
                                (;; Ignoring Convert Character / Selection to Entities
                                 (yas/ignore-item "3DD8406C-A116-11D9-A5A2-000D93C8BE28")
                                 ;; Ignoring Convert Character / Selection to Entities Excl. Tags
                                 (yas/ignore-item "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09")
                                 ;; Ignoring Decode Entities in Line / Selection
                                 (yas/ignore-item "C183920D-A126-11D9-A5A2-000D93C8BE28")
                                 
                                 ;; Non-Breaking Space
                                 (yas/item "73B40BAE-A295-11D9-87F7-000D93C8BE28")
                                 ;; →
                                 (yas/item "C70BB693-0954-4440-AEB4-F2ADD6D923F0")
                                 ;; ←
                                 (yas/item "C0418A4A-7E42-4D49-8F86-6E339296CB84")
                                 ;; ⇤
                                 (yas/item "7F102705-27D8-4029-BF61-2F042FB61E06")
                                 ;; ⌅
                                 (yas/item "7062316B-4236-4793-AD35-05E4A6577393")
                                 ;; ⌃
                                 (yas/item "B4987DA5-9C2F-4D2D-AC14-678115079205")
                                 ;; ⌦
                                 (yas/item "44E448B6-37CE-4BFE-8611-C5113593B74B")
                                 ;; ↩
                                 (yas/item "9B216475-D73D-4518-851F-CACD0066A909")
                                 ;; ⇥
                                 (yas/item "ADC78A82-40C2-4AAC-8968-93AF0ED98DF0")
                                 ;; ⌫
                                 (yas/item "38E50882-27AF-4246-A039-355C3E1A699E")
                                 ;; ⌘
                                 (yas/item "7214ACD1-93D9-4D3F-A428-8A7302E0A35E")
                                 ;; ↓
                                 (yas/item "35654B4E-2D76-4CD3-8FBB-2DA1F314BA19")
                                 ;; →
                                 (yas/item "AC15621A-8A16-40DD-A671-EA4C37637215")
                                 ;; ↑
                                 (yas/item "0E2F4A47-EADE-4A05-931E-FC874FA28FC3")
                                 ;; ⇧
                                 (yas/item "1B8D58B9-D9DB-484C-AACD-5D5DF5385308")
                                 ;; ⎋
                                 (yas/item "D7CC7C7C-CD01-4357-AF91-AEFFD914DF98")
                                 ;; ⌥
                                 (yas/item "980A8D39-CA8B-4EC2-9739-DC36A262F28E")
                                 (yas/separator)
                                 ;; Ignoring Insert Entity…
                                 (yas/ignore-item "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529")))
                   (yas/submenu "URL Escapes"
                                (;; Ignoring URL Escape Line / Selection
                                 (yas/ignore-item "6B024865-6095-4CE3-8EDD-DC6F2230C2FF")
                                 ;; Ignoring URL Unescape Line / Selection
                                 (yas/ignore-item "2C4C9673-B166-432A-8938-75A5CA622481")))
                   ;; Ignoring Encrypt Line / Selection (ROT 13)
                   (yas/ignore-item "9B13543F-8356-443C-B6E7-D9259B604927")
                   
                   ;; Ignoring CodeCompletion HTML Attributes
                   (yas/ignore-item "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA")
                   ;; Insert Open/Close Tag (With Current Word)
                   (yas/item "2ED44A32-C353-447F-BAE4-E3522DB6944D")
                   ;; Ignoring Insert Close Tag
                   (yas/ignore-item "0658019F-3635-462E-AAC2-74E4FE508A9B")
                   (yas/submenu "Insert DocType"
                                (;; HTML — 4.01 Strict
                                 (yas/item "944F1410-188C-4D70-8340-CECAA56FC7F2")
                                 ;; HTML — 4.01 Transitional
                                 (yas/item "B2AAEE56-42D8-42C3-8F67-865473F50E8D")
                                 (yas/separator)
                                 ;; XHTML — 1.0 Frameset
                                 (yas/item "9ED6ABBE-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Strict
                                 (yas/item "C8B83564-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.0 Transitional
                                 (yas/item "7D8C2F74-A802-11D9-BFC8-000D93C8BE28")
                                 ;; XHTML — 1.1
                                 (yas/item "5CE8FC6E-A802-11D9-BFC8-000D93C8BE28")))
                   (yas/submenu "Insert Tag"
                                (;; Ignoring CodeCompletion HTML Tags
                                 (yas/ignore-item "3463E85F-F500-49A0-8631-D78ED85F9D60")
                                 
                                 ;; Base
                                 (yas/item "4462A6B8-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Body
                                 (yas/item "4905D47B-A08B-11D9-A5A2-000D93C8BE28")
                                 ;; Br
                                 (yas/item "3E008E42-A5C9-11D9-9BCD-000D93C8BE28")
                                 ;; Div
                                 (yas/item "576036C0-A60E-11D9-ABD6-000D93C8BE28")
                                 ;; Embed QT Movie
                                 (yas/item "42F15753-9B6D-4DD8-984C-807B94363277")
                                 ;; Fieldset
                                 (yas/item "9BD2BE01-A854-4D55-B584-725D04C075C0")
                                 ;; Form
                                 (yas/item "232C2E8B-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Head
                                 (yas/item "9CF008C4-A086-11D9-A5A2-000D93C8BE28")
                                 ;; Heading
                                 (yas/item "65BA66DC-A07F-11D9-A5A2-000D93C8BE28")
                                 ;; Input
                                 (yas/item "44180979-A08E-11D9-A5A2-000D93C8BE28")
                                 ;; Input with Label
                                 (yas/item "D8DCCC81-749A-4E2A-B4BC-D109D5799CAA")
                                 ;; Link
                                 (yas/item "77BFD0C0-A08A-11D9-A5A2-000D93C8BE28")
                                 ;; Mail Anchor
                                 (yas/item "81DA4C74-A530-11D9-9BCD-000D93C8BE28")
                                 ;; Meta
                                 (yas/item "DA99AC44-A083-11D9-A5A2-000D93C8BE28")
                                 ;; Option
                                 (yas/item "5820372E-A093-4F38-B25C-B0CCC50A0FC4")
                                 ;; Script
                                 (yas/item "6592050A-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Script With External Source
                                 (yas/item "7D676C4C-A087-11D9-A5A2-000D93C8BE28")
                                 ;; Select Box
                                 (yas/item "26023CFF-C73F-4EF5-9803-E4DBA2CBEADD")
                                 ;; Style
                                 (yas/item "3C518074-A088-11D9-A5A2-000D93C8BE28")
                                 ;; Table
                                 (yas/item "57176082-A12F-11D9-A5A2-000D93C8BE28")
                                 ;; Text Area
                                 (yas/item "AAC9D7B8-A12C-11D9-A5A2-000D93C8BE28")
                                 ;; Title
                                 (yas/item "B62ECABE-A086-11D9-A5A2-000D93C8BE28")))
                   
                   (yas/submenu "Includes"
                                (;; Ignoring Add Persistent Include
                                 (yas/ignore-item "0D814247-7A00-46EE-A2A4-45FBBF4B1181")
                                 ;; Ignoring Update Document
                                 (yas/ignore-item "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8")
                                 ;; Ignoring Help: Persistent Includes
                                 (yas/ignore-item "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C")))
                   (yas/submenu "Format"
                                (;; Strong
                                 (yas/item "4117D930-B6FA-4022-97E7-ECCAF4E70F63")
                                 ;; Emphasize
                                 (yas/item "EBB98620-3292-4621-BA38-D8A9A65D9551")))
                   (yas/submenu "Conditional Comments"
                                (;; IE Conditional Comment: Internet Explorer
                                 (yas/item "0ED6DA73-F38F-4A65-B18F-3379D2BA9387")
                                 ;; IE Conditional Comment: Internet Explorer 5.0 only
                                 (yas/item "3A517A94-001E-464D-8184-1FE56D0D0D70")
                                 ;; IE Conditional Comment: Internet Explorer 5.5 only
                                 (yas/item "E3F8984E-7269-4981-9D30-967AB56A6ACE")
                                 ;; IE Conditional Comment: Internet Explorer 5.x
                                 (yas/item "F3512848-7889-45DA-993B-0547976C8E6D")
                                 ;; IE Conditional Comment: Internet Explorer 6 and below
                                 (yas/item "32BBB9AB-8732-4F91-A587-354941A27B69")
                                 ;; IE Conditional Comment: Internet Explorer 6 only
                                 (yas/item "48DF7485-52EA-49B3-88AF-3A41F933F325")
                                 ;; IE Conditional Comment: Internet Explorer 7 and above
                                 (yas/item "CBC24AF4-88E0-498B-BE50-934B9CF29EC7")
                                 ;; IE Conditional Comment: NOT Internet Explorer
                                 (yas/item "F00170EE-4A82-413F-A88B-85293E69A88B")))
                   
                   ;; Wrap Selection in Open/Close Tag
                   (yas/item "BC8B8AE2-5F16-11D9-B9C3-000D93589AF6")
                   ;; Wrap Each Selected Line in Open/Close Tag
                   (yas/item "991E7EBD-F3F5-469A-BA01-DC30E04AD472")
                   ;; Wrap in <?= … ?>
                   (yas/item "912906A0-9A29-434B-AE98-E9DFDE6E48B4")
                   (yas/separator)
                   ;; Ignoring Strip HTML Tags from Document / Selection
                   (yas/ignore-item "20D760B5-A127-11D9-A5A2-000D93C8BE28")
                   ;; Ignoring Tidy
                   (yas/ignore-item "45F92B81-6F0E-11D9-A1E4-000D9332809C"))
                    '("7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "3C44EABE-8D6F-4B1B-AB91-F419FAD1A0AD"
                       "4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8"
                       "3463E85F-F500-49A0-8631-D78ED85F9D60"
                       "CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA"
                       "9B13543F-8356-443C-B6E7-D9259B604927"
                       "0D814247-7A00-46EE-A2A4-45FBBF4B1181"
                       "9AFDEB2C-D9F0-423E-8211-EBB089F51F0C"
                       "C8B717C2-6B33-11D9-BB47-000D93589AF6"
                       "CD6D2CC6-6B33-11D9-BDFD-000D93589AF6"
                       "B23D6E15-6B33-11D9-86C1-000D93589AF6"
                       "7B7E945E-A112-11D9-A5A2-000D93C8BE28"
                       "45F92B81-6F0E-11D9-A1E4-000D9332809C"
                       "3DD8406C-A116-11D9-A5A2-000D93C8BE28"
                       "3F26240E-6E4A-11D9-B411-000D93589AF6"
                       "43C9E8AE-3E53-4B82-A1AF-56697BB3EF09"
                       "89E5CC0A-3EFF-4DEF-A299-2E9651DE6529"
                       "2C4C9673-B166-432A-8938-75A5CA622481"
                       "6B024865-6095-4CE3-8EDD-DC6F2230C2FF"
                       "0658019F-3635-462E-AAC2-74E4FE508A9B"
                       "20D760B5-A127-11D9-A5A2-000D93C8BE28"
                       "B8651C6E-A05E-11D9-86AC-000D93C8BE28"
                       "C183920D-A126-11D9-A5A2-000D93C8BE28"
                       "CDE8EFD6-9DE2-4E8C-BB6A-52E8CCD2E977"
                       "E6F19171-F664-4B4F-92DA-3E15E6CAD35C"
                       "EBEE6B51-29C7-4362-818F-A190CACD5296"
                       "26068A55-4C84-409D-BA00-162B55AF6961"
                       "65D38039-6B0A-48E9-9E49-43832ECC4107"
                       "04332FA8-8157-46C4-9854-8C190FFD96C6"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Templates/XHTML 1.1/info.yasnippet
;; CDE8EFD6-9DE2-4E8C-BB6A-52E8CCD2E977                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_0 only.yasnippet
;; `(or (yas/selected-text) "   IE Conditional Comment: Internet Explorer 5.0 only ")`        =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer.yasnippet
;; `(or (yas/selected-text) "       IE Conditional Comment: Internet Explorer          ")`    =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; 3463E85F-F500-49A0-8631-D78ED85F9D60                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/Emphasize.yasnippet
;; `(yas/html-toggle-wrap yas/selected-text "em")`                                            =yyas> (yas/unknown)
;; 
;; # as in Templates/HTML 4.0 Transitional/info.yasnippet
;; E6F19171-F664-4B4F-92DA-3E15E6CAD35C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Entity.yasnippet
;; 89E5CC0A-3EFF-4DEF-A299-2E9651DE6529                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert to HTML Entities.yasnippet
;; 3DD8406C-A116-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML body.yasnippet
;; ${TM_FILENAME/(.*)\..*/\L$1/}                                                              =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML form.yasnippet
;; ${TM_FILENAME/(.*?)\..*/$1_submit/}                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Persistent Include.yasnippet
;; 0D814247-7A00-46EE-A2A4-45FBBF4B1181                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; B8651C6E-A05E-11D9-86AC-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/Strong.yasnippet
;; `(yas/html-toggle-wrap yas/selected-text "strong")`                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_5 only.yasnippet
;; `(or (yas/selected-text) "   IE Conditional Comment: Internet Explorer 5.5 only ")`        =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Close Tag.yasnippet
;; 0658019F-3635-462E-AAC2-74E4FE508A9B                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Decode Numeric URL Escapes in Line Selection.yasnippet
;; 2C4C9673-B166-432A-8938-75A5CA622481                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert to named entities excl tags.yasnippet
;; 43C9E8AE-3E53-4B82-A1AF-56697BB3EF09                                                       =yyas> (yas/unknown)
;; 
;; # as in DragCommands/CSS Link.yasnippet
;; C8B717C2-6B33-11D9-BB47-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/About Persistent Includes.yasnippet
;; 9AFDEB2C-D9F0-423E-8211-EBB089F51F0C                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML link.yasnippet
;; `yas/html-xhtml-attr`                                                                      =yyas> (yas/unknown)
;; 
;; # as in Templates/HTML 4.0 Strict/info.yasnippet
;; 04332FA8-8157-46C4-9854-8C190FFD96C6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; 7B7E945E-A112-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in DragCommands/Anchor Tag.yasnippet
;; B23D6E15-6B33-11D9-86C1-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Transitional/info.yasnippet
;; 65D38039-6B0A-48E9-9E49-43832ECC4107                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Strip HTML tags.yasnippet
;; 20D760B5-A127-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; 45F92B81-6F0E-11D9-A1E4-000D9332809C                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML title.yasnippet
;; ${TM_FILENAME/((.+)\..*)?/(?2:$2:Page Title)/}                                             =yyas> (yas/unknown)
;; 
;; # as in Commands/Encrypt Line Selection (ROT 13).yasnippet
;; 9B13543F-8356-443C-B6E7-D9259B604927                                                       =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Strict/info.yasnippet
;; EBEE6B51-29C7-4362-818F-A190CACD5296                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Convert Line Selection to URL Escapes.yasnippet
;; 6B024865-6095-4CE3-8EDD-DC6F2230C2FF                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/W3C validation.yasnippet
;; 3F26240E-6E4A-11D9-B411-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Attributes.yasnippet
;; CBD82CF3-74E9-4E7A-B3F6-9348754EB5AA                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; 4400BCE9-20E3-426E-B1D7-2C0BCA53BCF8                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 6 and below.yasnippet
;; `(or (yas/selected-text) " IE Conditional Comment: Internet Explorer 6 and below ")`       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 5_x.yasnippet
;; `(or (yas/selected-text) "  IE Conditional Comment: Internet Explorer 5.x      ")`         =yyas> (yas/unknown)
;; 
;; # as in DragCommands/Image Tag.yasnippet
;; CD6D2CC6-6B33-11D9-BDFD-000D93589AF6                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML h1.yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment NOT Internet Explorer.yasnippet
;; `(or (yas/selected-text) "  IE Conditional Comment: NOT Internet Explorer      ")`         =yyas> (yas/unknown)
;; 
;; # as in Templates/XHTML 1.0 Frameset/info.yasnippet
;; 26068A55-4C84-409D-BA00-162B55AF6961                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Decode HTML Entities.yasnippet
;; C183920D-A126-11D9-A5A2-000D93C8BE28                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 6 only.yasnippet
;; `(or (yas/selected-text) "     IE Conditional Comment: Internet Explorer 6 only   ")`      =yyas> (yas/unknown)
;; 
;; # as in Snippets/IE Conditional Comment Internet Explorer 7+.yasnippet
;; `(or (yas/selected-text) " IE Conditional Comment: Internet Explorer 7 and above ")`       =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Snippets/XHTML head.yasnippet
;; text.html - text.html source                                                               =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Attributes.yasnippet
;; text.html punctuation.definition.tag -source, text.html meta.tag -entity.other.attribute-name -source  =yyas> (yas/unknown)
;; 
;; # as in Snippets/Smart returnindent for tag pairs.yasnippet
;; meta.scope.between-tag-pair                                                                =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Tags.yasnippet
;; text.html -entity.other.attribute-name -string.quoted, invalid.illegal.incomplete.html     =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap Selection In Tag.yasnippet
;; text.html,                                                                                 =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; text.html, source.css                                                                      =yyas> (yas/unknown)
;; 
;; # as in Templates/HTML 4.0 Strict/info.yasnippet
;; text.html                                                                                  =yyas> (yas/unknown)
;; 
;; # as in Commands/Documentation for Tag.yasnippet
;; text.html, text.html entity.name.tag                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML title.yasnippet
;; text.html - text.blog                                                                      =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in =.yasnippet
;; text.html string                                                                           =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/W3C validation.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Snippets/Smart returnindent for tag pairs.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML &nbsp NonBreakingSpace.yasnippet
;; ~                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Close Tag.yasnippet
;; ~@.                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/XHTML br.yasnippet
;; ^                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Decode HTML Entities.yasnippet
;; @&                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Refresh All Active Browsers.yasnippet
;; @r                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/CodeCompletion HTML Attributes.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Update Includes.yasnippet
;; ^@u                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete whitespace between tags.yasnippet
;; ^~                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Tidy.yasnippet
;; ^H                                                                                         =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for html-mode ends here
;; Supporting elisp for subdir rails-mode

;; .yas-setup.el for rails-mode
(defvar yas/rails-root-cache nil)

(add-to-list 'auto-mode-alist '("\\.erb$" . yas/rails-erb-mode))

(define-derived-mode yas/rails-erb-mode
  nxml-mode "eRB"
  "Embedded Ruby Mode, very thin layer over `nxml-mode'."
  (add-to-list (make-local-variable 'yas/extra-modes) 'html-mode)
  (rng-set-vacuous-schema)
  (message "hey erb mode"))

(defvar yas/rails-erb-font-lock-keywords
  '(("\\(<%=\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face)
     (3 font-lock-function-name-face))
    ("\\(<%\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face)
     (3 font-lock-variable-name-face)))
  "(Crummy) font lock highlighting for ERB constructs.."
  )
(font-lock-add-keywords 'yas/rails-erb-mode yas/rails-erb-font-lock-keywords)

;; stolen from rinari-mode's rinari-root
(defun yas/rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (or (and (featurep 'rinari) (rinari-root dir))
      yas/rails-root-cache
      (if (file-exists-p (expand-file-name
                          "environment.rb" (expand-file-name "config" dir)))
          (set (make-local-variable 'yas/rails-root-cache) dir)
        (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
          ;; regexp to match windows roots, tramp roots, or regular posix roots
          (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
            (yas/rails-root new-dir))))))

;; stolen from rinari-mode's rinari-extract-partial
(defun yas/rails-extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (let* ((path (buffer-file-name)) ending)
    (if (string-match "view" path)
	(let ((ending (and (string-match ".+?\\(\\.[^/]*\\)$" path)
			   (match-string 1 path)))
	      (partial-name
	       (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
	  (kill-region begin end)
	  (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
	      (let ((default-directory (expand-file-name (match-string 1 partial-name)
							 (expand-file-name ".."))))
		(find-file (concat "_" (match-string 2 partial-name) ending)))
	    (find-file (concat "_" partial-name ending)))
	  (yank) (pop-to-buffer nil)
	  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))
      (message "not in a view"))))
;;;
;;; The TextMate "intelligent" migration snippet
;;
(defvar yas/rails-intelligent-migration-snippet-bits
      '((:rename_column . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}$0")
                           (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_column_continue . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}\nmncc$0")
                                    (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_table . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}$0")
                          (:down . "rename_table :$2, :$1" )))

        (:rename_table_continue . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}\nmntc$0")
                                   (:down . "rename_table :$2, :$1" )))

        (:add_remove_column . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}$0")
                               (:down . "remove_column :$1, :$2" )))
        
        (:add_remove_column_continue . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}\nmarcc$0")
                                        (:down . "remove_column :$1, :$2" )))
        
        (:remove_add_column . ((:up   . "remove_column :${1:table_name}, :${2:column_name}$0")
                               (:down . "add_column :$1, :$2, :$3{string}" )))

        (:create_drop_table . ((:up   . "create_table :${1:table_name}, :force . true do |t|\nt.$0\nt.timestamps\nend")
                               (:down . "drop_table :$1" )))

        (:change_change_table . ((:up   . "change_table :${1:table_name} do |t|\nt.$0\nend")
                                 (:down . "change_table :$1 do |t|\nend" )))

        (:add_remove_index . ((:up   . "add_index :${1:table_name}, :${2:column_name}$0")
                              (:down . "remove_index :$1, :$2" )))

        (:add_remove_unique_index . ((:up   . "add_index :${1:table_name}, ${2:[:${3:column_name}${4:, :${5:column_name}}]}, :unique . true$0")
                                     (:down . "remove_index :$1, :column . $2" )))

        (:add_remove_named_index . ((:up   . "add_index :${1:table_name}, [:${2:column_name}${3:, :${4:column_name}}], :name . \"${5:index_name}\"${6:, :unique . true}$0")
                                    (:down . "remove_index :$1, :name . :$5" )))))


(defun yas/rails-intelligent-migration-snippet (type)
  (let* ((start  (point))
         (end (save-excursion
                (search-forward-regexp "^\s*def\sself\.down" nil 'noerror)))
         (up (aget (aget yas/rails-intelligent-migration-snippet-bits type) :up))
         (down (aget (aget yas/rails-intelligent-migration-snippet-bits type) :down))
         (snippet
          (and up down start end (concat up
                                         (buffer-substring-no-properties start end)
                                         "\n" down))))
    (when snippet
      (delete-region start end)
      (yas/expand-snippet snippet))))

(yas/define-condition-cache
  yas/rails-intelligent-migration-snippet-condition-p
  "Non-nil if an \"intelligent\" migration snippet should be expanded"
  (and (yas/rails-migration-p)
       (not (yas/rails-in-create-table-p))
       (not (yas/rails-in-change-table-p))
       (yas/rails-in-ruby-block-like "self\.up")))

(defun yas/rails-in-ruby-block-like (regexp)
  (save-excursion
    (ruby-accurate-end-of-block)
    (ruby-backward-sexp)
    (search-forward-regexp regexp (line-end-position) t)))

;;; conditions
(yas/define-condition-cache
 yas/rails-in-create-table-p
 "Non-nil if point is inside a 'create_table' method call."
 (yas/rails-in-ruby-block-like "create_table"))

(yas/define-condition-cache
 yas/rails-in-change-table-p
 "Non-nil if point is inside a 'change_table' method call."
 (yas/rails-in-ruby-block-like "change_table"))

(yas/define-condition-cache
 yas/rails-model-p
 "Non-nil if the current buffer is a rails model."
 (and (yas/rails-root)
      (string-match "app/models/$" default-directory)))

(yas/define-condition-cache
 yas/rails-view-p
 "Non-nil if the current buffer is a rails view."
 (and (yas/rails-root)
      (string-match "app/views/" default-directory)))

(yas/define-condition-cache
 yas/rails-controller-p
"Non-nil if the current buffer is a rails controller." 
 (and (yas/rails-root)
      (string-match "app/controllers/$" default-directory)))

(yas/define-condition-cache
 yas/rails-migration-p
 "Non-nil if the current buffer is a rails migration."
 (and (yas/rails-root)
      (string-match "db/migrate/" default-directory)))

(defun yas/rails-activate-maybe ()
  (when (and yas/minor-mode
             (yas/rails-root))
    (add-to-list (make-local-variable 'yas/extra-modes) 'rails-mode)))

(defadvice cd (after yas/rails-on-cd-activate activate)
  "Add `rails-mode' to `yas/extra-modes' so that rails snippets
are recognized. Stolen from `rinari-mode' more or`' less."
  (setq yas/rails-root-cache nil)
  (yas/rails-activate-maybe))

(add-hook 'yas/minor-mode-hook 'yas/rails-activate-maybe)
;; Substitutions for: content
;; 
;; # as in Macros/Remove 3A Add Column.yasnippet
;; 809BCA42-5C49-4B08-B3C4-BB773036C086                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Named Index.yasnippet
;; A7F692C1-778A-48B8-945E-573568BA0403                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Unit Test.yasnippet
;; BDBB15A4-2824-4BEC-93A5-7475F9C46A39                                                       =yyas> (yas/rails-find 'unit-test)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; 09BB96F2-75FD-48A7-8314-B5B56B09B477                                                       =yyas> (ffap)
;; 
;; # as in Commands/Test Uncommitted.yasnippet
;; 212C3047-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Redo Last Migration.yasnippet
;; CFDA9F62-D071-4E0F-AD10-66AE0729FFCF                                                       =yyas> (yas/rails-compile "rake")
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; 0BCF0EE2-35EE-4959-A771-E74D55271D5A                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; 275C0B86-F735-49B6-8A22-218A8F4CC2E0                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Change Table.yasnippet
;; 20FC02C5-32A3-4F20-B163-FF75C9FDFABF                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; 985F56D4-82ED-4C45-8250-2ECCFC71957E                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Import.yasnippet
;; 6DEF923E-2347-46EC-AFBE-183D08E63DC1                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures (Test DB).yasnippet
;; F758BFD1-00CA-4742-BE71-032580080F5C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; D696FA2C-785A-4B73-A2F6-F750904DD7C2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Remove 3A Add Timestamps.yasnippet
;; E885A3E8-8020-4AC3-A25E-510B26F114B2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns (marcc).yasnippet
;; 27A6C58A-896B-4956-BA81-D671A2EF9C7D                                                       =yyas> (yas/rails-intelligent-migration-snippet :add_remove_column_continue)
;; 
;; # as in Macros/Add 3A Remove Column.yasnippet
;; 18C76913-061C-4D65-866D-67AA3724AFEF                                                       =yyas> (yas/rails-intelligent-migration-snippet :add_remove_column)
;; 
;; # as in Commands/Go To View.yasnippet
;; EE862691-A624-4797-90CF-EDD39EFB2D8E                                                       =yyas> (yas/rails-find 'view)
;; 
;; # as in Commands/Test Plugins.yasnippet
;; 0D966168-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column.yasnippet
;; 42DE1441-D1B7-4998-BAF9-16B1EC7E210C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; 1DD8A214-1C97-45BA-ADEE-8F888DDE8570                                                       =yyas> (call-interactively 'yas/rails-extract-partial)
;; 
;; # as in Commands/Go To Functional Test.yasnippet
;; DFE393BE-0764-49FE-B464-6350A50921E6                                                       =yyas> (yas/rails-find 'functional-test)
;; 
;; # as in Commands/Test Recent.yasnippet
;; 190401C2-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test All.yasnippet
;; DC549A45-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Column.yasnippet
;; AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Create 3A Drop Table.yasnippet
;; 25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Unique Index.yasnippet
;; 33057A79-677B-4DFB-99D4-1492778BDDC6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Timestamps.yasnippet
;; 221969A1-A5EA-4A8E-8817-C74EBED63901                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Helper.yasnippet
;; 51C9C27A-D931-49F9-B6D8-C0E7ABEC992D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Dump.yasnippet
;; 310C901C-EF32-4E88-938A-804ABBF8C428                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Functionals.yasnippet
;; F4EA552D-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Migrate to Previous Version.yasnippet
;; 9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Model.yasnippet
;; C7151BF3-7068-4344-9B09-86F3BF4A9C63                                                       =yyas> (yas/rails-find 'model)
;; 
;; # as in Macros/Drop 3A Create Table.yasnippet
;; A2135370-67A1-488D-B43C-B4F221127C2F                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column Default.yasnippet
;; A219EBB8-004A-4012-B5B2-232C9A5C94F8                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Index.yasnippet
;; 95F83E1D-5B03-424F-8BEC-8AF66C8939BC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures.yasnippet
;; 5EEA0C71-B34B-4408-953B-F47AAD343CCC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Documentation for Word.yasnippet
;; 32F30207-D827-46D9-889A-451C35269D52                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Clone Development DB to Test DB.yasnippet
;; 6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns.yasnippet
;; F03162DE-9DB6-417B-9DD7-52D9F11EA736                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Stylesheet.yasnippet
;; B207BBD4-D6AA-41E9-9530-27210F2D7B66                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Javascript.yasnippet
;; B078346F-61D8-4E75-9427-80720FBC67F7                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate to Version.yasnippet
;; 07C696F8-79F5-4E0B-9EE9-03B693A54ABB                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> (yas/rails-find 'alternate)
;; 
;; # as in Commands/View demo help.yasnippet
;; 964436B8-E578-11DC-8177-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go to Fixture.yasnippet
;; 638D94A4-BDFC-4FE9-8909-9934F3FD2899                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Table.yasnippet
;; FD8CC811-2AD3-480F-B975-DF959DC96C67                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns (mncc).yasnippet
;; 04A86178-71B1-430A-A06D-DFF7C9A338B5                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate.yasnippet
;; 4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Integration.yasnippet
;; 04A30A4D-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; 1970AE74-3949-40B3-B263-727AA3FF167A                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns.yasnippet
;; 7BC860E6-7561-4E6E-983B-507D7A6F6228                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Install Bundled Plugin.yasnippet
;; 46ECE243-0448-4A64-A223-27CC21E7704D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> (yas/rails-find 'file)
;; 
;; # as in Commands/Test Units.yasnippet
;; 2C60CBA1-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/returning do 7Cvariable7C E280A6 end.yasnippet
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}                     =yyas> ${2:$(and (yas/text) " |")}
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}                      =yyas> ${2:$(and (yas/text) "|")}
;; 
;; # as in Snippets/form_for label.yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1: :\u$0)/g}                                                        =yyas> ${1:$(capitalize (replace-regexp-in-string "_" " " yas/text))}
;;
;; # as in Snippets/has_one (ho).yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1::\u$0)/g}                                                         =yyas> ${1:$(replace-regexp-in-string "_" "" (capitalize yas/text))}
;;
;; # as in Snippets/Create sweeper class.yasnippet
;; ${1/./\l$0/}                                                                               =yyas> ${1:$(and (yas/text) (concat (downcase (substring yas/text 0 1)) (substring yas/text 1)))} 
;;
;; # as in Snippets/image_submit_tag.yasnippet
;; ${1/^(\w+)(\.\w*)?$/$1/}                                                                   =yyas> ${1:$(file-name-sans-extension yas-text)}
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; $TM_SELECTED_TEXT                                                                          =yyas> `yas/selected-text`
;;
;; # as in Snippets/find_in_batches.yasnippet
;; ${TM_CURRENT_WORD/(\w+)\./\L$1/g}                                                          =yyas> `(downcase (replace-regexp-in-string "\\..*$"  "" (current-word)))` 
;; 

;; Substitutions for: condition

;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.unit_test, source.js, source.css, source.yaml, meta.rails.controller, meta.rails.functional_test, text.haml =yyas> (yas/unknown)
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.functional_test, source.js, source.css, source.yaml, meta.rails.model, meta.rails.unit_test, text.haml      =yyas> (yas/unknown)
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test, text.haml                                      =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test                                                                               =yyas> (yas/unknown)
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.helper, text.haml                                                                                       =yyas> (yas/unknown)
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.functional_test, source.yaml                                                                                        =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, source.yaml, meta.rails.unit_test                                                                                              =yyas> t
;; meta.rails.migration - meta.rails.migration.create_table - meta.rails.migration.change_table                                                                                               =yyas> (yas/rails-intelligent-migration-snippet-condition-p)
;; meta.rails.migration.create_table, meta.rails.migration.change_table                                                                                                                       =yyas> (or (yas/rails-in-create-table-p) (yas/rails-in-change-table-p))
;; meta.rails.controller, meta.rails.mailer, source.js, source.css                                                                                                                            =yyas> (yas/unknown)
;; meta.rails.migration.create_table                                                                                                                                                          =yyas> (yas/rails-in-create-table-p)
;; meta.rails.functional_test                                                                                                                                                                 =yyas> (yas/rails-in-functional-test-p)
;; text.html.ruby, text.haml                                                                                                                                                                  =yyas> (yas/rails-view-p)
;; meta.rails.controller                                                                                                                                                                      =yyas> (yas/rails-in-controller-p)
;; meta.rails.routes                                                                                                                                                                          =yyas> (yas/rails-in-routes-p)
;; text.html.ruby                                                                                                                                                                             =yyas> (yas/unknown)


;; Substitutions for: binding
;; 
;; # as in Snippets/rails session.yasnippet
;; ^j                                                                                         =yyas> C-c M-j
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; ~$                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To View.yasnippet
;; ~$@                                                                                     =yyas> C-M-s-down
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; ^M                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; ~@                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; ^@S                                                                                        =yyas> C-c M-s
;; 
;; # as in Snippets/rails params.yasnippet
;; ^p                                                                                         =yyas> C-c M-p
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> M-s-up
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> M-s-down
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Units.yasnippet
;; ^\                                                                                         =yyas> C-c M-\
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; ^|                                                                                         =yyas> C-c M-|
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; @H                                                                                         =yyas> C-c M-h
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; ^H                                                                                         =yyas> C-c M-m
;; 
;; # as in Commands/View demo help.yasnippet
;; ^h                                                                                         =yyas> (yas/unknown)
;; 
;;
;; 
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'rails-mode
                 '((yas/submenu "Go To"
                                (;; Alternate File
                                 (yas/item "0CCC8443-40F3-4BAB-9440-D737562B5F45")
                                 ;; File on Current Line
                                 (yas/item "09BB96F2-75FD-48A7-8314-B5B56B09B477")
                                 (yas/separator)
                                 ;; Go to Model
                                 (yas/item "C7151BF3-7068-4344-9B09-86F3BF4A9C63")
                                 ;; Go to Controller
                                 (yas/item "9453F0B3-B946-445F-BDB0-B01DE70732FC")
                                 ;; Go to View
                                 (yas/item "EE862691-A624-4797-90CF-EDD39EFB2D8E")
                                 ;; Go to Functional Test
                                 (yas/item "DFE393BE-0764-49FE-B464-6350A50921E6")
                                 ;; Ignoring Go to Helper
                                 (yas/ignore-item "51C9C27A-D931-49F9-B6D8-C0E7ABEC992D")
                                 ;; Ignoring Go to Javascript
                                 (yas/ignore-item "B078346F-61D8-4E75-9427-80720FBC67F7")
                                 ;; Ignoring Go to Stylesheet
                                 (yas/ignore-item "B207BBD4-D6AA-41E9-9530-27210F2D7B66")
                                 ;; Go to Unit Test
                                 (yas/item "BDBB15A4-2824-4BEC-93A5-7475F9C46A39")
                                 ;; Ignoring Go to Fixture
                                 (yas/ignore-item "638D94A4-BDFC-4FE9-8909-9934F3FD2899")))
                   (yas/submenu "Run Tests"
                                (;; Ignoring Test All
                                 (yas/ignore-item "DC549A45-D9B0-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Functionals
                                 (yas/ignore-item "F4EA552D-D9B0-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Integration
                                 (yas/ignore-item "04A30A4D-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Plugins
                                 (yas/ignore-item "0D966168-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Recent
                                 (yas/ignore-item "190401C2-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Uncommitted
                                 (yas/ignore-item "212C3047-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Units
                                 (yas/ignore-item "2C60CBA1-D9B1-11DC-94E9-00112475D960")))
                   
                   ;; Ignoring Call Generate Script
                   (yas/ignore-item "4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE")
                   ;; Ignoring Install Plugin
                   (yas/ignore-item "46ECE243-0448-4A64-A223-27CC21E7704D")
                   (yas/submenu "Database"
                                (;; Ignoring Migrate to Current
                                 (yas/ignore-item "985F56D4-82ED-4C45-8250-2ECCFC71957E")
                                 ;; Ignoring Migrate to Version ...
                                 (yas/ignore-item "07C696F8-79F5-4E0B-9EE9-03B693A54ABB")
                                 ;; Ignoring Migrate to Previous Version
                                 (yas/ignore-item "9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29")
                                 ;; Redo Last Migration
                                 (yas/item "CFDA9F62-D071-4E0F-AD10-66AE0729FFCF")
                                 (yas/separator)
                                 ;; Ignoring Load Fixtures (Development DB)
                                 (yas/ignore-item "5EEA0C71-B34B-4408-953B-F47AAD343CCC")
                                 ;; Ignoring Load Fixtures (Test DB)
                                 (yas/ignore-item "F758BFD1-00CA-4742-BE71-032580080F5C")
                                 
                                 ;; Ignoring Load schema.rb to DB
                                 (yas/ignore-item "6DEF923E-2347-46EC-AFBE-183D08E63DC1")
                                 ;; Ignoring Dump DB to schema.rb
                                 (yas/ignore-item "310C901C-EF32-4E88-938A-804ABBF8C428")
                                 ;; Ignoring Clone Development DB to Test DB
                                 (yas/ignore-item "6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1")))
                   
                   ;; params[…]
                   (yas/item "AC8EDA3E-875B-11D9-897C-000393CBCE2E")
                   ;; session[…]
                   (yas/item "7B02ABF8-8763-11D9-897C-000393CBCE2E")
                   (yas/submenu "Logger"
                                (;; logger.debug
                                 (yas/item "D975E5C1-42C2-40F1-8960-0DA533B18113")
                                 ;; logger.info
                                 (yas/item "36E2A3EE-E9CC-4B7F-A4CF-AFAF970B8699")
                                 ;; logger.warn
                                 (yas/item "38D5CA05-E219-4399-A244-609AF40B1D0B")
                                 ;; logger.error
                                 (yas/item "7053B86A-9B81-4154-AB3C-61B8035C5D33")
                                 ;; logger.fatal
                                 (yas/item "35E95C81-22F7-4C40-8297-ED21086DDA81")
                                 ;; RAILS_DEFAULT_LOGGER.debug
                                 (yas/item "7B15B396-1F41-4529-9253-32761E94448C")))
                   (yas/separator)
                   (yas/submenu "Models"
                                (;; Ignoring Show DB Schema for Current Class
                                 (yas/ignore-item "1970AE74-3949-40B3-B263-727AA3FF167A")
                                 
                                 (yas/submenu "Callbacks"
                                              (;; before_validation
                                               (yas/item "A1776279-5396-4FE9-9218-8BF2C88C5271")
                                               ;; before_validation_on_create
                                               (yas/item "E2CE2E3B-8A61-4866-9AF5-A12F44CF7233")
                                               ;; before_validation_on_update
                                               (yas/item "86CFB156-E72B-440F-9C7D-08A3375C3ADB")
                                               ;; after_validation
                                               (yas/item "44FBD811-70A9-462B-AC56-F975ADAD62AF")
                                               ;; after_validation_on_create
                                               (yas/item "BA0DE6C7-EAD3-42C9-8ABB-2B9A5F2FE225")
                                               ;; after_validation_on_update
                                               (yas/item "BCB25D36-2D3F-41E9-B2CF-37D6E883E8D1")
                                               ;; before_save
                                               (yas/item "523BE8A6-0845-493D-A9B6-532F73D21950")
                                               ;; after_save
                                               (yas/item "4D1787E3-1583-4CF3-8D99-CC45D7C35EED")
                                               ;; before_create
                                               (yas/item "D64D8863-DCB6-4397-B5B0-073E0AE04167")
                                               ;; after_create
                                               (yas/item "279D1981-B055-4693-B9AF-5B571A62A6AE")
                                               ;; before_destroy
                                               (yas/item "3F4B502B-5F68-4687-88E9-6EF3BDF9677D")
                                               ;; after_update
                                               (yas/item "0C9EA1A1-66C5-4E1C-9C30-E1FFE8EC6EAE")
                                               ;; before_update
                                               (yas/item "1C20EEBE-B4BA-48C8-9B33-7B5BB00D958C")
                                               ;; after_destroy
                                               (yas/item "A2F3E8C1-4216-4890-8491-2F8C7534ED03")))
                                 (yas/submenu "Associations"
                                              (;; belongs_to
                                               (yas/item "B8F08BD7-6160-482C-8A3D-CBC6BD2079A4")
                                               ;; has_and_belongs_to_many
                                               (yas/item "2AC3AC1F-743B-4A33-863C-C37885073806")
                                               ;; has_one
                                               (yas/item "BD2E4045-54E6-450E-B31B-5E1865CFFBC9")
                                               ;; has_many
                                               (yas/item "F396B7BD-8255-48B1-904A-06E7D7CC2741")
                                               ;; has_many :dependent => :destroy
                                               (yas/item "3E3AF538-171B-4108-AB92-827AD7E24C77")
                                               ;; has_many (through)
                                               (yas/item "9D58B6C9-BA52-48B3-B639-D5CB894AF810")
                                               ;; has_many :dependent => :destroy
                                               (yas/item "3E3AF538-171B-4108-AB92-827AD7E24C77")))
                                 (yas/submenu "Scopes"
                                              (;; named_scope
                                               (yas/item "1CB65A0D-4FEC-4438-9B4F-8B0BD13FB875")
                                               ;; named_scope lambda
                                               (yas/item "4E286CB4-069E-474C-A970-95216FE7DE95")))
                                 (yas/submenu "Finders"
                                              (;; find(id)
                                               (yas/item "59CD3A41-8164-4FB4-B462-D7ACE86BCDBF")
                                               ;; find(:all)
                                               (yas/item "A017AB39-A875-40DC-8ACF-7E3551057CA0")
                                               ;; find(:first)
                                               (yas/item "FE430ECD-5D40-4D95-A73B-F064C73992DE")))
                                 (yas/submenu "Validations"
                                              (;; validates_acceptance_of
                                               (yas/item "89198999-7E6D-4D97-A20E-45263E1CA993")
                                               ;; validates_acceptance_of if
                                               (yas/item "A2477223-AD5A-4723-8052-943CE9BA634D")
                                               ;; validates_associated
                                               (yas/item "47944705-F605-4ED4-B4C0-9E823EE25138")
                                               ;; validates_associated if
                                               (yas/item "85E9264C-5414-4FA0-AC07-F305A798ED46")
                                               ;; validates_confirmation_of
                                               (yas/item "B5893618-D07C-48F1-8867-736D0AAFF0E7")
                                               ;; validates_confirmation_of if
                                               (yas/item "1354726C-DA64-4CA6-A099-26626A865D8D")
                                               ;; validates_exclusion_of
                                               (yas/item "4CC98A56-B60B-4A89-80E0-400C5314A050")
                                               ;; validates_exclusion_of if
                                               (yas/item "869AB0B7-12DD-440A-905A-BFB1E0E16E1C")
                                               ;; validates_inclusion_of
                                               (yas/item "4611F02E-E9BF-11DC-8518-00112475D960")
                                               ;; validates_inclusion_of if
                                               (yas/item "47FF50AF-E9BF-11DC-8518-00112475D960")
                                               ;; validates_format_of
                                               (yas/item "EB47FBA1-AFB3-42F9-94A4-552D3175C17A")
                                               ;; validates_format_of if
                                               (yas/item "14BF0586-F2E8-4AB3-BB4B-E49099384403")
                                               ;; validates_length_of
                                               (yas/item "5CE8838A-BF2C-497E-B87A-E90C3BC482E0")
                                               ;; validates_length_of if
                                               (yas/item "EC511A43-D3B7-11DC-BA49-00112475D960")
                                               ;; validates_numericality_of
                                               (yas/item "B21BA16D-7C04-4912-8488-425CDCC332A8")
                                               ;; validates_numericality_of if
                                               (yas/item "CF506019-E964-4172-A3DA-475AE3B65558")
                                               ;; validates_presence_of
                                               (yas/item "5DAC28A7-33C8-4DA7-9E85-56618D6BEC9F")
                                               ;; validates_presence_of if
                                               (yas/item "F5CBBE16-F5CC-4EDA-8BC6-30281BD7D854")
                                               ;; validates_uniqueness_of
                                               (yas/item "F8316545-9AE4-4C7F-87ED-A2C00E6637FA")
                                               ;; validates_uniqueness_of if
                                               (yas/item "43680344-0818-42BF-95B4-58CD2D76545B")))))
                   (yas/submenu "Controllers"
                                (;; Create controller class
                                 (yas/item "4B3F798E-E3B6-48C8-8C2F-CB8631011638")
                                 ;; flash[…]
                                 (yas/item "D864896E-8763-11D9-897C-000393CBCE2E")
                                 (yas/submenu "respond_to"
                                              (;; respond_to (html)
                                               (yas/item "3BDD0D52-443E-4F5F-AE09-ABCC2ABE9A42")
                                               ;; respond_to
                                               (yas/item "B41D3164-EA53-4DDC-850E-27B82B24061F")
                                               ;; wants.format
                                               (yas/item "3F26FDB4-ACF9-4856-9312-6A4D78DC8564")))
                                 (yas/submenu "redirect_to"
                                              (;; redirect_to (path)
                                               (yas/item "A909C4C3-8EFE-4E39-9D96-BA8F0ABE6085")
                                               ;; redirect_to (path plural)
                                               (yas/item "AFE06B67-CE98-42A6-93D1-8EC8E3B9F83C")
                                               ;; redirect_to (nested path)
                                               (yas/item "9D7228B3-A6ED-4598-B096-032B3600864F")
                                               ;; redirect_to (nested path plural)
                                               (yas/item "EF527A27-D1D4-4FD8-BD23-71397881C29A")
                                               ;; redirect_to (action)
                                               (yas/item "F2F3167C-73B9-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (action, id)
                                               (yas/item "2233B484-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller)
                                               (yas/item "053490FE-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller, action)
                                               (yas/item "0C137FBF-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller, action, id)
                                               (yas/item "18D3C1C3-73BA-11D9-B752-000D932CD5BA")))
                                 (yas/submenu "render"
                                              (;; render (action)
                                               (yas/item "7B03D38B-7580-41AC-BC2B-3766AB074A43")
                                               ;; render (action, layout)
                                               (yas/item "053F1D6A-B413-43FF-B697-E3120FD0489F")
                                               ;; render (file)
                                               (yas/item "7D43B0EA-2C3C-499B-9346-A8E48CBF29CD")
                                               ;; render (file, use_full_path)
                                               (yas/item "2A8FBE48-E196-4019-AE76-BF3ED4B54F47")
                                               ;; render (inline)
                                               (yas/item "64E93A71-6E62-48D9-9694-123080AE6723")
                                               ;; render (inline, locals)
                                               (yas/item "1E5DE984-510C-4992-8AD5-C5FA6D7F2A88")
                                               ;; render (inline, type)
                                               (yas/item "A8AF8B90-94E8-42E1-8057-DDBA57809F6A")
                                               ;; render (layout)
                                               (yas/item "3F83272F-62D5-4BCB-BAA3-806083078829")
                                               ;; render (nothing)
                                               (yas/item "AC8A995F-0034-433C-905D-E5C1F29D6EFF")
                                               ;; render (nothing, status)
                                               (yas/item "724A68C1-A727-46FF-AF59-288E26B09629")
                                               ;; render (partial)
                                               (yas/item "498168A5-5AF8-4F59-8A2D-B517FAB98CDB")
                                               ;; render (partial, collection)
                                               (yas/item "046FB1B6-9C65-4702-91EC-4AA9878CD949")
                                               ;; render (partial, locals)
                                               (yas/item "6F41AFFD-B3A7-42D0-8A84-D6086C118D92")
                                               ;; render (partial, object)
                                               (yas/item "BFAAC8DA-A043-4684-967B-B3E5DAE08C62")
                                               ;; render (partial, status)
                                               (yas/item "CBB06A4E-3A82-45F3-91AA-259F02314B9D")
                                               ;; render (text)
                                               (yas/item "67C5082F-5011-434A-8EAA-6B8D3600935F")
                                               ;; render (text, layout)
                                               (yas/item "A3B09AFE-40B5-4623-8B85-E9F369ECE22D")
                                               ;; render (text, layout => true)
                                               (yas/item "97C0992D-715F-4322-A3E0-DD4D2B7E2FC2")
                                               ;; render (text, status)
                                               (yas/item "4F636977-F7A6-4BF5-B09B-7F087683C3B9")
                                               ;; render (update)
                                               (yas/item "ECB10C0B-E8B7-4606-ABF5-4A2A26E5AB1A")))
                                 (yas/submenu "REST methods"
                                              (;; def create - resource
                                               (yas/item "54F61419-001F-4B71-83AC-8DC633694AF0")))
                                 (yas/separator)
                                 ;; verify — render
                                 (yas/item "9ECBF20C-003E-41D9-A881-4BAC0656F9DC")
                                 ;; verify — redirect
                                 (yas/item "7BBD3F57-57A5-4CD0-8E79-B931021FC110")))
                   (yas/submenu "ERb Templates"
                                (;; Create Partial From Selection
                                 (yas/item "1DD8A214-1C97-45BA-ADEE-8F888DDE8570")
                                 (yas/separator)
                                 ;; form_for
                                 (yas/item "7D99041D-C3B7-4940-AE64-6B1758CDB47C")
                                 ;; form_for with errors
                                 (yas/item "15BDD7B6-5C15-4684-93C7-A05E3D2221AC")
                                 (yas/submenu "form_for f. drop-down list"
                                              (;; f.label (ffl)
                                               (yas/item "402C251E-595B-4A58-8EB9-41989040F280")
                                               ;; f.text_field (fftf)
                                               (yas/item "CC1BCD1C-2479-4335-B511-17B880316A75")
                                               ;; f.text_area (ffta)
                                               (yas/item "06498926-F84D-466C-8736-B8A0AC586A94")
                                               ;; f.check_box (ffcb)
                                               (yas/item "F579F9E7-E072-4BCC-BFF9-C8C5BAE7FFA5")
                                               ;; f.radio_box (ffrb)
                                               (yas/item "A95358D2-C68A-4894-8C36-062C9F45848A")
                                               ;; f.password_field (ffpf)
                                               (yas/item "42289456-C8D1-498C-AE30-5206544B349F")
                                               ;; f.hidden_field (ffhf)
                                               (yas/item "5DBA8F72-DD6C-4CBF-83FD-76301E159BA9")
                                               ;; f.file_field (ffff)
                                               (yas/item "79BC2303-3D9D-4E21-AF85-73B388B7B56D")
                                               ;; f.submit (ffs)
                                               (yas/item "C315EC5D-A7F3-49CB-9795-21B78BB42FF4")))
                                 (yas/submenu "form_for helpers"
                                              (;; form_for label
                                               (yas/item "B31822D9-2048-4D16-B2AF-00E0B4E5C368")
                                               ;; form_for text_field
                                               (yas/item "F46EE8EE-239C-46D7-980B-3F861B7D9111")
                                               ;; form_for text_area
                                               (yas/item "4C898FA8-D09C-4B28-BE42-14BB4EA4E2B1")
                                               ;; form_for check_box
                                               (yas/item "F0DB6886-4FFE-45BA-907F-44326AD8142D")
                                               ;; form_for radio_box
                                               (yas/item "D4282CE1-4171-4B13-9220-3F2718BC2505")
                                               ;; form_for password_field
                                               (yas/item "3379FB35-C664-4255-96C6-6E4B91F12759")
                                               ;; form_for hidden_field
                                               (yas/item "99FEFD9B-5A07-46E3-950D-5C474E42B695")
                                               ;; form_for file_field
                                               (yas/item "C8BA285D-E12E-4AB8-A941-514C963E8226")
                                               ;; form_for submit
                                               (yas/item "3000E569-4E19-4566-B08E-A3FFFAAC9075")))
                                 (yas/separator)
                                 ;; form_tag
                                 (yas/item "F0F6DACA-6A0B-11D9-BDC2-000D932CD5BA")
                                 ;; submit_tag
                                 (yas/item "D0E29200-E910-11DC-A399-00112475D960")
                                 ;; image_submit_tag
                                 (yas/item "9FB9848E-EA5A-11DC-9DE5-00112475D960")
                                 (yas/submenu "link_to"
                                              (;; link_to (path)
                                               (yas/item "326B57A7-B4A9-447B-A3D2-0EA74158E1E1")
                                               ;; link_to (path plural)
                                               (yas/item "6BA737F0-63D1-4D82-9381-4331E18B12C5")
                                               ;; link_to (nested path)
                                               (yas/item "750DEEF9-18A0-40FC-8E54-574CE5EE5565")
                                               ;; link_to (nested path plural)
                                               (yas/item "866AAD87-E458-4F2D-9E7C-3CE73EFC047B")
                                               ;; link_to (action)
                                               (yas/item "9E2B42FE-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (action, id)
                                               (yas/item "B4F952F4-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller)
                                               (yas/item "74590E16-7BCB-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller, action)
                                               (yas/item "C11C0BF5-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller, action, id)
                                               (yas/item "D21BE958-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to model
                                               (yas/item "E5E08AA0-4EDD-4583-BF07-5D6C49E98410")))
                                 ;; end (ERB)
                                 (yas/item "AC385ABF-96CD-4FCB-80AD-BF37D6EE79D2")
                                 (yas/separator)
                                 ;; map(&:sym_proc)
                                 (yas/item "EC605540-C431-4FD0-AD91-D913118DACA7")
                                 (yas/separator)
                                 ;; for loop in rhtml
                                 (yas/item "F7744F07-306C-4951-AB5A-3D69BA5516B7")))
                   (yas/submenu "Layouts"
                                (;; javascript_include_tag
                                 (yas/item "FEF49C86-9386-405E-A191-684D1C963E3A")
                                 ;; stylesheet_link_tag
                                 (yas/item "980C7667-9D60-49FF-AF74-A7B19B379F45")))
                   (yas/submenu "RJS"
                                (;; page.replace (id, partial)
                                 (yas/item "273E5E76-8D13-4476-9C38-8AF87432CB96")
                                 ;; page.hide (*ids)
                                 (yas/item "390A447F-0FA3-4F01-A10C-4F35675E0A43")
                                 ;; page.replace_html (id, partial)
                                 (yas/item "8B914165-9C66-4FA3-9AD6-1DA41B25F8F1")
                                 ;; page.insert_html (position, id, partial)
                                 (yas/item "62BEA590-F4EF-4001-B661-764EDFB92811")
                                 ;; page.visual_effect (effect, id)
                                 (yas/item "CFDC27A3-58CF-4198-8F93-36360978F0D0")
                                 ;; page.show (*ids)
                                 (yas/item "5ACBF49D-B5A5-495C-89D8-18AA740D9D02")
                                 ;; page.toggle (*ids)
                                 (yas/item "028DA0A4-B310-4BEF-8643-2A22993C21C7")))
                   (yas/submenu "Migrations"
                                (;; Ignoring Quick Migration
                                 (yas/ignore-item "D696FA2C-785A-4B73-A2F6-F750904DD7C2")
                                 
                                 (yas/submenu "Columns"
                                              (;; Add / Remove Column
                                               (yas/item "18C76913-061C-4D65-866D-67AA3724AFEF")
                                               ;; Ignoring Add / Remove Several Columns
                                               (yas/ignore-item "7BC860E6-7561-4E6E-983B-507D7A6F6228")
                                               ;; Add / Remove Several Columns (marcc)
                                               (yas/item "27A6C58A-896B-4956-BA81-D671A2EF9C7D")
                                               ;; Ignoring Rename / Rename Column
                                               (yas/ignore-item "AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3")
                                               ;; Ignoring Rename / Rename Several Columns
                                               (yas/ignore-item "F03162DE-9DB6-417B-9DD7-52D9F11EA736")
                                               ;; Ignoring Rename / Rename Several Columns (mncc)
                                               (yas/ignore-item "04A86178-71B1-430A-A06D-DFF7C9A338B5")
                                               ;; Remove / Add Column
                                               (yas/item "16A705EB-10DC-42B5-9FF2-377E206421DC")))
                                 (yas/submenu "Tables"
                                              (;; Ignoring Create / Drop Table
                                               (yas/ignore-item "25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2")
                                               ;; Ignoring Rename / Rename Table
                                               (yas/ignore-item "FD8CC811-2AD3-480F-B975-DF959DC96C67")
                                               ;; Drop / Create Table
                                               (yas/item "20375601-B13F-4314-B8E4-362706566636")
                                               ;; Ignoring Change / Change Table
                                               (yas/ignore-item "20FC02C5-32A3-4F20-B163-FF75C9FDFABF")
                                               
                                               (yas/submenu "Create columns t. drop-down list"
                                                            (;; t.string (tcs)
                                                             (yas/item "B757F7E5-E4BD-11DC-A11A-00112475D960")
                                                             ;; t.text (tct)
                                                             (yas/item "FFE7B820-E4BD-11DC-A11A-00112475D960")
                                                             ;; t.integer (tci)
                                                             (yas/item "0E63B7D5-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.float (tcf)
                                                             (yas/item "1BDC463A-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.decimal (tcd)
                                                             (yas/item "26C09807-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.datetime (tcdt)
                                                             (yas/item "3458B140-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.timestamp (tcts)
                                                             (yas/item "49643690-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.time (tcti)
                                                             (yas/item "537BDD48-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.date (tcda)
                                                             (yas/item "61CF5B32-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.boolean (tcb)
                                                             (yas/item "6BE6F315-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.binary (tcbi)
                                                             (yas/item "7CE57C6C-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.timestamps (tctss)
                                                             (yas/item "950B0BF2-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.lock_version (tcl)
                                                             (yas/item "A677FFD4-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.references (tcr)
                                                             (yas/item "B6D9225C-E4BE-11DC-A11A-00112475D960")))
                                               (yas/submenu "Create columns helpers"
                                                            (;; Table column string
                                                             (yas/item "377BF814-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column text
                                                             (yas/item "6A9D4C30-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column integer
                                                             (yas/item "729D559E-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column float
                                                             (yas/item "8AF989C4-D52E-11DC-BD8E-00112475D960")
                                                             ;; Table column decimal
                                                             (yas/item "93A16768-D52E-11DC-BD8E-00112475D960")
                                                             ;; Table column datetime
                                                             (yas/item "D6CBCA96-D52F-11DC-BD8E-00112475D960")
                                                             ;; Table column timestamp
                                                             (yas/item "4600CE20-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column time
                                                             (yas/item "4F5DDD37-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column date
                                                             (yas/item "56276686-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column boolean
                                                             (yas/item "967093B4-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column binary
                                                             (yas/item "5E9B8B0E-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column timestamps
                                                             (yas/item "E0C8FDC4-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column lock_version
                                                             (yas/item "FC2523C1-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column(s) references
                                                             (yas/item "EDA6568B-D533-11DC-BD8E-00112475D960")))
                                               ;; Create Column in Table
                                               (yas/item "7592CA99-75D7-48B6-9133-00B9F148FF43")
                                               ;; Create Several Columns in Table
                                               (yas/item "67FD2F8F-5F25-45F2-A451-2F39977A9EDE")
                                               (yas/submenu "Change columns t. drop-down list"
                                                            (;; t.rename (tre)
                                                             (yas/item "9D4E30E2-4A61-4941-B9F3-BEE97552747A")))
                                               (yas/submenu "Change columns helpers"
                                                            (;; Table column(s) rename
                                                             (yas/item "DF30226E-1111-448A-B669-7CA34EE83909")))))
                                 (yas/submenu "Indexes"
                                              (;; Ignoring Add / Remove Index
                                               (yas/ignore-item "95F83E1D-5B03-424F-8BEC-8AF66C8939BC")
                                               ;; Ignoring Add / Remove Named Index
                                               (yas/ignore-item "A7F692C1-778A-48B8-945E-573568BA0403")
                                               ;; Ignoring Add / Remove Unique Index
                                               (yas/ignore-item "33057A79-677B-4DFB-99D4-1492778BDDC6")))))
                   (yas/submenu "Routes"
                                (;; map.named_route
                                 (yas/item "91C543BF-7BD8-4E3A-B493-AE572C5472A0")
                                 ;; map.resources
                                 (yas/item "0FF86C46-0E01-4D03-8232-72CA5BD55706")
                                 ;; map.resource
                                 (yas/item "2183A9A9-17ED-4A4F-ABB6-668EDDD3A6E4")
                                 ;; map.with_options
                                 (yas/item "BD4B90F7-2187-4E75-BFFB-77BE67CB8DAE")
                                 ;; map.catch_all
                                 (yas/item "F3606586-F905-4A91-92CA-82319239221D")))
                   (yas/submenu "ActiveSupport"
                                (;; cattr_accessor
                                 (yas/item "F57522B2-9F5F-4DF9-AE46-9478AF019C63")
                                 ;; mattr_accessor
                                 (yas/item "B25B7560-FACB-4A9E-A226-B71C796BD1F3")
                                 ;; returning do |variable| … end
                                 (yas/item "D2783155-23F3-4B90-A317-5BD139471193")))
                   (yas/separator)
                   (yas/submenu "Fixtures"
                                (;; $LABEL
                                 (yas/item "786980D8-FA69-4542-85A3-5E48CFAA6814")
                                 ;; <%= Fixtures.identify(:symbol) %>
                                 (yas/item "9671EB7A-89D6-4C23-914F-88CBEE0D177A")
                                 (yas/separator)
                                 ;; Ignoring Autocomplete Foreign Key Fixture Reference
                                 (yas/ignore-item "0BCF0EE2-35EE-4959-A771-E74D55271D5A")
                                 ;; Ignoring Autocomplete Foreign Key Fixture Reference (habtm)
                                 (yas/ignore-item "275C0B86-F735-49B6-8A22-218A8F4CC2E0")))
                   (yas/submenu "Unit Tests"
                                (;; assert_difference
                                 (yas/item "30BEA6FB-301C-4460-93EC-FA3404688962")
                                 ;; assert_no_difference
                                 (yas/item "5C6F4462-70E6-40B4-B3F2-F371656E7784")
                                 (yas/separator)))
                   (yas/submenu "Functional Tests"
                                (;; Create functional test class
                                 (yas/item "F60D0630-CBF5-4283-9D20-FA46C787A88D")
                                 ;; def test_should_get_action
                                 (yas/item "1C491A76-751F-44EF-8DFB-0A585C7EEFF6")
                                 ;; def test_should_post_action
                                 (yas/item "8B9CD068-4338-4039-AA06-D839A6C7A9FF")
                                 (yas/separator)
                                 ;; assert_response
                                 (yas/item "2BD82DCB-1F19-4C8F-BC70-C0BBB06A2138")
                                 ;; assert_redirected_to
                                 (yas/item "CD60F800-850D-47CF-BE32-3DE665DD5C68")
                                 ;; assert_redirected_to (path)
                                 (yas/item "D33EDCE7-F8AF-48D4-AA7A-852BBF03E31D")
                                 ;; assert_redirected_to (path plural)
                                 (yas/item "0249637E-0720-46DA-A8FD-E176A2CC458B")
                                 ;; assert_redirected_to (nested path)
                                 (yas/item "97021C0D-EB65-4046-B688-01F09B3B1615")
                                 ;; assert_redirected_to (nested path plural)
                                 (yas/item "4C92C020-7337-4D6E-91EE-7ABF2BFC7F41")
                                 (yas/separator)
                                 ;; assert_select
                                 (yas/item "DBE14FE8-B415-4DBC-A316-F8DA63FE9FD7")
                                 (yas/separator)
                                 ;; assert_rjs
                                 (yas/item "E0F281EC-5311-41F8-ADD9-2E2D059DA651")
                                 (yas/separator)
                                 ;; assert(var = assigns(:var))
                                 (yas/item "FE9C4B4E-860D-49F0-AAF7-5582B98F5F54")))
                   (yas/submenu "Ajax Tests"
                                (;; xhr post
                                 (yas/item "62C3838B-0790-4FC2-8425-F273A57F5D33")
                                 ;; xhr get
                                 (yas/item "78FCF992-D01B-404F-BC54-5EE7B91F999A")
                                 ;; xhr delete
                                 (yas/item "F1BE0C3D-7203-43E9-BEFB-D1A99CDD31C1")
                                 ;; xhr put
                                 (yas/item "C12C98A5-74E5-4E70-9ADB-8783455D6539")))
                   (yas/separator)
                   ;; Ignoring View demo help
                   (yas/ignore-item "964436B8-E578-11DC-8177-00112475D960"))
                    '("A2135370-67A1-488D-B43C-B4F221127C2F"
                       "809BCA42-5C49-4B08-B3C4-BB773036C086"
                       "1970AE74-3949-40B3-B263-727AA3FF167A"
                       "638D94A4-BDFC-4FE9-8909-9934F3FD2899"
                       "F758BFD1-00CA-4742-BE71-032580080F5C"
                       "6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1"
                       "07C696F8-79F5-4E0B-9EE9-03B693A54ABB"
                       "D696FA2C-785A-4B73-A2F6-F750904DD7C2"
                       "DC549A45-D9B0-11DC-94E9-00112475D960"
                       "964436B8-E578-11DC-8177-00112475D960"
                       "5EEA0C71-B34B-4408-953B-F47AAD343CCC"
                       "0BCF0EE2-35EE-4959-A771-E74D55271D5A"
                       "0D966168-D9B1-11DC-94E9-00112475D960"
                       "190401C2-D9B1-11DC-94E9-00112475D960"
                       "212C3047-D9B1-11DC-94E9-00112475D960"
                       "F4EA552D-D9B0-11DC-94E9-00112475D960"
                       "04A30A4D-D9B1-11DC-94E9-00112475D960"
                       "275C0B86-F735-49B6-8A22-218A8F4CC2E0"
                       "2C60CBA1-D9B1-11DC-94E9-00112475D960"
                       "9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29"
                       "B207BBD4-D6AA-41E9-9530-27210F2D7B66"
                       "B078346F-61D8-4E75-9427-80720FBC67F7"
                       "6DEF923E-2347-46EC-AFBE-183D08E63DC1"
                       "4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE"
                       "46ECE243-0448-4A64-A223-27CC21E7704D"
                       "310C901C-EF32-4E88-938A-804ABBF8C428"
                       "51C9C27A-D931-49F9-B6D8-C0E7ABEC992D"
                       "985F56D4-82ED-4C45-8250-2ECCFC71957E"
                       "25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2"
                       "04A86178-71B1-430A-A06D-DFF7C9A338B5"
                       "FD8CC811-2AD3-480F-B975-DF959DC96C67"
                       "AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3"
                       "95F83E1D-5B03-424F-8BEC-8AF66C8939BC"
                       "33057A79-677B-4DFB-99D4-1492778BDDC6"
                       "A7F692C1-778A-48B8-945E-573568BA0403"
                       "20FC02C5-32A3-4F20-B163-FF75C9FDFABF"
                       "A2135370-67A1-488D-B43C-B4F221127C2F"
                       "7BC860E6-7561-4E6E-983B-507D7A6F6228"
                       "F03162DE-9DB6-417B-9DD7-52D9F11EA736"
                       "809BCA42-5C49-4B08-B3C4-BB773036C086"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Macros/Remove 3A Add Column.yasnippet
;; 809BCA42-5C49-4B08-B3C4-BB773036C086                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Named Index.yasnippet
;; A7F692C1-778A-48B8-945E-573568BA0403                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; 275C0B86-F735-49B6-8A22-218A8F4CC2E0                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Uncommitted.yasnippet
;; 212C3047-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; 0BCF0EE2-35EE-4959-A771-E74D55271D5A                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Change Table.yasnippet
;; 20FC02C5-32A3-4F20-B163-FF75C9FDFABF                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; 985F56D4-82ED-4C45-8250-2ECCFC71957E                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Import.yasnippet
;; 6DEF923E-2347-46EC-AFBE-183D08E63DC1                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; D696FA2C-785A-4B73-A2F6-F750904DD7C2                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures (Test DB).yasnippet
;; F758BFD1-00CA-4742-BE71-032580080F5C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Plugins.yasnippet
;; 0D966168-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Recent.yasnippet
;; 190401C2-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test All.yasnippet
;; DC549A45-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Unique Index.yasnippet
;; 33057A79-677B-4DFB-99D4-1492778BDDC6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Column.yasnippet
;; AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Create 3A Drop Table.yasnippet
;; 25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Helper.yasnippet
;; 51C9C27A-D931-49F9-B6D8-C0E7ABEC992D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Dump.yasnippet
;; 310C901C-EF32-4E88-938A-804ABBF8C428                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Migrate to Previous Version.yasnippet
;; 9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Functionals.yasnippet
;; F4EA552D-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Drop 3A Create Table.yasnippet
;; A2135370-67A1-488D-B43C-B4F221127C2F                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Index.yasnippet
;; 95F83E1D-5B03-424F-8BEC-8AF66C8939BC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures.yasnippet
;; 5EEA0C71-B34B-4408-953B-F47AAD343CCC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Clone Development DB to Test DB.yasnippet
;; 6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns.yasnippet
;; F03162DE-9DB6-417B-9DD7-52D9F11EA736                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Javascript.yasnippet
;; B078346F-61D8-4E75-9427-80720FBC67F7                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Stylesheet.yasnippet
;; B207BBD4-D6AA-41E9-9530-27210F2D7B66                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate to Version.yasnippet
;; 07C696F8-79F5-4E0B-9EE9-03B693A54ABB                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/View demo help.yasnippet
;; 964436B8-E578-11DC-8177-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go to Fixture.yasnippet
;; 638D94A4-BDFC-4FE9-8909-9934F3FD2899                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Table.yasnippet
;; FD8CC811-2AD3-480F-B975-DF959DC96C67                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns (mncc).yasnippet
;; 04A86178-71B1-430A-A06D-DFF7C9A338B5                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate.yasnippet
;; 4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Integration.yasnippet
;; 04A30A4D-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; 1970AE74-3949-40B3-B263-727AA3FF167A                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns.yasnippet
;; 7BC860E6-7561-4E6E-983B-507D7A6F6228                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Install Bundled Plugin.yasnippet
;; 46ECE243-0448-4A64-A223-27CC21E7704D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Units.yasnippet
;; 2C60CBA1-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Commands/Rake Migrate.yasnippet
;;                                                                                            =yyas> (yas/unknown)
;; 
;; # as in Snippets/stylesheet_link_tag.yasnippet
;; text.html.ruby                                                                             =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; ~$                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; ^M                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; ~@                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/View demo help.yasnippet
;; ^h                                                                                         =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for rails-mode ends here
;; Supporting elisp for subdir ruby-mode

;; .yas-setup.el for ruby-mode
;; -*- coding: utf-8 -*-
;;
(defvar yas/ruby-snippet-open-paren " "
  "The open parenthesis used in ruby-mode snippets. Normally blank but could be (")
(defvar yas/ruby-snippet-close-paren " "
  "The close parenthesis used in ruby-mode snippets. Normally blank but could be )")
(defvar yas/ruby-shebang-args " -wKU"
  "Arguments for the ruby shebang line.")

(defun yas/ruby-infer-class-name ()
  "Infer the class name from the buffer. Thanks to hitesh <hitesh.jasani@gmail.com>"
  (if buffer-file-name
      (let ((fn (capitalize (file-name-nondirectory
                             (file-name-sans-extension
                              (buffer-file-name))))))
        (cond
         ((string-match "_" fn) (replace-match "" nil nil fn))
         (t fn)))
    "SomeClass"))

(defun yas/ruby-chomp (x)
  "Chomp string X, return nil if X became empty"
  (let ((len (length x))
        (start 0)
        (end (1- (length x))))
    (unless (zerop len)
      (while (and (< start len)
                  (memq (aref x start)
                        '(?  ?\t ?\n)))
        (setq start (1+ start)))
      (while (and (> end start)
                  (memq (aref x end)
                        '(?  ?\t ?\n)))
        (setq end (1- end)))
      (unless (<= end start)
        (substring x start (1+ end))))))

(defvar yas/ruby-block-start-regexp "\\(^\\|[\s\t\n^]\\)\\(do\\)[\s\t\n]\\(|.*|\\)?")

(defun yas/ruby-toggle-single-multi-line-block ()
  "Toggle \"do .. end\" blocks into  \"{ .. }\" blocks back and forth."
  ;;
  ;; TODO: Some code to be refactored here.
  ;; 
  ;; FIXME: correctly detect statements in { .. } block, split-string(";") is no good
  ;;
  (interactive)
  (let* ((do-block-bounds (save-excursion
                            (when (or (save-excursion (beginning-of-line)
                                                      (looking-at yas/ruby-block-start-regexp))
                                      (save-excursion (ruby-beginning-of-block)
                                                      (looking-at yas/ruby-block-start-regexp)))
                              (cons (match-beginning 1)
                                    (progn (goto-char (match-beginning 1))
                                           (ruby-end-of-block) (point))))))
         (brace-block-bounds (condition-case nil
                                 (let ((syntax-info (syntax-ppss)))
                                   (if (fourth syntax-info)
                                       (goto-char (ninth syntax-info)))
                                   (while (progn (up-list -1) (not (eq (char-after) ?{))))
                                   (cons (point)
                                         (progn (forward-sexp) (point))))
                               (error nil)))
         (block-region)
         (statements))
    (if (and do-block-bounds brace-block-bounds)
        (if (< (car do-block-bounds) (car brace-block-bounds))
            (setq do-block-bounds nil)
          (setq brace-block-bounds nil)))
    (cond (do-block-bounds
           (goto-char (car do-block-bounds))
           (setq block-region (buffer-substring-no-properties (+ 2 (car do-block-bounds)) (cdr do-block-bounds)))
           (delete-region (car do-block-bounds) (+ 3 (cdr do-block-bounds)))
           (insert "{")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas/ruby-chomp
                                                (split-string block-region "\n"))))
           (mapc #'(lambda (string)
                     (insert " " string)
                     (if (member (aref string (1- (length string))) '(?;
                                                                      ?|))
                         (insert " ")
                       (insert ";")))
                 statements)
           (when statements (delete-backward-char 1))
           (save-excursion
             (insert " }")))
          (brace-block-bounds
           ;; (message "found a brace block")
           (goto-char (car brace-block-bounds))
           (setq block-region (buffer-substring (1+ (car brace-block-bounds)) (1- (cdr brace-block-bounds))))
           (delete-region (car brace-block-bounds) (cdr brace-block-bounds))
           (insert "do")
           (when (string-match "\\(|.*|\\).*" block-region)
             (insert " " (match-string 1 block-region))
             (setq block-region (substring block-region (match-end 1))))
           (setq statements (remove nil (mapcar #'yas/ruby-chomp
                                                (split-string block-region ";"))))
           (mapc #'(lambda (string)
                     (insert "\n" string)
                     (indent-according-to-mode))
                 statements)
           (unless statements (insert "\n") (indent-according-to-mode))
           (save-excursion
             (insert "\nend")
             (indent-according-to-mode)))
          (t
           (message "No enclosing block found.")))))

(defvar yas/ruby-require-regexps
  '(("abbrev"                            . ("abbrev"))
    ("base64"                            . ("Base64"))
    ("benchmark"                         . ("Benchmark"))
    ("bigdecimal"                        . ("BigDecimal"))
    ("bigdecimal/math"                   . ("BigMath"))
    ("cgi"                               . ("CGI"))
    ("complex"                           . ("Complex"))
    ("csv"                               . ("CSV"))
    ("curses"                            . ("Curses"))
    ("date"                              . ("Date(?:Time)?"))
    ("dbm"                               . ("DBM"))
    ("delegate"                          . ("DelegateClass" "Delegator" "SimpleDelegator "))
    ("digest"                            . ("MD5" "SHA1"))
    ("dl"                                . ("DL"))
    ("enumerator"                        . ("(?:enum|each)_(?:cons|slice)" "enum_(?:for|with_index)" "to_enum "))
    ("erb"                               . ("ERB"))
    ("etc"                               . ("Etc"))
    ("fcntl"                             . ("Fcntl"))
    ("fileutils"                         . ("FileUtils"))
    ("find"                              . ("Find(?:\.|::)find"))
    ("forwardable"                       . ("(?:Single)?Forwardable"))
    ("gdbm"                              . ("GDBM"))
    ("generator"                         . ("Generator" "SyncEnumerator"))
    ("getoptlong"                        . ("GetoptLong"))
    ("gserver"                           . ("GServer"))
    ("iconv"                             . ("Iconv"))
    ("ipaddr"                            . ("IpAddr"))
    ("logger"                            . ("Logger"))
    ("matrix"                            . ("Matrix" "Vector"))
    ("monitor"                           . ("Monitor(?:Mixin)?"))
    ("net/ftp"                           . ("Net::FTP"))
    ("net/http"                          . ("Net::HTTP"))
    ("net/imap"                          . ("Net::IMAP"))
    ("net/pop"                           . ("Net::(?:APOP|POP3)"))
    ("net/smtp"                          . ("Net::SMTP"))
    ("net/telnet"                        . ("Net::Telnet"))
    ("nkf"                               . ("NKF"))
    ("observer"                          . ("Observable"))
    ("open3"                             . ("Open3"))
    ("optparse"                          . ("OptionParser"))
    ("ostruct"                           . ("OpenStruct"))
    ("pathname"                          . ("Pathname"))
    ("ping"                              . ("Ping"))
    ("pp"                                . ("pp"))
    ("pstore"                            . ("PStore"))
    ("rational"                          . ("Rational"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("rdoc/markup/simple_markup"         . ("SM::SimpleMarkup"))
    ("rdoc/markup/simple_markup/to_html" . ("SM::SimpleMarkup"))
    ("rdoc/usage"                        . ("RDoc(?:\.|::)usage"))
    ("resolv"                            . ("Resolv"))
    ("rexml/document"                    . ("REXML"))
    ("rinda/tuplespace"                  . ("Rinda::TupleSpace(?:Proxy)?"))
    ("rinda/ring"                        . ("Rinda::Ring(?:Finger|Server)?"))
    ("rss"                               . ("RSS"))
    ("scanf"                             . ("scanf"))
    ("sdbm"                              . ("SDBM"))
    ("set"                               . ("(?:Sorted)?Set"))
    ("singleton"                         . ("Singleton"))
    ("soap"                              . ("SOAP"))
    ("socket"                            . (" (?:TCP|UNIX)(?:Socket|Server)" "(?:UDP)?Socket"))
    ("stringio"                          . ("StringIO"))
    ("strscan"                           . ("StringScanner"))
    ("syslog"                            . ("Syslog"))
    ("tempfile"                          . ("Tempfile"))
    ("test/unit"                         . ("Test::Unit"))
    ("thread"                            . (" ConditionVariable" "Mutex" "(?:Sized)?Queue "))
    ("time"                              . ("Time(?:\.|::)parse"))
    ("timeout"                           . ("Timeout(?:\.|::)timeout"))
    ("tk"                                . ("TK"))
    ("tmpdir"                            . ("Dir(?:\.|::)tmpdir"))
    ("tracer"                            . ("Tracer"))
    ("tsort"                             . ("TSort"))
    ("uri"                               . ("URI"))
    ("weakref"                           . ("WeakRef"))
    ("webrick"                           . ("WEBrick"))
    ("Win32API"                          . ("Win32(?:API)?"))
    ("win32ole"                          . ("WIN32OLE"))
    ("wsdl"                              . ("WSDL"))
    ("xmlrpc"                            . ("XMLRPC"))
    ("yaml"                              . ("YAML"))
    ("zlib"                              . ("Zlib"))))

(defun yas/ruby-require (package)
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward-regexp (format "^[\s\t]*require[( ][ ]*\"%s\"[ )]*$"
                                           package) nil t)
      (unless (search-forward-regexp "^[\s\t]*require.*\n" nil t)
        (search-forward-regexp "^[\s\t]*[^#]" nil t)
        (goto-char (line-beginning-position)))
      (insert "require \"" package "\"\n"))))

(defun yas/ruby-pipe-through-xmpfilter ()
  (interactive)
  (let ((start (or (and mark-active
                        (region-beginning))
                   (point-min)))
        (end (or (and mark-active
                      (region-end))
                 (point-max)))
        (orig (point))
        retval
        (orig-line (count-screen-lines (window-start) (line-beginning-position))))
    
    (unless (zerop (shell-command-on-region start end "xmpfilter" (get-buffer-create "*xmpfilter*") t (get-buffer-create "*xmpfilter errors*") t))
      ;;some undo actions here
      )
    (goto-char (min (point-max) orig))
    (recenter orig-line)
    retval))

(put (intern "ruby-thing") 'bounds-of-thing-at-point 'yas/ri-ruby-thing-bounds)
(defun yas/ri-ruby-thing-bounds ()
  (let ((start (point))
        (end (point)))
    (save-excursion
      (while (not (and (zerop (skip-syntax-forward "\w\_"))
                       (zerop (skip-chars-forward "#:"))))
        (setq end (point)))
      (while (not (and (zerop (skip-syntax-backward "\w\_"))
                       (zerop (skip-chars-backward "#:"))))
        (setq start (point))))
    (unless (= start end)
      (cons start end))))

(defvar yas/ri-history nil
  "History of yas/ri queries.")
(require 'ansi-color)
(defun yas/ri (query)
  (interactive (list (read-from-minibuffer "Ri query: "
                                           (thing-at-point 'ruby-thing)
                                           nil
                                           nil
                                           'ri-history)))
  (with-current-buffer (get-buffer-create "*Ri*")
    (setq buffer-read-only nil)
    (shell-command (concat "ri -f ansi " query) "*Ri*")
    (ansi-color-apply-on-region (point-min) (point-max))
    (yas/ri-mode)
    (display-buffer (current-buffer)))
  t)

(defun yas/ri-mode ()
  "Mode for viewing Ruby documentation."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "RET") 'yas/ri)
  (setq mode-name "ri")
  (setq major-mode 'yas/ri-mode)
  (setq buffer-read-only t)
  (run-hooks 'yas/ri-mode-hook))

;; conditions
;; 
(yas/define-condition-cache yas/ruby-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\`)))
(yas/define-condition-cache yas/ruby-in-comment-p (fifth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-in-string-p (fourth (syntax-ppss)))
(yas/define-condition-cache yas/ruby-end-is-block-end-p
                            (save-excursion
                              (ruby-backward-sexp)
                              (not (eq (point) (point-min)))))

;; My work in progress substitutions
;;
;; Substitutions for: content
;;
;; ${1/.+/(/}                                                                        =yyas> ${1:$(and (yas/text) "(")}
;; ${1/.+/)/}                                                                        =yyas> ${1:$(and (yas/text) ")")}
;; ${2/.+/ => /}                                                                     =yyas> ${2:$(and (yas/text) " => ")}
;; ${1:${TM_FILENAME/\.\w+//}                                                        =yyas> ${1:$(and buffer-file-name (file-name-sans-extension buffer-file-name))}
;; ${1/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${1/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${2/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${3/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${3/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) " " )}
;; ${3/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) " " )}
;; ${3/(^[rwab+]+$)|.*/(?1:, ")/}                                                    =yyas> ${3:$(and (string-match "^[rwab+]+$" yas/text) ", \\"" )}
;; ${3/(^[rwab+]+$)|.*/(?1:")/}                                                      =yyas> ${3:$(and (string-match "^[rwab+]+$" yas/text) "\\"" )}
;; ${3/^\s*$|(.*\S.*)/(?1:, )/}                                                      =yyas> ${3:$(and (string-match "[^\s\t]" (yas/text) ", ")}
;; ${TM_SELECTED_TEXT/([\t ]*).*/$1/m}                                               =yyas>
;; ${TM_SELECTED_TEXT/(\A.*)|(.+)|\n\z/(?1:$0:(?2:\t$0))/g}                          =yyas> `yas/selected-text`
;; (yas/multi-line-unknown BF487539-8085-4FF4-8601-1AD20FABAEDC)                     =yyas> `(yas/ruby-infer-class-name)`
;; (yas/multi-line-unknown 2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC)                     =yyas> `(yas/ruby-infer-class-name)`
;; 
;; ${TM_FILENAME/(?:\A|_)([A-Za-z0-9]+)(?:\.rb)?/(?2::\u$1)/g}                       =yyas> `(yas/ruby-infer-class-name)`
;; 
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}             =yyas> ${1:$(and (yas/text) "|")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}            =yyas> ${1:$(and (yas/text) " |")}
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}            =yyas> ${1:$(and (yas/text) "| ")}
;;
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${1:$(and (yas/text) "|")}
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${1:$(and (yas/text) "| ")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}   =yyas> ${2:$(and (yas/text) "|")}
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}  =yyas> ${2:$(and (yas/text) "| ")}
;; 
;; ${1/([\w&&[^_]]+)|./\u$1/g}                                                       =yyas> ${1:$(replace-regexp-in-string "[_/]" "" (capitalize yas/text))}
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B               =yyas> (yas/ruby-toggle-single-multi-line-block)
;; 7E084412-80E6-4B70-8092-C03D1ECE4CD2               =yyas> (yas/ruby-require "eac")(yas/expand-uuid 'ruby-mode "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
;; FBFC214F-B019-4967-95D2-028F374A3221               =yyas> (yas/ruby-pipe-through-xmpfilter)
;; 63F3B3B7-CBE2-426B-B551-657733F3868B               =yyas> (call-interactively (if (featurep 'ri) 'ri 'yas/ri))

;;
;; `[[ $TM_LINE_INDEX != 0 ]] && echo; echo`                                         =yyas> `(concat (if (eq 0 current-line) "\n" "") "\n")`
;; `snippet_paren.rb`                                                                =yyas> `yas/ruby-snippet-open-paren`
;; `snippet_paren.rb end`                                                            =yyas> `yas/ruby-snippet-close-paren`
;; ${TM_RUBY_SWITCHES: -wKU}                                                         =yyas> `yas/ruby-shebang-args`
;; 
;; Substitutions for: condition
;;
;; 7990EE60-C850-4779-A8C0-7FD2C853B99B                                              =yyas> 'force-in-comment
;; FBFC214F-B019-4967-95D2-028F374A3221                                              =yyas> 'force-in-comment
;; 88BC3896-DC39-4307-A271-21D33340F15A                                              =yyas> 'force-in-comment
;; 0F940CBC-2173-49FF-B6FD-98A62863F8F2                                              =yyas> 'force-in-comment
;; 451A0596-1F72-4AFB-AF2F-45900FABB0F7                                              =yyas> (not (yas/ruby-end-is-block-end-p))
;; (string.quoted.double.ruby|string.interpolated.ruby) - string source              =yyas> (and (yas/ruby-in-interpolated-string-p) 'force-in-comment)
;; text.html.ruby, text.html source.ruby                                             =yyas> (yas/unimplemented)
;; text.html, source.yaml, meta.erb                                                  =yyas> (yas/unimplemented)
;; keyword.control.start-block.ruby, meta.syntax.ruby.start-block                    =yyas>
;; 
;; Substitutions for: binding
;;
;; # as in Commands/New Method.yasnippet
;; $                                                                               =yyas> C-c M-m
;; ^W                                                                                =yyas> C-c M-w
;; #                                                                                 =yyas> #
;; ^{                                                                                =yyas> C-c M-{
;; @R                                                                                =yyas> C-c M-R
;; @r                                                                                =yyas> C-c M-r
;; ^R                                                                                =yyas> C-c M-S-r
;; @i                                                                                =yyas> s-i
;; @b                                                                                =yyas> s-b
;; ^@E                                                                               =yyas> C-c M-e
;; ^:                                                                                =yyas> C-c M-:
;; ^>                                                                                =yyas> C-c M->
;; ^h                                                                                =yyas> C-c M-h
;;
;;
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; @k                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; ^@O                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in (RDoc comments).yasnippet
;; @b                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; @D                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'ruby-mode
                 '(;; Ignoring Run
                   (yas/ignore-item "35222962-C50D-4D58-A6AE-71E7AD980BE4")
                   ;; Ignoring Run Focused Unit Test
                   (yas/ignore-item "5289EE40-86B8-11D9-A8D4-000A95E13C98")
                   ;; Ignoring Run Rake Task
                   (yas/ignore-item "569C9822-8C41-4907-94C7-1A8A0031B66D")
                   
                   ;; Documentation for Word / Selection
                   (yas/item "63F3B3B7-CBE2-426B-B551-657733F3868B")
                   (yas/submenu "RDoc"
                                (;; Ignoring Show for Current File / Project
                                 (yas/ignore-item "1AD6A138-2E89-4D6A-AB3F-416BF9CE968D")
                                 
                                 (yas/submenu "Format"
                                              (;; Ignoring Bold
                                               (yas/ignore-item "931DD73E-615E-476E-9B0D-8341023AE730")
                                               ;; Ignoring Italic
                                               (yas/ignore-item "DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE")
                                               ;; Ignoring Typewriter
                                               (yas/ignore-item "2DDB6FE0-6111-4C40-A149-8E67E76F8272")))
                                 
                                 ;; New Block
                                 (yas/item "05984208-D559-4C04-A69C-2019361A985A")
                                 ;; Ignoring Omit
                                 (yas/ignore-item "BF4CA9F1-51CD-48D4-8357-852234F59046")
                                 
                                 ;; :yields:
                                 (yas/item "ED6368FB-A11D-4622-9F42-7879481094F1")))
                   (yas/separator)
                   (yas/submenu "Rake"
                                (;; namespace :name ... end
                                 (yas/item "A3D89AAA-9156-4077-A026-37BB7358C3BA")
                                 ;; namespace :name ... task :default ... end
                                 (yas/item "2031FC41-CBD3-41CC-B9A9-7F068E607A05")
                                 ;; desc ...
                                 (yas/item "F686E1AD-B03D-45A6-BD51-6E3FD1298FE0")
                                 ;; task :name ... end
                                 (yas/item "CB81DA55-F3BC-4BFB-B0C5-29F0EE6F8081")
                                 ;; desc ... task :name ... end
                                 (yas/item "FE9A8EDA-C243-4068-8F38-A615B82D08C9")
                                 ;; Ignoring Rake/Sake task using file path
                                 (yas/ignore-item "E07FF68B-C87D-4332-8477-D026929FDADA")))
                   (yas/separator)
                   ;; Ignoring Open Require
                   (yas/ignore-item "8646378E-91F5-4771-AC7C-43FC49A93576")
                   ;; Ignoring Validate Syntax
                   (yas/ignore-item "EE5F19BA-6C02-11D9-92BA-0011242E4184")
                   
                   ;; Ignoring Execute Line / Selection as Ruby
                   (yas/ignore-item "EE5F1FB2-6C02-11D9-92BA-0011242E4184")
                   ;; Execute and Update ‘# =>’ Markers
                   (yas/item "FBFC214F-B019-4967-95D2-028F374A3221")
                   ;; Add ‘# =>’ Marker
                   (yas/item "88BC3896-DC39-4307-A271-21D33340F15A")
                   (yas/separator)
                   ;; Ignoring Insert Missing Requires
                   (yas/ignore-item "9FB64639-F776-499B-BA6F-BB45F86F80FD")
                   ;; Ignoring Add ! to Method in Line / Selection
                   (yas/ignore-item "7F79BC8D-8A4F-4570-973B-05DFEC25747F")
                   ;; Ignoring Toggle String / Symbol
                   (yas/ignore-item "B297E4B8-A8FF-49CE-B9C4-6D4911724D43")
                   ;; Insert ERb’s <% .. %> or <%= .. %>
                   (yas/item "FDFABCB9-DF58-4469-AE11-5407A4FF4D70")
                   (yas/separator)
                   (yas/submenu "Declarations"
                                (;; begin … rescue … end
                                 (yas/item "0F940CBC-2173-49FF-B6FD-98A62863F8F2")
                                 ;; case … end
                                 (yas/item "667083EE-62C3-11D9-B8CF-000D93589AF6")
                                 ;; when …
                                 (yas/item "48D8E498-C9A5-4B1B-9A18-71A5860276FB")
                                 ;; if … end
                                 (yas/item "6670835F-62C3-11D9-B8CF-000D93589AF6")
                                 ;; if … else … end
                                 (yas/item "667082E6-62C3-11D9-B8CF-000D93589AF6")
                                 ;; elsif ...
                                 (yas/item "CD1609FA-47DA-4EE4-9C5B-5C56D953F5B1")
                                 ;; unless … end
                                 (yas/item "F53E098D-D08E-4CE2-990A-B0BD70E60614")
                                 ;; while ... end
                                 (yas/item "D121FC61-96A4-4B8F-8709-280EDA876FF3")
                                 ;; until ... end
                                 (yas/item "488B387C-50C0-4B2D-9260-5A7E7EAF9B42")
                                 (yas/separator)
                                 (yas/submenu "Classes and Modules"
                                              (;; class .. end
                                               (yas/item "BF487539-8085-4FF4-8601-1AD20FABAEDC")
                                               ;; class .. initialize .. end
                                               (yas/item "83EED068-8C1C-4BAF-9893-902DC00616AB")
                                               ;; class .. < ParentClass .. initialize .. end
                                               (yas/item "0CCBE04E-F4E2-4E55-9506-7DE67ACF8388")
                                               ;; ClassName = Struct .. do .. end
                                               (yas/item "05DFF82C-5A29-4EBD-93FE-C165FFFB5EA8")
                                               ;; class BlankSlate .. initialize .. end
                                               (yas/item "E98FB8F9-7302-431D-8BF2-275A68A6126C")
                                               ;; Ignoring class .. < DelegateClass .. initialize .. end
                                               (yas/ignore-item "121B334B-2AA6-4E9A-A8B8-BF93B627982B")
                                               ;; class .. < DelegateClass .. initialize .. end
                                               (yas/item "AFE1D078-EA16-45F5-AD8A-FAC1B523D861")
                                               ;; class << self .. end
                                               (yas/item "C7AAAE45-487A-4B61-8962-D47675AAC05F")
                                               (yas/separator)
                                               ;; module .. end
                                               (yas/item "2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC")
                                               ;; module .. module_function .. end
                                               (yas/item "0E85EC81-2FAB-4648-B590-119CC1BB6E41")
                                               ;; module .. ClassMethods .. end
                                               (yas/item "A71A18CF-2D71-4BFF-AA0C-D9B8C59BC4EB")))
                                 (yas/submenu "Methods"
                                              (;; Ignoring New Method
                                               (yas/ignore-item "0275EF39-9357-408F-AF20-79E415CA9504")
                                               
                                               ;; attr_reader ..
                                               (yas/item "A150C2D8-25B3-4339-BC92-8A0160A70486")
                                               ;; attr_writer ..
                                               (yas/item "3D383096-A03F-4EF8-9060-3C727045AB34")
                                               ;; attr_accessor ..
                                               (yas/item "D7A7D3C9-1714-4C50-8CC0-D83A03883E8F")
                                               (yas/separator)
                                               ;; include Enumerable ..
                                               (yas/item "AAD5D511-6BE7-41DA-8F2B-1593A48FBB08")
                                               ;; include Comparable ..
                                               (yas/item "6C9D6B3D-D8E9-4606-9534-577C8D21FFF6")
                                               (yas/separator)
                                               ;; Ignoring extend Forwardable
                                               (yas/ignore-item "58FDEA60-10AF-4C49-AA09-29B77030DB25")
                                               ;; extend Forwardable
                                               (yas/item "7F46C90A-595B-4B83-A4F7-058F63CE4218")
                                               (yas/separator)
                                               ;; def … end
                                               (yas/item "4E9A7A73-875C-11D9-897C-000393CBCE2E")
                                               ;; def self .. end
                                               (yas/item "7C6E88FA-CA0E-4110-8C75-A94E54286A75")
                                               ;; def method_missing .. end
                                               (yas/item "87D5F8AD-8DA6-4AED-A0D8-B51CAC980445")
                                               ;; def_delegator ..
                                               (yas/item "C44ED391-614F-4BA2-BB0F-87668EEA9954")
                                               ;; def_delegators ..
                                               (yas/item "4A6EFD6B-88E2-4822-AD48-03460EDBC796")
                                               (yas/separator)
                                               ;; alias_method ..
                                               (yas/item "988C8AEF-FC71-4455-9C4F-9338C05685A4")))
                                 ;; __END__
                                 (yas/item "451A0596-1F72-4AFB-AF2F-45900FABB0F7")
                                 (yas/separator)
                                 ;; #!/usr/bin/env ruby -wKU
                                 (yas/item "A05CBDD6-845D-45EB-94FB-F8787F5456BE")
                                 ;; require ".."
                                 (yas/item "97DE939B-D243-4D5C-B953-1C9090912E7C")
                                 ;; application { .. }
                                 (yas/item "E16D24D2-CC7E-4786-BE0B-1725FC865D78")
                                 ;; usage_if()
                                 (yas/item "21C0D711-F32A-4665-AA0D-B136F9DD3945")
                                 ;; usage_unless()
                                 (yas/item "49D69DEC-6991-49F4-8D9B-BA60BFDD3D17")))
                   (yas/submenu "Iterators"
                                ((yas/submenu "Arrays"
                                              (;; Array.new(10) { |i| .. }
                                               (yas/item "DAE6A754-D906-4763-B816-CE67125CEF08")
                                               (yas/separator)
                                               ;; delete_if { |e| .. }
                                               (yas/item "263C94DC-63CF-4BA3-9692-C5582CA8F1AB")
                                               ;; fill(range) { |i| .. }
                                               (yas/item "6021BBDC-4AAD-447B-A0C2-A4BB31721558")
                                               ;; flatten_once()
                                               (yas/item "3DDB99C4-486D-4C11-A217-5680FDD8EC19")
                                               ;; zip(enums) { |row| .. }
                                               (yas/item "FD010022-E0E7-44DB-827F-33F7D9310DA2")))
                                 (yas/submenu "Counting"
                                              (;; downto(0) { |n| .. }
                                               (yas/item "4991BB86-736E-4758-B9B2-E4FA90B9368F")
                                               ;; step(2) { |e| .. }
                                               (yas/item "36853A11-0307-4AE7-B835-7CE6358717A5")
                                               ;; times { |n| .. }
                                               (yas/item "206D54AF-E67A-4DF0-B7F4-3D42FEB81685")
                                               ;; upto(1.0/0.0) { |n| .. }
                                               (yas/item "51954118-81D7-42B6-9A10-BE23D8B9FFE2")
                                               (yas/separator)
                                               ;; loop { .. }
                                               (yas/item "567E3D18-BF2B-4379-8927-2777EC9F495E")))
                                 (yas/submenu "Each Element"
                                              (;; each { |e| .. }
                                               (yas/item "ECBA4CA0-275F-460E-85BE-E82FEA2E2B26")
                                               ;; each_byte { |byte| .. }
                                               (yas/item "338EC03D-3FF4-4435-94E8-1CEF20CEC75D")
                                               ;; each_char { |chr| .. }
                                               (yas/item "7E084412-80E6-4B70-8092-C03D1ECE4CD2")
                                               ;; each_char { |chr| .. }
                                               (yas/item "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
                                               ;; Ignoring each_cons(..) { |group| .. }
                                               (yas/ignore-item "EC73D5CC-5F05-46B9-A6F4-82037E4A38C9")
                                               ;; each_cons(..) { |group| .. }
                                               (yas/item "3C04589C-5127-478E-97B3-CA7DD2EA7ECD")
                                               ;; each_index { |i| .. }
                                               (yas/item "689120C9-AB40-4081-8268-9362E00FA4A0")
                                               ;; each_key { |key| .. }
                                               (yas/item "E54F7077-3C33-4B53-A4F7-21E16132D3AD")
                                               ;; each_line { |line| .. }
                                               (yas/item "02913388-EE8E-4C55-AC94-94F3D751F47E")
                                               ;; each_pair { |name, val| .. }
                                               (yas/item "7A3CECED-452B-438E-A5C6-95B6BDC43243")
                                               ;; Ignoring each_slice(..) { |group| .. }
                                               (yas/ignore-item "825B721D-4367-4DF7-98C0-F005695DF9E3")
                                               ;; each_slice(..) { |group| .. }
                                               (yas/item "CD748479-D2A4-4AB5-95BD-4C89512BA210")
                                               ;; each_value { |val| .. }
                                               (yas/item "844DBD70-BC23-4FBF-9C18-F4A610239DF2")
                                               ;; each_with_index { |e, i| .. }
                                               (yas/item "1DD13CF5-39C0-4F10-B655-56DACEBC7F94")
                                               ;; reverse_each { |e| .. }
                                               (yas/item "F3C5F719-EF03-4FF7-A777-4A8402FE3B6B")
                                               (yas/separator)
                                               ;; inject(init) { |mem, var| .. }
                                               (yas/item "B563E0D7-513D-49B4-9733-1B04A6F25A74")
                                               (yas/separator)
                                               ;; map { |e| .. }
                                               (yas/item "5A3754FC-43A3-462B-AB42-E3E951872E6F")
                                               ;; Ignoring map_with_index { |e, i| .. }
                                               (yas/ignore-item "BFB65D1C-62F1-485D-8A67-3E5A2E55107C")
                                               ;; map_with_index { |e, i| .. }
                                               (yas/item "BD4CFD7B-1AC0-4569-9BDA-FD491F41F4E6")))
                                 (yas/submenu "Files"
                                              (;; Dir.glob("..") { |file| .. }
                                               (yas/item "332AA973-AA71-48CB-AEE9-1D71E11019AC")
                                               ;; File.foreach ("..") { |line| .. }
                                               (yas/item "8F594E5E-6F46-4E98-B5FB-1C8F3BA9828F")
                                               ;; open("path/or/url", "w") { |io| .. }
                                               (yas/item "418F1817-255F-430A-B09A-222964ED66A7")
                                               ;; unix_filter { .. }
                                               (yas/item "8CEF9711-88D5-4202-AFB9-29EF4EFD25C1")
                                               (yas/separator)
                                               ;; option_parse { .. }
                                               (yas/item "C3C48948-4F49-484E-A8DE-DEB44723099E")
                                               ;; option(..)
                                               (yas/item "209D5D73-7A77-4931-A158-3FB6D5B48A88")))
                                 (yas/submenu "Ordering"
                                              (;; sort { |a, b| .. }
                                               (yas/item "9E0B4D4B-2956-4B3A-800A-3D8CE54E66BF")
                                               ;; sort_by { |e| .. }
                                               (yas/item "BA9440C9-36C3-4031-BB61-67B581D5B179")
                                               (yas/separator)
                                               ;; randomize()
                                               (yas/item "B0CE57EC-FB2E-4482-8CCE-448DC2588715")))
                                 (yas/submenu "Searching and Selection"
                                              (;; all? { |e| .. }
                                               (yas/item "07D1F987-7CDB-4EAD-B64A-27A93051700E")
                                               ;; any? { |e| .. }
                                               (yas/item "A3B9B76B-2BC5-425C-AB24-9FAAFC375798")
                                               ;; classify { |e| .. }
                                               (yas/item "5DA9E1E8-2C54-420A-9B84-B040A1AF2B9E")
                                               ;; collect { |e| .. }
                                               (yas/item "669A86AD-936F-4EDA-8E4E-6863804072DA")
                                               ;; detect { |e| .. }
                                               (yas/item "6C6B9849-9631-49FF-A9F9-F0E94A1512C5")
                                               ;; fetch(name) { |key| .. }
                                               (yas/item "1F72122A-35AD-4BA1-AA01-889A10319666")
                                               ;; find { |e| .. }
                                               (yas/item "E23FE534-8061-4828-98A5-46270B6910B0")
                                               ;; find_all { |e| .. }
                                               (yas/item "197709C5-8382-4A59-B6D7-31A0CC0F23B7")
                                               ;; grep(/pattern/) { |match| .. }
                                               (yas/item "9D9E7BA3-8C5D-4532-83EA-326358C2F5BB")
                                               ;; max { |a, b| .. }
                                               (yas/item "98182B9E-7C61-4824-BE4C-9CD69C816037")
                                               ;; min { |a, b| .. }
                                               (yas/item "CB03D11A-7204-48D0-92C1-E109034403E7")
                                               ;; partition { |e| .. }
                                               (yas/item "52B8BF63-F09E-4789-8407-06168A8AE666")
                                               ;; reject { |e| .. }
                                               (yas/item "B79B9DAB-ABEF-44F6-BF7E-635E7BA11DFD")
                                               ;; select { |e| .. }
                                               (yas/item "4E409AA4-E7D4-46B7-A4E9-E32F992B33E9")))
                                 (yas/submenu "Strings"
                                              (;; sub(/../) { |match| .. }
                                               (yas/item "8021944C-CEA4-4983-8D1C-78D18D4004A1")
                                               ;; gsub(/../) { |match| .. }
                                               (yas/item "2514FC26-468C-4D08-A788-494A444C4286")
                                               (yas/separator)
                                               ;; scan(/../) { |match| .. }
                                               (yas/item "66802933-B49F-479B-9DF9-1D898FF1FA90")))))
                   (yas/submenu "Blocks"
                                (;; Toggle ‘do … end’ / ‘{ … }’
                                 (yas/item "7990EE60-C850-4779-A8C0-7FD2C853B99B")
                                 (yas/separator)
                                 ;; Insert { |variable| … }
                                 (yas/item "855FC4EF-7B1E-48EE-AD4E-5ECB8ED79D1C")
                                 ;; Insert do |variable| … end
                                 (yas/item "4B72C5C3-6CA7-41AC-B2F9-51DEA25D469E")
                                 (yas/separator)
                                 ;; lambda { |args| .. }
                                 (yas/item "21E75321-0CF7-45E8-A297-BCC7C0DDDD15")))
                   (yas/submenu "Hashes"
                                (;; Hash.new { |hash, key| hash[key] = .. }
                                 (yas/item "E16EE658-1CA0-4950-954B-B962E50B754F")
                                 (yas/separator)
                                 ;; Hash Pair — :key => "value"
                                 (yas/item "840B9C4C-7037-4C3B-9028-EB9DC75EDB3E")
                                 ;; Hash Pointer — =>
                                 (yas/item "B9E3A6DF-875D-11D9-897C-000393CBCE2E")))
                   (yas/submenu "Tests"
                                (;; class .. < Test::Unit::TestCase .. end
                                 (yas/item "31D1F145-33AB-4441-BA11-4D1C46928C4C")
                                 ;; def test_ .. end
                                 (yas/item "00F66D41-25AF-4597-B67D-E540965A5222")
                                 ;; require "tc_.." ..
                                 (yas/item "5297FD0C-98B1-4514-BBD1-1516810BECA6")
                                 (yas/separator)
                                 ;; assert(..)
                                 (yas/item "B32C147D-44A6-478A-9D5D-189D7831E9A7")
                                 ;; assert_equal(..)
                                 (yas/item "43A61A22-6BEE-4997-961C-1CDE739C05FE")
                                 ;; assert_not_equal(..)
                                 (yas/item "A243E96F-DC21-4AA0-B340-13A7674F6AFF")
                                 ;; assert_in_delta(..)
                                 (yas/item "429D0EF5-580D-4166-8F79-713DE96B77F1")
                                 ;; assert_instance_of(..)
                                 (yas/item "0E831E03-67E1-4357-8323-C60685C23C4F")
                                 ;; assert_kind_of(..)
                                 (yas/item "671F05E2-D9CC-485E-BB1B-B13EF20FAC65")
                                 ;; assert_nil(..)
                                 (yas/item "4C79256C-480A-459C-BDE8-BB0D972811DB")
                                 ;; assert_not_nil(..)
                                 (yas/item "79FEC3CC-2A40-4611-9A85-ECDB22FE0701")
                                 ;; assert_match(..)
                                 (yas/item "711ED6C3-0F18-41FB-9A7D-3094BB319A85")
                                 ;; assert_no_match(..)
                                 (yas/item "A072BB1E-1DD1-45D3-9346-8CA3BA21B364")
                                 ;; assert_operator(..)
                                 (yas/item "1B925A4D-8EE4-442B-9254-293599F5717F")
                                 ;; assert_raise(..) { .. }
                                 (yas/item "68B21F6F-5D89-41FA-A19C-F29C2F912B4E")
                                 ;; assert_nothing_raised(..) { .. }
                                 (yas/item "82F8EEE0-2452-411E-8102-7BFDDBCA2E72")
                                 ;; assert_respond_to(..)
                                 (yas/item "09A11FDA-49FC-4466-8787-8D1D5D111A89")
                                 ;; assert_same(..)
                                 (yas/item "29340695-E426-4F77-8CF7-C59360A549F4")
                                 ;; assert_not_same(..)
                                 (yas/item "F91C25EC-EC76-498B-BFB5-FDA8F57C5875")
                                 ;; assert_send(..)
                                 (yas/item "7850AD5C-A90D-4E2C-A931-EADFF8D3D9A3")
                                 ;; assert_throws(..) { .. }
                                 (yas/item "05655BD8-23C6-445F-BFD1-420BF25C3030")
                                 ;; assert_nothing_thrown { .. }
                                 (yas/item "33639D7A-BD8C-4396-9C44-307B8AC87C9E")
                                 ;; flunk(..)
                                 (yas/item "DB457094-1AC9-4856-AEFC-43A9576B6775")
                                 (yas/separator)
                                 ;; Ignoring Benchmark.bmbm do .. end
                                 (yas/ignore-item "C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3")
                                 ;; Benchmark.bmbm do .. end
                                 (yas/item "942F20E2-C40A-44B8-A3F2-99AAC68CB534")
                                 ;; results.report(..) { .. }
                                 (yas/item "1C60D589-DD46-4109-90CA-6B34AEA2F298")))
                   (yas/submenu "Serialization"
                                (;; Marshal.dump(.., file)
                                 (yas/item "0CB48BCA-3F6E-4AE0-85BC-08A1D2508216")
                                 ;; Marshal.load(obj)
                                 (yas/item "20AAD0BC-075D-4EC0-9057-E3E5E62C4125")
                                 (yas/separator)
                                 ;; Ignoring PStore.new( .. )
                                 (yas/ignore-item "5AE7CFB4-418E-4E00-AD76-06DB755EE876")
                                 ;; PStore.new( .. )
                                 (yas/item "5B46ECFD-23A4-4F0C-9951-F64C19C72C2B")
                                 ;; transaction( .. ) { .. }
                                 (yas/item "46BF99AD-E172-4D49-BCF7-072F4730E1D9")
                                 (yas/separator)
                                 ;; Ignoring YAML.dump(.., file)
                                 (yas/ignore-item "9460392B-C036-4A76-A5AE-1191F10E4B1B")
                                 ;; YAML.dump(.., file)
                                 (yas/item "3BA6762A-BB6B-489E-8006-F30F386AEF48")
                                 ;; Ignoring YAML.load(file)
                                 (yas/ignore-item "2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA")
                                 ;; YAML.load(file)
                                 (yas/item "8343ACF4-EEB7-44B5-B835-94826466D4D5")
                                 (yas/separator)
                                 ;; Ignoring xmlread(..)
                                 (yas/ignore-item "F6BF907E-FDF7-4D9B-9E57-BE159561349D")
                                 ;; xmlread(..)
                                 (yas/item "B904D4AA-D15D-48A4-8EB2-563BAF489332")
                                 ;; xpath(..) { .. }
                                 (yas/item "CC300D44-6C3F-4F6C-A8AB-86F5A2DC57CF")))
                   (yas/submenu "Idioms"
                                (;; class_from_name()
                                 (yas/item "2DBEE50B-3097-4A57-AB48-3586CF392D8B")
                                 ;; deep_copy(..)
                                 (yas/item "0BA2B2F1-E767-4A03-9791-0AC0183251F1")
                                 ;; path_from_here( .. )
                                 (yas/item "A4E89D97-D5ED-48BB-B5FF-1BFB79211FCD")
                                 ;; singleton_class()
                                 (yas/item "B46D35B8-5DEB-4C10-A110-BA1965A2EB9C")
                                 ;; Ignoring word_wrap()
                                 (yas/ignore-item "97054C4D-E4A3-45B1-9C00-B82DBCB30CAD")))
                   (yas/submenu "File"
                                (;; require File.dirname(__FILE__) + "/.."
                                 (yas/item "7C42D878-FD0F-4181-A71A-57A091C0154A")
                                 (yas/separator)
                                 ;; File.dirname(__FILE__)
                                 (yas/item "16920DC1-6FA6-48C8-90C5-C19E2C734303")
                                 (yas/separator)
                                 ;; File.read(filename)
                                 (yas/item "FAFE9F5C-BF9C-4416-8623-2CB8EBC31B3C")
                                 ;; File.open(filename, 'r') { |f| f.read }
                                 (yas/item "005EB926-4BFE-4BFA-93B2-C9030636289C")))
                   ;; class .. < Test::Unit::TestCase with test_helper
                   (yas/item "228CAB3A-E221-4727-B430-31E94F76C9D3"))
                    '("E5158F94-CC52-4424-A495-14EF9272653F"
                       "EEE6D060-C5A0-400D-A2E0-0835013C5365"
                       "76FCF165-54CB-4213-BC55-BD60B9C6A3EC"
                       "6519CB08-8326-4B77-A251-54722FFBFC1F"
                       "835FAAC6-5431-436C-998B-241F7226B99B"
                       "A83F68A9-F751-4BB4-AE16-56812878C16A"
                       "47D203ED-EB9B-4653-A07B-A897800CEB76"
                       "835FAAC6-5431-436C-998B-241F7226B99B"
                       "569C9822-8C41-4907-94C7-1A8A0031B66D"
                       "BF4CA9F1-51CD-48D4-8357-852234F59046"
                       "B3875596-723C-41EE-9E6F-F84930C3B568"
                       "B297E4B8-A8FF-49CE-B9C4-6D4911724D43"
                       "931DD73E-615E-476E-9B0D-8341023AE730"
                       "DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE"
                       "47D203ED-EB9B-4653-A07B-A897800CEB76"
                       "2DDB6FE0-6111-4C40-A149-8E67E76F8272"
                       "35222962-C50D-4D58-A6AE-71E7AD980BE4"
                       "8646378E-91F5-4771-AC7C-43FC49A93576"
                       "E07FF68B-C87D-4332-8477-D026929FDADA"
                       "2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA"
                       "E5158F94-CC52-4424-A495-14EF9272653F"
                       "58FDEA60-10AF-4C49-AA09-29B77030DB25"
                       "825B721D-4367-4DF7-98C0-F005695DF9E3"
                       "121B334B-2AA6-4E9A-A8B8-BF93B627982B"
                       "BFB65D1C-62F1-485D-8A67-3E5A2E55107C"
                       "EC73D5CC-5F05-46B9-A6F4-82037E4A38C9"
                       "9460392B-C036-4A76-A5AE-1191F10E4B1B"
                       "1AD6A138-2E89-4D6A-AB3F-416BF9CE968D"
                       "EE5F1FB2-6C02-11D9-92BA-0011242E4184"
                       "9FB64639-F776-499B-BA6F-BB45F86F80FD"
                       "7F79BC8D-8A4F-4570-973B-05DFEC25747F"
                       "EE5F19BA-6C02-11D9-92BA-0011242E4184"
                       "97054C4D-E4A3-45B1-9C00-B82DBCB30CAD"
                       "76FCF165-54CB-4213-BC55-BD60B9C6A3EC"
                       "0275EF39-9357-408F-AF20-79E415CA9504"
                       "5289EE40-86B8-11D9-A8D4-000A95E13C98"
                       "6519CB08-8326-4B77-A251-54722FFBFC1F"
                       "C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3"
                       "5AE7CFB4-418E-4E00-AD76-06DB755EE876"
                       "A83F68A9-F751-4BB4-AE16-56812878C16A"
                       "F6BF907E-FDF7-4D9B-9E57-BE159561349D"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Macros/xmlread(__).yasnippet
;; F6BF907E-FDF7-4D9B-9E57-BE159561349D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; EE5F19BA-6C02-11D9-92BA-0011242E4184                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Overwrite } in #{ .. }.yasnippet
;; E5158F94-CC52-4424-A495-14EF9272653F                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; 2DDB6FE0-6111-4C40-A149-8E67E76F8272                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/RDoc documentation block.yasnippet
;; `(concat (if (eq 0 current-line) "\n" "") "\n")`                                           =yyas> (yas/unknown)
;; 
;; # as in Snippets/class __ TestUnitTestCase with test_helper.yasnippet
;; (yas/multi-line-unknown 228CAB3A-E221-4727-B430-31E94F76C9D3)                              =yyas> (yas/unknown)
;; 
;; # as in Macros/YAML.dump(.., file) (Yd).yasnippet
;; 9460392B-C036-4A76-A5AE-1191F10E4B1B                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/map_with_index { e, i .. } (mapwi).yasnippet
;; BFB65D1C-62F1-485D-8A67-3E5A2E55107C                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/each_slice(..) { group .. } (eas).yasnippet
;; 825B721D-4367-4DF7-98C0-F005695DF9E3                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run.yasnippet
;; 35222962-C50D-4D58-A6AE-71E7AD980BE4                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in _ (RDoc comments).yasnippet
;; DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; BF4CA9F1-51CD-48D4-8357-852234F59046                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/assert_not_nil(..) (asnn).yasnippet
;; `yas/ruby-snippet-close-paren`                                                             =yyas> (yas/unknown)
;; 
;; # as in Macros/YAML.load(file) (Yl).yasnippet
;; 2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle StringSymbol.yasnippet
;; B297E4B8-A8FF-49CE-B9C4-6D4911724D43                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run Rake Task.yasnippet
;; 569C9822-8C41-4907-94C7-1A8A0031B66D                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Benchmark_bmbm(__) do __ end.yasnippet
;; C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; 6519CB08-8326-4B77-A251-54722FFBFC1F                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/word_wrap() (worw).yasnippet
;; 97054C4D-E4A3-45B1-9C00-B82DBCB30CAD                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/class .. TestUnitTestCase .. end (tc).yasnippet
;; (yas/multi-line-unknown 31D1F145-33AB-4441-BA11-4D1C46928C4C)                              =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; 7F79BC8D-8A4F-4570-973B-05DFEC25747F                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Missing Requires.yasnippet
;; 9FB64639-F776-499B-BA6F-BB45F86F80FD                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/extend Forwardable (Forw).yasnippet
;; 58FDEA60-10AF-4C49-AA09-29B77030DB25                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/RakeSake task using file path.yasnippet
;; E07FF68B-C87D-4332-8477-D026929FDADA                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).yasnippet
;; 47D203ED-EB9B-4653-A07B-A897800CEB76                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/gsub - remove whitespace from front of line.yasnippet
;; B3875596-723C-41EE-9E6F-F84930C3B568                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle ERb Tags.yasnippet
;; 835FAAC6-5431-436C-998B-241F7226B99B                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/assert_not_nil(..) (asnn).yasnippet
;; `yas/ruby-snippet-open-paren`                                                              =yyas> (yas/unknown)
;; 
;; # as in Commands/New Method.yasnippet
;; 0275EF39-9357-408F-AF20-79E415CA9504                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; EE5F1FB2-6C02-11D9-92BA-0011242E4184                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/class .. DelegateClass .. initialize .. end (class).yasnippet
;; 121B334B-2AA6-4E9A-A8B8-BF93B627982B                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/PStore_new( __ ).yasnippet
;; 5AE7CFB4-418E-4E00-AD76-06DB755EE876                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Run focused unit test.yasnippet
;; 5289EE40-86B8-11D9-A8D4-000A95E13C98                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show RDoc for this file.yasnippet
;; 1AD6A138-2E89-4D6A-AB3F-416BF9CE968D                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/module .. end.yasnippet
;; `(yas/ruby-infer-class-name)`                                                              =yyas> (yas/unknown)
;; 
;; # as in Commands/Check ERB Syntax.yasnippet
;; 76FCF165-54CB-4213-BC55-BD60B9C6A3EC                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/embed string variable.yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;; A83F68A9-F751-4BB4-AE16-56812878C16A                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/each_cons(..) { group .. } (eac).yasnippet
;; EC73D5CC-5F05-46B9-A6F4-82037E4A38C9                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; 8646378E-91F5-4771-AC7C-43FC49A93576                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Enclose in (RDoc comments).yasnippet
;; 931DD73E-615E-476E-9B0D-8341023AE730                                                       =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Macros/xmlread(__).yasnippet
;;                                                                                            =yyas> (yas/unknown)
;; 
;; # as in Snippets/Insert ERb's __ or = __.yasnippet
;; text.html, source.yaml                                                                     =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/Enclose in + (RDoc comments).yasnippet
;; @k                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Check ERB Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Omit from RDoc.yasnippet
;; ^@O                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Open Require.yasnippet
;; @D                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Overwrite } in #{ .. }.yasnippet
;; }                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Commands/Completion Ruby (rcodetools).yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for ruby-mode ends here
(yas/global-mode 1)
)

(yas/initialize-bundle)
;;;###autoload(require 'yasnippet-textmate-bundle)
(set-default 'yas/dont-activate
	     #'(lambda nil
		 (and
		  (or yas/snippet-dirs
		      (featurep 'yasnippet-textmate-bundle))
		  (null
		   (yas/get-snippet-tables)))))
(provide 'yasnippet-textmate-bundle)
;;; yasnippet-textmate-bundle.el ends here
