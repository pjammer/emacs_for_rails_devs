;;; ede-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (autoconf-parameters-for-macro) "autoconf-edit"
;;;;;;  "autoconf-edit.el" (19335 11025))
;;; Generated autoloads from autoconf-edit.el

(autoload (quote autoconf-parameters-for-macro) "autoconf-edit" "\
Retrieve the parameters to MACRO.
Returns a list of the arguments passed into MACRO as strings.

\(fn MACRO &optional IGNORE-BOL IGNORE-CASE)" nil nil)

;;;***

;;;### (autoloads (ede-target-parent ede-parent-project ede-load-project-file
;;;;;;  project-make-dist project-compile-target project-compile-project
;;;;;;  project-edit-file-target ede-compile-target ede-remove-file
;;;;;;  global-ede-mode) "ede" "ede.el" (19335 11025))
;;; Generated autoloads from ede.el

(defvar ede-projects nil "\
A list of all active projects currently loaded in Emacs.")

(defvar ede-minor-mode nil "\
Non-nil in EDE controlled buffers.")

(autoload (quote global-ede-mode) "ede" "\
Turn on variable `ede-minor-mode' mode when ARG is positive.
If ARG is negative, disable.  Toggle otherwise.

\(fn ARG)" t nil)

(autoload (quote ede-remove-file) "ede" "\
Remove the current file from targets.
Optional argument FORCE forces the file to be removed without asking.

\(fn &optional FORCE)" t nil)

(autoload (quote ede-compile-target) "ede" "\
Compile the current buffer's associated target.

\(fn)" t nil)

(autoload (quote project-edit-file-target) "ede" "\
Edit the target OT associated w/ this file.

\(fn (OT ede-target))" nil nil)

(autoload (quote project-compile-project) "ede" "\
Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling.

\(fn (OBJ ede-project) &optional COMMAND)" nil nil)

(autoload (quote project-compile-target) "ede" "\
Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target.

\(fn (OBJ ede-target) &optional COMMAND)" nil nil)

(autoload (quote project-make-dist) "ede" "\
Build a distribution for the project based on THIS project.

\(fn (THIS ede-project))" nil nil)

(autoload (quote ede-load-project-file) "ede" "\
Project file independent way to read a project in from DIR.
Optional ROOTRETURN will return the root project for DIR.

\(fn DIR &optional ROOTRETURN)" nil nil)

(autoload (quote ede-parent-project) "ede" "\
Return the project belonging to the parent directory.
nil if there is no previous directory.
Optional argument OBJ is an object to find the parent of.

\(fn &optional OBJ)" nil nil)

(autoload (quote ede-target-parent) "ede" "\
Return the project which is the parent of TARGET.
It is recommended you track the project a different way as this function
could become slow in time.

\(fn TARGET)" nil nil)

;;;***

;;;### (autoloads nil "ede-auto" "ede-auto.el" (19335 11025))
;;; Generated autoloads from ede-auto.el

(eieio-defclass-autoload (quote ede-project-autoload) (quote nil) "ede-auto" "Class representing minimal knowledge set to run preliminary EDE functions.\nWhen more advanced functionality is needed from a project type, that projects\ntype is required and the load function used.")

;;;***

;;;### (autoloads (ede-adebug-project-root ede-adebug-project-parent
;;;;;;  ede-adebug-project ede-documentation-files ede-description
;;;;;;  ede-name) "ede-base" "ede-base.el" (19335 11025))
;;; Generated autoloads from ede-base.el

(eieio-defclass-autoload (quote ede-target) (quote (eieio-speedbar-directory-button)) "ede-base" "A target is a structure that describes a file set that produces something.\nTargets, as with 'Make', is an entity that will manage a file set \nand knows how to compile or otherwise transform those files into some\nother desired outcome.")

(eieio-defclass-autoload (quote ede-project) (quote (ede-project-placeholder)) "ede-base" "Top level EDE project specification.\nAll specific project types must derive from this project.")

(autoload (quote ede-name) "ede-base" "\
Return the name of THIS targt.

\(fn (THIS ede-target))" nil nil)

(autoload (quote ede-description) "ede-base" "\
Return a description suitable for the minibuffer about THIS.

\(fn (THIS ede-project))" nil nil)

(autoload (quote ede-documentation-files) "ede-base" "\
Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list.

\(fn)" nil nil)

(autoload (quote ede-adebug-project) "ede-base" "\
Run adebug against the current ede project.
Display the results as a debug list.

\(fn)" t nil)

(autoload (quote ede-adebug-project-parent) "ede-base" "\
Run adebug against the current ede parent project.
Display the results as a debug list.

\(fn)" t nil)

(autoload (quote ede-adebug-project-root) "ede-base" "\
Run adebug against the current ede parent project.
Display the results as a debug list.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-cpp-root-load ede-cpp-root-project-root ede-cpp-root-project-file-for-dir)
;;;;;;  "ede-cpp-root" "ede-cpp-root.el" (19335 11025))
;;; Generated autoloads from ede-cpp-root.el

(autoload (quote ede-cpp-root-project-file-for-dir) "ede-cpp-root" "\
Return a full file name to the project file stored in DIR.

\(fn &optional DIR)" nil nil)

(autoload (quote ede-cpp-root-project-root) "ede-cpp-root" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload (quote ede-cpp-root-load) "ede-cpp-root" "\
Return a CPP root object if you created one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list (quote ede-project-class-files) (ede-project-autoload "cpp-root" :name "CPP ROOT" :file (quote ede-cpp-root) :proj-file (quote ede-cpp-root-project-file-for-dir) :proj-root (quote ede-cpp-root-project-root) :load-type (quote ede-cpp-root-load) :class-sym (quote ede-cpp-root) :new-p nil) t)

(eieio-defclass-autoload (quote ede-cpp-root-project) (quote (ede-project eieio-instance-tracker)) "ede-cpp-root" "EDE cpp-root project class.\nEach directory needs a a project file to control it.")

;;;***

;;;### (autoloads (ede-project-sort-targets ede-customize-target
;;;;;;  ede-customize-current-target ede-customize-project) "ede-custom"
;;;;;;  "ede-custom.el" (19335 11025))
;;; Generated autoloads from ede-custom.el

(autoload (quote ede-customize-project) "ede-custom" "\
Edit fields of the current project through EIEIO & Custom.
Optional GROUP specifies the subgroup of slots to customize.

\(fn &optional GROUP)" t nil)

(defalias (quote customize-project) (quote ede-customize-project))

(autoload (quote ede-customize-current-target) "ede-custom" "\
Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.
Optional argument GROUP is the slot group to display.

\(fn &optional GROUP)" t nil)

(defalias (quote customize-target) (quote ede-customize-current-target))

(autoload (quote ede-customize-target) "ede-custom" "\
Edit fields of the current target through EIEIO & Custom.
Optional argument OBJ is the target object to customize.
Optional argument GROUP is the slot group to display.

\(fn OBJ GROUP)" nil nil)

(autoload (quote ede-project-sort-targets) "ede-custom" "\
Create a custom-like buffer for sorting targets of current project.

\(fn)" t nil)

;;;***

;;;### (autoloads (ede-emacs-load ede-emacs-project-root) "ede-emacs"
;;;;;;  "ede-emacs.el" (19335 11025))
;;; Generated autoloads from ede-emacs.el

(autoload (quote ede-emacs-project-root) "ede-emacs" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload (quote ede-emacs-load) "ede-emacs" "\
Return an Emacs Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list (quote ede-project-class-files) (ede-project-autoload "emacs" :name "EMACS ROOT" :file (quote ede-emacs) :proj-file "src/emacs.c" :proj-root (quote ede-emacs-project-root) :load-type (quote ede-emacs-load) :class-sym (quote ede-emacs-project) :new-p nil) t)

(eieio-defclass-autoload (quote ede-emacs-project) (quote (ede-project eieio-instance-tracker)) "ede-emacs" "Project Type for the Emacs source code.")

;;;***

;;;### (autoloads (ede-find-file) "ede-files" "ede-files.el" (19335
;;;;;;  11025))
;;; Generated autoloads from ede-files.el

(autoload (quote ede-find-file) "ede-files" "\
Find FILE in project.  FILE can be specified without a directory.
There is no completion at the prompt.  FILE is searched for within
the current EDE project.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (ede-linux-load ede-linux-project-root) "ede-linux"
;;;;;;  "ede-linux.el" (19335 11025))
;;; Generated autoloads from ede-linux.el

(autoload (quote ede-linux-project-root) "ede-linux" "\
Get the root directory for DIR.

\(fn &optional DIR)" nil nil)

(autoload (quote ede-linux-load) "ede-linux" "\
Return an Linux Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project.

\(fn DIR &optional ROOTPROJ)" nil nil)

(add-to-list (quote ede-project-class-files) (ede-project-autoload "linux" :name "LINUX ROOT" :file (quote ede-linux) :proj-file "scripts/ver_linux" :proj-root (quote ede-linux-project-root) :load-type (quote ede-linux-load) :class-sym (quote ede-linux-project) :new-p nil) t)

(eieio-defclass-autoload (quote ede-linux-project) (quote (ede-project eieio-instance-tracker)) "ede-linux" "Project Type for the Linux source code.")

;;;***

;;;### (autoloads (ede-enable-locate-on-project) "ede-locate" "ede-locate.el"
;;;;;;  (19335 11025))
;;; Generated autoloads from ede-locate.el

(autoload (quote ede-enable-locate-on-project) "ede-locate" "\
Enable an EDE locate feature on PROJECT.
Attempt to guess which project locate style to use
based on `ede-locate-setup-options'.

\(fn &optional PROJECT)" t nil)

;;;***

;;;### (autoloads (ede-make-check-version) "ede-make" "ede-make.el"
;;;;;;  (19335 11025))
;;; Generated autoloads from ede-make.el

(autoload (quote ede-make-check-version) "ede-make" "\
Check the version of GNU Make installed.
The check passes if the MAKE version is no high enough, or if it
is not GNU make.
If NOERROR is non-nil, return t for success, nil for failure.
If NOERROR is nil, then throw an error on failure.  Return t otherwise.

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (ede-pmake-varname) "ede-pmake" "ede-pmake.el"
;;;;;;  (19335 11025))
;;; Generated autoloads from ede-pmake.el

(autoload (quote ede-pmake-varname) "ede-pmake" "\
Convert OBJ into a variable name name.
Change .  to _ in the variable name.

\(fn OBJ)" nil nil)

;;;***

;;;### (autoloads nil "ede-proj" "ede-proj.el" (19335 11025))
;;; Generated autoloads from ede-proj.el

(add-to-list (quote auto-mode-alist) (quote ("Project\\.ede$" . emacs-lisp-mode)))

;;;***

;;;### (autoloads (ede-shell-buffer ede-shell-run-something) "ede-shell"
;;;;;;  "ede-shell.el" (19335 11025))
;;; Generated autoloads from ede-shell.el

(autoload (quote ede-shell-run-something) "ede-shell" "\
Create a shell to run stuff for TARGET.
COMMAND is a text string representing the thing to be run.

\(fn (TARGET ede-target) COMMAND)" nil nil)

(autoload (quote ede-shell-buffer) "ede-shell" "\
Get the buffer for running shell commands for TARGET.

\(fn (TARGET ede-target))" nil nil)

;;;***

;;;### (autoloads (ede-simple-load ede-simple-projectfile-for-dir)
;;;;;;  "ede-simple" "ede-simple.el" (19335 11025))
;;; Generated autoloads from ede-simple.el

(add-to-list (quote ede-project-class-files) (ede-project-autoload "simple-overlay" :name "Simple" :file (quote ede-simple) :proj-file (quote ede-simple-projectfile-for-dir) :load-type (quote ede-simple-load) :class-sym (quote ede-simple-project)) t)

(autoload (quote ede-simple-projectfile-for-dir) "ede-simple" "\
Return a full file name to the project file stored in the current directory.
The directory has three parts:
  <STORAGE ROOT>/<PROJ DIR AS FILE>/ProjSimple.ede

\(fn &optional DIR)" nil nil)

(autoload (quote ede-simple-load) "ede-simple" "\
Load a project of type `Simple' for the directory DIR.
Return nil if there isn't one.
ROOTPROJ is nil, since we will only create a single EDE project here.

\(fn DIR &optional ROOTPROJ)" nil nil)

(eieio-defclass-autoload (quote ede-simple-project) (quote (ede-project eieio-persistent)) "ede-simple" "EDE Simple project class.\nEach directory needs a a project file to control it.")

;;;***

;;;### (autoloads (ede-srecode-insert ede-srecode-setup) "ede-srecode"
;;;;;;  "ede-srecode.el" (19335 11025))
;;; Generated autoloads from ede-srecode.el

(autoload (quote ede-srecode-setup) "ede-srecode" "\
Update various paths to get SRecode to identify our macros.

\(fn)" nil nil)

(autoload (quote ede-srecode-insert) "ede-srecode" "\
Insert at the current point TEMPLATE.
TEMPLATE should specify a context by using a string format of:
  context:templatename
Add DICTIONARY-ENTRIES into the dictionary before insertion.
Note: Just like `srecode-insert', but templates found in 'ede app.

\(fn TEMPLATE &rest DICTIONARY-ENTRIES)" nil nil)

;;;***

;;;### (autoloads (ede-update-version) "ede-util" "ede-util.el" (19335
;;;;;;  11025))
;;; Generated autoloads from ede-util.el

(autoload (quote ede-update-version) "ede-util" "\
Update the current projects main version number.
Argument NEWVERSION is the version number to use in the current project.

\(fn NEWVERSION)" t nil)

;;;***

;;;### (autoloads nil nil ("autoconf-compat.el" "ede-dired.el" "ede-load.el"
;;;;;;  "ede-pconf.el" "ede-proj-archive.el" "ede-proj-aux.el" "ede-proj-comp.el"
;;;;;;  "ede-proj-elisp.el" "ede-proj-info.el" "ede-proj-misc.el"
;;;;;;  "ede-proj-obj.el" "ede-proj-prog.el" "ede-proj-scheme.el"
;;;;;;  "ede-proj-shared.el" "ede-proj-skel.el" "ede-source.el" "ede-speedbar.el"
;;;;;;  "ede-system.el" "makefile-edit.el" "project-am.el") (19457
;;;;;;  37438 441779))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ede-loaddefs.el ends here
