;;; objed.el --- Navigate and edit text objects. -*- lexical-binding: t -*-
;; Copyright (C) 2018  Clemens Radermacher

;; Author: Clemens Radermacher <clemera@posteo.net>
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/clemera/objed

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A global minor-mode to navigate and edit text objects. Objed enables modal
;; editing and composition of commands, too. It combines ideas of other Editors
;; like Vim or Kakoune and tries to align them with regular Emacs conventions.
;;
;; Text objects are textual patterns like a line, a top level definition, a
;; word, a sentence or any other unit of text. When `objed-mode' is enabled,
;; certain editing commands (configurable) will activate `objed' and enable its
;; modal editing features. When active, keys which would usually insert a
;; character are mapped to objed commands. Other keys and commands will continue
;; to work as they normally would and exit this editing state again.
;;
;; By default important self inserting keys like Space or Return are not bound
;; to modal commands and will exit `objed' on insertion. This makes it
;; convenient to move around and continue adding new text.
;;
;; With activation `objed' shows the current object type in the mode-line. The
;; textual content of the object is highlighted visually in the buffer and the
;; cursor color is changed, too. The user can now navigate by units of this
;; object, change the object state or switch to other object types.
;;
;; The object state is either "inner" or "whole" and is indicated in the
;; modeline by (i) or (w) after the object type. With inner state, anything that
;; would be considered delimiters or padding around an object is excluded.
;;
;; The user can apply operations to objects. By marking objects before applying
;; an operation, s?he can even operate on multiple objects at once. This works
;; similar to the way you interact with files in `dired'. When marking an object
;; the point moves on to the next object of this type.
;;
;; The object type used for initialization is determined by the mapping of the
;; entry command (see `objed-cmd-alist'). For example using `beginning-of-defun'
;; will activate `objed' using the `defun' object as initial object type. With
;; command `next-line', `objed' would initialize with the `line' object.
;;
;; Objeds modal state provides basic movement commands which move by line, word
;; or character. Those switch automatically to the corresponding object type,
;; otherwise they work the same as the regular Emacs movement commands. Other
;; commands only activate the part between the initial position and the new
;; position moved to. By repeating commands you can often expand/proceed to
;; other objects. This way you can compose movement and editing operations very
;; efficiently.
;;
;; The expansion commands distinguish between block objects (objects built out
;; of lines of text) and context objects (programming constructs like strings,
;; brackets or textual components like sentences). This way you can quickly
;; expand to the desired objects.
;;
;; For example to move to the end of the paragraph, the user would first move to
;; the end of the line with "e". This would activate the text between the
;; starting position and the end of the line. The user can now continue to the
;; end of the paragraph by by pressing "e" again. Now s?he is able to proceed
;; even further by pressing "e" again OR to continue by adding new text to the
;; end of the paragraph OR to continue by acting on the text moved over, for
;; example killing it by pressing "k".
;;
;; As often with text editing, the explanation sounds more complicated than
;; using it. To get a better impression of the editing workflow with `objed'
;; have look at https://github.com/clemera/objed where you can find some
;; animated demos.
;;
;; To learn more about available features and commands have a look at the
;; descriptions below or the Docstrings and bindings defined in `objed-map'. To
;; define your own operations and text objects see `objed-define-op' and
;; `objed-define-object'.
;;
;; Although some features are still experimental the basic user interface will
;; stay the same. The following gives an overview of available keys and
;; commands:
;;
;; Basic movement commands (switch the object type on movement):
;;
;; [`objed-map']
;; f/b: Move forward/backward one character and activate the char object.
;; s/r: Move forward/backward one word and activate the word object (*).
;; S/R: Move forward/backward one symbol and activate the symbol object.
;;      Skips strings and comments.
;; n/p: Move to the next/previous line and activate the line object.

;; (*): Emacs uses the f/b keys for word AND character movement (depending on
;;      the used modifier). I made the decision to remap M-f, M-b to M-s, M-r in
;;      my personal configuration (using `key-translation-map'). This is by no
;;      means necessary but might be something to think about. Afterwards you
;;      are able to start word movement with M-s/r and continue moving by words
;;      with s/r which is more convenient than switching between M-b/f and s/r.
;;      You can still access M-s/r regular bindings by using Meta-shift (M-S/R).
;;
;; Commands for block objects (objects built out of lines of text):
;;
;; [`objed-map']
;; l  : Activate (line based) object at point and move to its start.
;;      On repeat proceed to beginning of the indentation block, comment block,
;;      paragraph or other block objects.
;; a  : Move to beginning of line and activate the text moved over.
;;      On repeat proceed to beginning of blocks like explained above.
;; e  : Move to end of line and activate the text moved over.
;;      On repeat proceed to end of blocks like explained above.
;;
;; Commands for context objects. Those objects are common programming constructs
;; like defuns, strings, parentheses but also sentences inside comments for
;; example. Any whitespace after point is skipped before determine the context:
;;
;; [`objed-map']
;; t/h: If coming from basic movement the object at point is guessed
;;      based on context. Point is moved  to the beginning/end of object.
;;      This is useful to "jump out" of the current context.
;;      On repeat move to the previous/next instance of current object type
;; o  : Activate the inner part of the object at point and move to the start.
;;      This is useful to act on the content of the string, brackets and so on.
;;      On repeat expand to other objects around current position.
;; u  : Move to end of the inner object at point and activate the text moved over.
;;      On repeat toggle between beginning/end inside the object.
;; q  : Move point to the other side of the current object.
;; i  : Toggle the state of the current object (inner/whole).
;;
;; Commands to switch to other objects (and move point to its start):
;;
;; [`objed-map']
;; c  : Prefix to switch to other objects,
;;      see `objed-object-map' for available objects
;;      and `objed-define-object' to add your own.
;;
;; Some objects are directly accessible without a prefix:
;;
;; .  : Switch To identifier object.
;; _  : Switch to symbol object.
;; %  : Switch to content object (defined by textual content of current object).
;;
;; Commands to edit objects (applying operations to them). When the region is
;; active the operation acts on the current region. To act on multiple objects
;; at once you can mark them first (see the "Misc commands" below):
;;
;; [`objed-map']
;; x  : Prefix to access other operations,
;;      see `objed-op-map' for available operations
;;      and `objed-define-op' to add your own.
;;
;; Some operations are accessible without a prefix:
;;
;; k  : Kill current object(s).
;;      Merges marked objects if they are only separated by whitespace.
;; d  : Delete current object(s).
;;      Merges marked objects if they are only separated by whitespace.
;; w  : Copy current object(s).
;; y  : Yank last killed text at point.
;; TAB: Indent object(s).
;; ;  : Un/comment object(s).
;; :  : Comment and duplicate object.
;; $  : Flyspell region.
;;
;; `      : Prefix to surround object(s) with any pair using `electric' (built-in).
;; "([{   : Surround object with corresponding chars using `electric',
;;
;; Misc commands:
;;
;; [`objed-map']
;; ,    : Pop to last state, which restores the last position and any object data.
;; j    : Choose an instance of current object type with completion,
;;        using the contents of the first line for completion.
;; z    : Choose an instance of current object type on the screen with `avy'.
;; m    : Add/Remove current object to marked objects and move to next.
;; M    : Add/Remove current object to marked objects and move to previous.
;; v    : Scroll up.
;; V/M-v: Scroll down.
;; /    : Undo last edit command.
;; C-SPC: Set mark.
;; C-h  : Get key binding help (uses `which-key' if available).
;; g/C-g: Exit and deactivate `objed'.
;;
;; Dispatch keys (dispatch to any object defined in `objed-object-map'):
;;
;; [You can add your own prefix bindings using `objed-define-dispatch']
;;  *  : Mark all instances of current object inside another object type.
;;  <  : Activate part from point backward until boundary of some object.
;;  >  : Activate part from point forward until boundary of some object.
;;
;;
;; CONTRIBUTE:
;;
;; I'm happy to receive pull requests or ideas to improve this package. Some
;; parts suffer from the bottom up approach of developing it, but this also
;; allowed me to experiment a lot and try ideas while working on them, something
;; that Emacs is especially good at. Most of the features are tested using
;; `emacs-lisp-mode' but hopefully there aren't to many problems using modes for
;; other languages, I tried my best to write text objects in a language agnostic
;; way by using Emacs syntax information. Testing this and writing tests in
;; general would be an important next step.
;;
;; This package would never been possible without the helpful community around
;; Emacs. Thank you all and see you in parendise...Share the software!
;;

;;
;;; Code:


;; * Deps

(require 'cl-lib)
(require 'nadvice)
(require 'face-remap)

(require 'objed-objects)

;; * Objed Mode

(defvar objed-mode-map
  (let ((map (make-sparse-keymap)))
    ;; because objed is a "big" addition to the interface overriding M-SPC might
    ;; be ok
    (define-key map (kbd "M-SPC") 'objed-activate)
    ;;(define-key map (kbd "C-.") 'objed-activate)
    map)
  "Keymap for /function`objed-mode'.")

;;;###autoload
(define-minor-mode objed-mode
  "Enable objeds modal editing features after certain commands.

With a prefix argument ARG, enable Objed mode if ARG is positive,
and disable it otherwise. If called from Lisp, enable the mode if
ARG is omitted or nil.

Objed mode is a global minor mode. When enabled, any command
configured in `objed-cmd-alist' will activate modal navigation
and editing features on text objects. Available commands,
operations and objects can be found in `objed-map',
`objed-op-map' and `objed-object-map'.

To define your own text objects and editing operations see
`objed-define-object' and `objed-define-op'.

Activating this mode loads the optional dependencies `which-key'
and `avy' if they are available. This can be deactivated by
setting the user options `objed-use-which-key-if-available-p' and
`objed-use-avy-if-available-p' before loading."
  :global t
  :require 'objed
  (if objed-mode
      (progn
        (setq objed--which-key-avail-p (when objed-use-which-key-if-available-p
					 (require 'which-key nil t))
	      objed--avy-avail-p (when objed-use-avy-if-available-p
				   (require 'avy nil t)))
        (dolist (cmd2obj objed-cmd-alist)
          (advice-add (car cmd2obj) :after
                      (apply-partially #'objed--activate (car cmd2obj)))
	  (advice-add (car cmd2obj) :before 'objed--save-start-position)))
    (dolist (cmd2obj objed-cmd-alist)
      (advice-remove (car cmd2obj)
                     (apply-partially #'objed--activate (car cmd2obj)))
      (advice-remove (car cmd2obj) 'objed--save-start-position))))

;; * Customization

(defgroup objed nil
  "Navigate and edit text objects."
  :group 'convenience
  :prefix "objed-")

(defgroup objed-faces nil
  "Faces for `objed'"
  :group 'objed
  :group 'faces
  :prefix "objed-")

;; * Faces


(defface objed-mark
  '((t (:inherit region)))
  "Face used for marked objects."
  :group 'objed-faces)

(defface objed-hl
  '((t (:inherit highlight)))
  "Face used for highlighting textual content of current object."
  :group 'objed-faces)

(defface objed-mode-line
  '((t (:inherit objed-hl)))
  "Face used for the mode line hint."
  :group 'objed-faces)

;; * User Settings and Variables

(defcustom objed-disabled-modes
  '(special-mode dired-mode Info-mode)
  "List of modes for which objed should stay disabled.

If the current `major-mode' is in the list or derives from a
member of it `objed' will not activate.

See also `objed-disabled-p'"
  :group 'objed
  :type '(repeat symbol))


(defcustom objed-cmd-alist
  '(
    (left-char . char)
    (right-char . char)
    (backward-char . char)
    (forward-char . char)
    (forward-word . word)
    (capitalize-word . word)
    (backward-word . word)
    (move-beginning-of-line . line)
    (move-end-of-line . line)
    (previous-line . line)
    (next-line . line)
    (beginning-of-buffer . line)
    (end-of-buffer . char)
    (scroll-up-command . line)
    (scroll-down-command . line)
    (xref-find-definitions . line)
    (xref-pop-marker-stack . line)
    (imenu . line)
    (backward-paragraph . paragraph)
    (forward-paragraph . paragraph)
    (fill-paragraph . textblock)
    (down-list string symbol word)
    (forward-sexp sexp defun bracket string symbol word)
    (backward-sexp sexp defun bracket string symbol word)
    (indent-pp-sexp bracket sexp)
    ;; just use inner line?
    (back-to-indentation section defun bracket string line)
    (org-beginning-of-line . line)
    (org-end-of-line . line)
    (backward-sentence . sentence)
    (org-backward-sentence . sentence)
    (org-backward-element . paragraph)
    (backward-up-list bracket string)
    (up-list bracket string)
    (beginning-of-defun . defun)
    (end-of-defun . defun)
    (outline-previous-visible-heading . section)
    (outline-next-visible-heading . section)
    (org-previous-visible-heading . section)
    (comint-previous-prompt . output)
    (comint-next-prompt . output)
    ;; editing entry commands
    (delete-char . char)
    (kill-line . char)
    (yank . region)
    (yank-pop . region)
    )
  "Entry commands and associated objects.

If the `cdr' of an entry is a list, each of the objects in this
list is tried and the first that matches will be used for
initialization."
  :group 'objed
  :type '(alist :key-type sexp
                :value-type (choice sexp
                                    (repeat sexp))))


(defcustom objed-states-max 20
  "Maximal number of states to remember.

This option holds the number of times `objed-last' can
be used to restore previous states."
  :group 'objed
  :type 'integer)


(defcustom objed-keeper-commands
  '(save-buffer
    read-only-mode
    undo
    undo-only
    recenter-top-bottom)
  "Regular Emacs commands which should not exit modal edit state.

When regular commands are executed `objed' will exit its editing
state. Commands added to this list wont do that."
  :group 'objed
  :type '(repeat function))

(defcustom objed-cursor-color "#e52b50"
  "Cursor color to use when `objed' is active."
  :group 'objed
  :type 'color)

(defcustom objed-which-key-order #'which-key-description-order
  "Key sort order to use for which key help popups."
  :group 'objed
  :type 'function)

(defcustom objed-modeline-hint-p t
  "Whether to show hint for current object in mode line."
  :group 'objed)

(defcustom objed-mode-line-format
  '(:eval (propertize
	   (format " %s(%s) "
		   (symbol-name objed--object)
		   (char-to-string (aref (symbol-name objed--obj-state) 0)))
	   'face 'objed-mode-line))
  "Format used to display hint in mode-line.

Only relevant when `objed-modeline-hint-p' is non-nil."
  :group 'objed
  :type 'sexp)

(defcustom objed-modeline-setup-func #'objed--setup-mode-line
  "Function to setup the mode line.

This function recieves `objed-mode-line-format' as an argument to
add/remove the mode line hint.

It also recieves a second optional argument which indicates if
the hint should be remove or added. If non-nil the hint should be
removed."
  :group 'objed
  :type 'symbol)

(defcustom objed-initial-object 'region
  "Object to use for inititalization with `objed-activate'."
  :group 'objed
  :type 'symbol)



;; optional dep options

(defcustom objed-use-which-key-if-available-p t
  "Whether to allow loading and use of `which-key'.

To avoid loading `which-key' set this var before activating `objed-mode.'"
  :group 'objed
  :type 'boolean)

(defcustom objed-auto-wk-top-level-p nil
  "Whether to show top level help automatically when activating.

Respects `which-key-idle-delay'.
The top level help is also available via `objed-show-top-level'."
  :group 'objed
  :type 'boolean)

(defcustom objed-use-avy-if-available-p t
  "Whether to allow loading and use of `avy'.

To avoid loading `avy' set this var before activating `objed-mode.'"
  :group 'objed
  :type 'boolean)

(defcustom objed-use-hl-p t
  "Whether allow loading and use of `hl-line' to highlight the current object."
  :group 'objed
  :type 'boolean)


;; info for byte-comp
(defvar ivy-sort-function-alist nil)
(defvar which-key-replacement-alist nil)
(defvar which-key-mode nil)
(defvar which-key-idle-delay 1.0)
(defvar which-key--using-top-level nil)
(defvar avy-all-windows nil)
(defvar avy-action nil)

(declare-function objed--exit-objed "objed" nil t)
(declare-function electric-pair-post-self-insert-function "ext:electric")
(declare-function which-key-description-order "ext:which-key")
(declare-function which-key--create-buffer-and-show "ext:which-key")
(declare-function avy--process "ext:avy")
(declare-function avy--style-fn "ext:avy")
(declare-function edit-indirect-region "ext:edit-indirect")

;; * Helper Macros

(defvar objed--exit-alist nil
  "Maps operations to exit functions.

If this list contains an entry for an operation, the
exit function is called after execution of the operation.")

(defvar objed--after-init-alist
  '((move-end-of-line . objed--object-trailing-line)
    (org-end-of-line . objed--object-trailing-line)
    (backward-sentence . objed--goto-start))
  "Maps commands which need special initialization to init functions.

The init functions are called with the position point was before
the command was executed.")

(defvar objed--opoint nil
  "Position before movement command activated `objed'.")


(defmacro objed-define-op (key cmd &optional exit)
  "Macro to create operations for `objed'.

KEY is a string to be interpreted as spelled-out keystrokes,
using same format as for `kbd'. If KEY is non-nil it is bound to
and operation that will use CMD in `objed-map'.

CMD should be a region command or a non-interactive function.

In the latter case the function recieves two buffer positions as
arguments. If the function is able to recieve three arguments the
third argument will be the prefix argument passed to the
operation.

If EXIT is given it should be a function which is called after
executing the operation. The function recieves one argument which
is the string of the textual content the operation acted on.

This macro returns a command that can be used as an `objed'
operation."
  (let ((name (intern (format "objed-%s" (symbol-name cmd))))
        (res (list 'progn)))
    (when key
      (push `(define-key objed-map (kbd ,key) ',name) res))
    (when exit
      (push `(setq objed--exit-alist
		   (cons (cons ',name #',exit) objed--exit-alist))
	    res))
    (push `(defun ,name (arg)
             "Objed operation."
             (interactive "P")
             (let ((cmd (objed--create-op ',cmd arg)))
               (if objed--marked-ovs
                   (objed--ov-apply ',name cmd objed--marked-ovs)
                 (objed--ob-apply ',name cmd (objed--current)))))
          res)
    (nreverse res)))


(defmacro objed--with-allow-input (&rest body)
  "Allow input in minibuffer while `objed' is active.

The code executed in BODY allows minibuffer input without
interferring with `objed'."
  `(let ((overriding-terminal-local-map nil))
     (set-cursor-color objed--saved-cursor)
     (unwind-protect (progn ,@body)
       ;; body might exit objed...
       (when objed--buffer
         (set-cursor-color objed-cursor-color)))))


(defvar objed--dispatch-alist nil
  "Maps prefix commands to functions.

Don't modify this list manually, use `objed-define-dispatch'.")

(defvar objed--dispatch-key-alist nil
  "Store keys for dispatch commands.")

(defmacro objed-define-dispatch (key def)
  "Define a dispatch binding.

Defines a prefix key KEY. After pressing this prefix key the user
is expected to invoke an object command bound in
variable/`objed-object-map' to choose an object.

KEY is a string to be interpreted as spelled-out keystrokes,
using same format as for `kbd'.

DEF is the symbol for the function to be called with the choosen
object as an argument."
  (declare (indent 2))
  (let ((dp (intern (format "objed-%s-dispatch" (symbol-name def)))))
    `(prog1 ',def
       (push (cons ',dp ',def) objed--dispatch-alist)
       (setf (symbol-function (define-prefix-command ',dp))
	     objed-dispatch-map)
       (define-key objed-map (kbd ,key) ',dp)
       ;; save for possible reinit of objed-map
       (push (cons ,key ',dp) objed--dispatch-key-alist))))

(defmacro objed--save-state (&rest body)
  "Preserve state during execution of BODY."
  `(let ((state (objed--get-current-state)))
     (unwind-protect (progn ,@body )
       (prog1 nil (objed--restore-state state)))))


;; * Keymaps

(defmacro objed--call-and-switch (cmd obj)
  "Create objed command which will invoke CMD and switch to OBJ."
  `(defun ,(intern (format "objed-%s" (symbol-name cmd))) ()
     ,(format "Call `%s' and switch to object %s"
	      (symbol-name cmd) (symbol-name obj))
     (interactive)
     (call-interactively ',cmd)
     (objed--switch-to ',obj)))


(defvar objed-map
  (let ((map (make-sparse-keymap)))
    ;; block unused chars by default
    (dolist (seq (list (number-sequence ?a ?z)
                       (number-sequence ?A ?Z)))
      (dolist (char seq)
        (define-key map (format "%c" char) 'objed-undefined)))
    ;; keep map active for numeric args
    (dolist (n (number-sequence ?0 ?9))
      (define-key map (format "%c" n) 'digit-argument)
      (define-key map (kbd (format "M-%c" n)) 'digit-argument)
      (define-key map (kbd (format "C-%c" n)) 'digit-argument))
    ;; common emacs keys
    (define-key map (kbd "C-g") 'objed-quit)
    (define-key map "g" 'objed-quit)
    (define-key map (kbd "C-h") 'objed-show-top-level)
    ;; TODO: support repeated invokation
    (define-key map (kbd "C-u") 'universal-argument)
    (define-key map (kbd "C-SPC") 'set-mark-command)
    (define-key map (kbd "C-M-w") 'append-next-kill)
    ;; todo: restore object state, too?
    (define-key map (kbd "/") (objed--call-and-switch undo char))
    ;; usual emacs keys which should not trigger reset should be added to
    ;; objed-keeper-commands...


    ;; general movement
    (define-key map "b" (objed--call-and-switch backward-char char))
    (define-key map "f" (objed--call-and-switch forward-char char))

    (define-key map "s" (objed--call-and-switch forward-word word))
    (define-key map "r" (objed--call-and-switch backward-word word))

    (define-key map "S" 'objed-forward-symbol)
    (define-key map "R" 'objed-backward-symbol)


    (define-key map "p" (objed--call-and-switch previous-line line))
    (define-key map "n" (objed--call-and-switch next-line line))


    ;; block expansions
    (define-key map "l" 'objed-expand-block)
    (define-key map "a" 'objed-beg-of-block)
    (define-key map "e" 'objed-end-of-block)

    ;; context expansions
    (define-key map "t" 'objed-current-or-previous-context)
    (define-key map "h" 'objed-current-or-next-context)
    (define-key map "o" 'objed-expand-context)
    (define-key map "u" 'objed-upto-context)

    (define-key map "i" 'objed-toggle-state)
    (define-key map "q" 'objed-toggle-side)

    ;; TODO: birdview mode/scroll mode
    (define-key map "v" 'scroll-up-command)
    (define-key map "\ev" 'scroll-down-command)
    (define-key map "V" 'scroll-down-command)

    ;; marking/unmarking
    (define-key map "m" 'objed-mark)
    ;; jump back and  mark, unmark if necessary
    (define-key map "M" 'objed-toggle-mark-backward)


    ;; basic edit ops
    (define-key map "k" 'objed-kill)
    (define-key map "w" 'objed-copy)
    (define-key map "d" 'objed-delete)
    (define-key map "y" 'objed-yank)
    (define-key map (kbd "TAB")
      (objed-define-op nil objed-indent))


    (define-key map ";"
      (objed-define-op nil objed-comment-or-uncomment-region))
    (define-key map ":"
      (objed-define-op nil objed-comment-duplicate))

    (define-key map "$"
      (objed-define-op nil flyspell-region))

    (dolist (str (split-string  "'\"([{" "" t))
      (define-key map (kbd str)
	(objed-define-op nil objed-electric)))

    ;; quote op
    (define-key map "`"
      (objed-define-op nil objed-electric-pair))
    ;; all the usual quoting signs


    ;; special commands
    (define-key map "," 'objed-last)
    ;; jump to objects with avy
    (define-key map "z" 'objed-ace)
    ;; swiper like object search
    (define-key map "j" 'objed-occur)

    ;; prefix keys
    (define-key map "x" 'objed-op-map)
    (define-key map "c" 'objed-object-map)

    (dolist (k2d objed--dispatch-key-alist)
      (define-key map (kbd (car k2d)) (cdr k2d)))

    ;; direct acc objs
    ;; moved to S/R
    ;; (define-key map "_" 'objed-symbol-object)
    ;; direct object switches
    (define-key map "." 'objed-identifier-object)
    (define-key map "%" 'objed-contents-object)
     ;; not regular objects, selection
    ;; (define-key map (kbd "M-SPC") 'objed-select-object)
    ;; used for direct quoting now...
    ;; (define-key map "{" 'objed-paragraph-object)
    ;; (define-key map "[" 'objed-section-object)
    ;; (define-key map "(" 'objed-textblock-object)

    map)
  "Keymap for commands when `objed' is active.")


(defun objed--define-prefix (key cmd)
  "Create a prefix keymap for `objed-map'.

The keymap will be accessible with KEY and bound to prefix command
CMD.

The keymap is initilized with basic bindings for numeric
arguments and help.

Other single character keys are bound to `objed-undefined'."
  (let ((map (define-prefix-command cmd)))
    ;; init as prefix
    (define-key objed-map (kbd key) map)
    ;; basic bindings
    (dolist (seq (list (number-sequence ?a ?z)
                       (number-sequence ?A ?Z)))
      (dolist (char seq)
        (define-key map (kbd (format "%c" char)) 'objed-undefined)))
    (let (loop)
      (define-key map "-" 'negative-argument)
      ;; Make plain numbers do numeric args.
      (setq loop ?0)
      (while (<= loop ?9)
        (define-key map (char-to-string loop) 'digit-argument)
        (setq loop (1+ loop))))

    (define-key map (kbd "C-h") 'objed-describe-prefix-bindings)
    map))


(defvar objed-op-map
  (let ((map (objed--define-prefix "x" 'objed-op-map)))
    (define-key map "x" 'objed-op-x)

    (define-key map "c"
       ;; upcase, downcase, capitalize, reformat
      (objed-define-op nil objed-case-op))
    (define-key map "u"
      (objed-define-op nil objed-undo-op))
    (define-key map "r"
      (objed-define-op nil objed-replace-op))
    (define-key map "p"
      (objed-define-op nil objed-pipe-region))

    ;; experimental
    (define-key map "e" 'objed-eval)
    ;; uses edit-indirect if av., via prefix
    (define-key map "n" 'objed-narrow)
    map)
  "Map for additional operations called via a prefix from `objed-map'.

To define new operations see `objed-define-op'.")


(defvar objed-object-map
  (let ((map (objed--define-prefix "c" 'objed-object-map)))
    ;; choose via completion
    (define-key map "x" 'objed-object-x)
    (define-key map (kbd "SPC") 'objed-region-object)
    ;; default objects
    (define-key map "c" 'objed-char-object)
    ;; sexp at point
    (define-key map "o" 'objed-sexp-object)

    (define-key map "a" 'objed-sentence-object)
    (define-key map "k" 'objed-textblock-object)
    (define-key map "q" 'objed-paragraph-object)
    (define-key map "i" 'objed-indent-object)
    ;; meg
    (define-key map "m" 'objed-iblock-object)

    (define-key map "b" 'objed-bracket-object)
    (define-key map "d" 'objed-defun-object)
    (define-key map "s" 'objed-string-object)
    (define-key map ";" 'objed-comment-object)

    (define-key map "t" 'objed-tag-object)
    (define-key map "f" 'objed-file-object)

    (define-key map "p" 'objed-section-object)
    (define-key map "l" 'objed-output-object)
    (define-key map "h" 'objed-buffer-object)

    ;;  handled by basic movement..
    ;; (define-key map "w" 'objed-word-object)
    ;; (define-key map "l" 'objed-line-object)
    ;; (define-key map "s" 'objed-symbol-object)

    map)
  "Keymap used for additional text-objects by `objed'.

To define new objects see `objed-define-object'.")

;; * Dispatch definitions

(defvar objed-dispatch-map
  (let ((map (make-composed-keymap nil objed-object-map)))
    (define-key map (kbd "C-h") 'objed-describe-dispatch-bindings)
    map)
  "Keymap for dispatching commands to objects.

Use `objed-define-dispatch' to define a dispatch command.")

(objed-define-dispatch "<" objed--backward-until)
(objed-define-dispatch ">" objed--forward-until)
(objed-define-dispatch "*" objed--mark-all-inside)


(defun objed--backward-until (name)
  "Activate part from point backward until object NAME."
  (let* ((start (point))
	 (o (objed--until name t)))
    (objed--switch-to
     name nil
     (objed-make-object
      :beg  (point)
      :end start
      :ibeg (objed--min o)
      :iend start))))

(defun objed--forward-until (name)
  "Activate part from point forward until object NAME."
  (let* ((start (point))
	 (o (objed--until name)))
    (objed--switch-to
     name nil
     (nreverse
      (objed-make-object
       :ibeg start
       :iend (point)
       :beg start
       :end (objed--max o))))))

(defun objed--mark-all-inside (name)
  "Mark all objects of current type inside object NAME."
  (save-excursion
    (save-restriction
      ;; narrow to object we search for objects in
      (when (objed--save-state
	     (when (objed--switch-to name)
	       (narrow-to-region (objed--min) (objed--max))
	       (goto-char (point-min))))
	;; objed-mark-object
	(let ((n (objed--do-all 'objed--mark-object)))
	  (message "Marked %s %ss in %s." n objed--object name))))))


(defun objed--until (n &optional back)
  "Get object N and move point accordingly.

By default search object in forward direction. If BACK is non-nil
search backwards."
  (let* ((objed--object n)
	 (objed--obj-state 'whole)
	 (o (objed--get back)))
    (prog1 o
      (if (objed--distant-p o)
	  (if back
	      (goto-char (objed--end o))
	    (goto-char (objed--min o)))
	(if back
	    (goto-char (objed--alt-beg o))
	  (goto-char (objed--alt-end o)))))))


(defun objed-undefined ()
  "Provide feedback if an undefined key is pressed."
  (interactive)
  (let ((msg (format "%s is undefined"
                     (key-description (this-single-command-keys)))))
    ;;(if (not objed--which-key-avail-p)
    ;; (objed--maybe-which-key objed-map msg t))))
    (message msg)))

(defvar objed--look-around nil
  "Object data used by toggle ends commands.")

(defun objed--toggle-ends (init)
  "Toggle point between ends of an object.

INIT should be a function which initializes the object. Its start
and end postion will be used for toggle."
  (cond ((eq last-command 'ignore)
	 (let ((pos (objed--other)))
	   (objed--update-current-object
	    (objed--copy-object objed--look-around))
	   (objed--change-to :iend pos :end pos)
	   (goto-char (objed--beg)))
	 ;; set it to same name(ignore by push-state)
	 (setq this-command 'objed-look-around))
	((eq last-command 'objed-look-around)
	 (let ((pos (objed--other)))
	   (objed--update-current-object
	    (objed--copy-object objed--look-around))
	   (objed--change-to :ibeg pos :beg pos)
	   (goto-char (objed--end))
	   (setq this-command 'ignore)))
	(t
	 (when (save-excursion (funcall init))
	   (setq objed--look-around (objed--copy-object))
	   (objed--change-to :ibeg (point) :beg (point))
	   (goto-char (objed--end))
	   ;; dont save state of modified object
	   (setq this-command 'ignore)))))



;; * Init active state

(defvar objed--which-key-avail-p nil
  "Whether `which-key' package is available.")

(defvar objed--avy-avail-p nil
  "Whether `avy' package is available.")

(defvar objed--saved-vars nil
  "Variables to save and restore.")

(defvar objed--hl-line-keep-p nil
  "Whether to keep command `hl-line-mode' active on reset.")

(defvar objed--saved-cursor nil
  "Cursor color before objed got initialized.")

(defvar objed--buffer nil
  "Buffer where objed got initialized.")

(defvar objed--block-p nil
  "Block advices installed by `objed'.")

(defvar objed--hl-cookie nil
  "The remapping cookie for `hl-line' face.")

(defvar objed--wk-replacement-alist
  (list
   ;; commands to ignore
   `((nil . ,(regexp-opt
              (list "objed-describe-prefix-bindings"
                    "objed-describe-dispatch-bindings"
                    "objed-show-top-level"
		    "digit-argument"
                    "universal-argument"
                    "undefined"
                    "negative-argument")))
     . t)
   ;; show only object name for objects
   '((nil . "\\`objed-\\(.*?\\)-.*?object\\'")
     . (nil . "\\1"))
   ;; remove package prefix all others
   '((nil . "\\`\\(objed-\\)+\\(.*\\)")
     . (nil . "\\2")))
  "Rules to add to `which-key-replacement-alist'.

Only relevant when `which-key' is used.")

(defvar objed--ignored-keys nil
  "Keys recently ignored by `which-key' popup.

Useful for keeping the same popup when pressing undefined keys.")

(defvar-local objed-disabled-p nil
  "If non-nil objed will not activate.")

(defun objed--activate (cmd &rest _)
  "Activate `objed' with command CMD.

See `objed-cmd-alist'."
  (when (and objed-mode
	     (not (minibufferp))
	     (not objed--block-p)
	     (eq real-this-command cmd)
	     (not objed-disabled-p)
	     ;; (memq (key-binding "q")
	     ;; 	   '(self-insert-command
	     ;; 	     outshine-self-insert-command
	     ;; 	     org-self-insert-command))
	     (or (memq major-mode '(messages-buffer-mode help-mode))
		 (not (apply 'derived-mode-p objed-disabled-modes))))
      (objed--init cmd)))

(defun objed--save-start-position (&rest _)
  "Save position of point via `objed--opoint'."
  (setq objed--opoint (point)))

(defun objed--goto-start (&optional _)
  "Goto start of current object if there is one."
  (when objed--current-obj
    (goto-char (objed--beg))))

(defun objed--object-trailing-line (pos)
  "Activate trailing part from POS."
  (unless (eq objed--obj-state 'inner)
    (objed--reverse))
  (objed--change-to :beg pos :ibeg pos))

(defun objed--init (&optional sym)
  "Initialize `objed'.

SYM is a symbol (command or object symbol) used to initialize."
  ;; if anything went wrong make sure to start with clean state
  (when objed--buffer
    (objed--reset))
  ;; (unless objed--buffer
  (setq objed--current-obj nil)
  (setq objed--obj-state 'whole)

  (setq objed--buffer (current-buffer))
  (add-hook 'pre-command-hook 'objed--push-state nil t)

  (pcase-dolist
      (`(,var . ,val)
       `((hl-line-range-function . objed-hl-function)
	 (suggest-key-bindings . nil)
	 (which-key-sort-order . ,objed-which-key-order)
	 (which-key-replacement-alist
	  . ,(append objed--wk-replacement-alist
		     which-key-replacement-alist))))
    (push
     (if (local-variable-p var)
	 (cons var (symbol-value var))
       var)
     objed--saved-vars)
    (set (make-local-variable var) val))

  (unless (or (setq objed--hl-line-keep-p
		    (bound-and-true-p hl-line-mode))
	      (not sym)
	      (not objed-use-hl-p))
    (unless (boundp 'hl-line-mode)
      (require 'hl-line))
    (setq objed--hl-cookie
	  (face-remap-add-relative 'hl-line 'objed-hl))
    (hl-line-mode 1))

  ;; init cursor
  (setq objed--saved-cursor
        (face-attribute 'cursor :background))
  (set-cursor-color objed-cursor-color)

  ;; init object
  (if (commandp sym)
      (let* ((objed--block-p t)
	     (initf (cdr (assq sym objed--after-init-alist)))
	     (o (objed--get-object-for-cmd sym)))
	(when o
	  (objed--switch-to o))
	(when initf (funcall initf objed--opoint)))
    (objed--switch-to sym))

  ;; transient map
  (fset #'objed--exit-objed
        (set-transient-map objed-map
			   #'objed--keep-transient-p
			   #'objed--reset))

  (when objed-modeline-hint-p
    (funcall objed-modeline-setup-func objed-mode-line-format))
  ;; show which key after redisplay if active
  (when objed-auto-wk-top-level-p
    (run-at-time 0 nil #'objed-show-top-level)))


(defun objed--setup-mode-line (format &optional reset)
  "Default function to setup the mode line hint.
FORMAT is used for `mode-line-format'.

Adds `objed-mode-line-format' at front. If RESET is non-nil the
mode line hint is removed again."
  (cond (reset
	 (setq mode-line-format
              (delete format mode-line-format)))
	(t
	 (push format mode-line-format))))


(defun objed--keep-transient-p ()
  "Return non-nil if `objed-map' should stay active.

Reinitializes the current object in case the current command is
one of `objed-keeper-commands'."
  (or (commandp (lookup-key objed-map (this-command-keys-vector)))
       (and (memq (key-binding (this-command-keys-vector))
		  objed-keeper-commands)
	    (prog1 #'ignore
	      (add-hook 'post-command-hook 'objed--reinit-object-one-time nil t)))))


(defun objed--reinit-object-one-time ()
  "To be used with `post-command-hook'.

Reinitializes current object and removes itself from the hook."
  (when objed--buffer
    (with-current-buffer objed--buffer
      (remove-hook 'post-command-hook 'objed--reinit-object-one-time t)
      (objed--update-current-object))))


(defun objed-hl-function ()
  "Function used as default for `hl-line-range-function'."
  (when (and objed--buffer objed--current-obj
	     ;; the region is the current text object now
	     (not (region-active-p)))
    (cons (objed--beg) (objed--end))))


;; * Help commands

(defun objed-show-top-level (&optional interactive)
  "Show bindings of `objed-map'.

When `which-key' isn't use only show bindings if INTERACTIVE is
non-nil which is the case when called interactively."
  (interactive "p")
  (unless (objed--maybe-which-key
           objed-map "Op keys"
           interactive)
    (when interactive
      (objed--describe-bindings objed-map))))

(defun objed-describe-dispatch-bindings ()
  "Describe `objed-dispatch-map' bindings."
  (interactive)
  (unless (objed--maybe-which-key
           objed-dispatch-map "Select" t)
    (setq mark-active nil)
    (objed--describe-bindings
     objed-dispatch-map (vector (aref (this-command-keys-vector) 0)))))

(defun objed--describe-bindings (map &optional key)
  "Decsribe bindings of keymap MAP which was activated with KEY."
  (with-temp-buffer
    (let ((key (or key
		   ;; for top level make one up
		   [modal]))
          (imap (make-sparse-keymap)))
      (define-key imap key map)
      (set-transient-map imap)
      (describe-bindings key (current-buffer))))
  (objed--exit-objed))


(defun objed-describe-prefix-bindings ()
  "Show current prefix bindings and exit."
  (interactive)
  (call-interactively 'describe-prefix-bindings)
  (objed--exit-objed))


(defvar objed--avy-err-msg
  "Package `avy' is not available.
Add `avy' to your load path and restart `objed-mode' with a
non-nil value of `objed-use-avy-if-available-p'."
  "Error message to use if avy commands are not ready to run.")


(defun objed--maybe-which-key (map desc &optional nowait iregex)
  "Show which key popup if user configured to use it.

MAP is the keymap which bindings should be displayed and DESC is
a string to show as the description of the popup.

Waits `which-key-idle-delay' before displaying the popup unless
NOWAIT is non-nil. IREGEX is a regular expressions of bindings to
ignore in `which-key' popup. Any binding whose description
matches IREGEX is not displayed."
  (when (and objed--which-key-avail-p
             ;; let the user deactivate later as well...
             objed-use-which-key-if-available-p
             which-key-mode
             (or nowait (sit-for which-key-idle-delay)))
    (prog1 t
      (setq which-key--using-top-level desc)
      (let ((which-key-replacement-alist
             (if iregex
                 (append `(((nil . ,iregex) . t))
                         which-key-replacement-alist)
               which-key-replacement-alist)))
        (which-key--create-buffer-and-show nil map)))))

;; * Basic Movement, Block Objects (textblocks)

(defvar objed--block-objects '(line buffer iblock section paragraph indent textblock)
  "List of objects which are 'line based'.

Objects which are built by lines of text.")

(defun objed--get-block-objects-for-context (&optional ignore)
  "Get list of objects for current context.

If IGNORE is non-nil it should be an object of
`objed--block-objects' which should be ignored."
  (let ((os (cond ((bound-and-true-p outline-minor-mode)
		   objed--block-objects)
		  ((eq major-mode 'org-mode)
		   (let ((os nil))
		     ;; TODO: sort by object size?
		     (dolist (o objed--block-objects (nreverse os))
		       (unless (memq o '(indent textblock iblock))
			 (push o os)))))
		  (t
		   ;; performance is poor on slow
		   ;; machines when searching after
		   ;; every char with objed--get in case
		   ;; there are no outlines/mode not active
		   (remq 'section objed--block-objects)))))
    (remq ignore
	  (if (save-excursion
		(and (or (not (derived-mode-p 'text-mode))
			 (derived-mode-p 'sgml-mode))
		     (progn (unless (eolp) (objed--skip-ws))
			    ;; if line is a comment line search
			    ;; for textblocks (parag. inside comments)
			    (objed--in-comment-p))))
	      (progn (unless (eolp) (objed--skip-ws))
		     (append  os (list 'comment)))
	    os))))

(defun objed--get-blocks (ignore collf &optional istate)
  "Get line based object states.

If IGNORE is non-nil it should be a symbol for a block object
which should be ignored. COLLF is a function which should be used
to collect positions used for sorting the results in ascending
order. ISTATE is the object state to use and defaults to whole."
  (objed--save-state
     (let ((os (objed--get-block-objects-for-context ignore))
	   (states nil)
	   (oos nil)
	   (nos nil))
     (while os
       (when (and (ignore-errors
		      (objed--switch-to (car os) istate))
		  (not (member (objed--current) oos)))
	 (push (objed--current) oos)
	 (push (cons (funcall collf)
		     (objed--get-current-state))
	       states))
       (pop os))
    ;; TODO: use size of object for sorting
    (dolist (ps (sort states (lambda (a b)
			       ;; ensure line comes first
			       (and (not (eq (cadr (cddr a)) 'line))
				    ;; TODO: fix state format
				    (<= (car a) (car b)))))
		nos)
      (push (cdr ps) nos)))))


(let ((blocks nil))
  (defun objed-expand-block ()
  "Jump to beginning of line based objects.

Activates (line based) object at point (and move to its start).
On repeat proceed to beginning of the indentation block,
paragraph and other 'line based' objects.

See also `objed--block-objects'."
  (interactive)
  (let ((init (not (memq last-command
			 (list this-command
			       'objed-toggle-side)))))
    (when init
      (setq blocks (cl-remove
		    nil
		    (objed--get-blocks
		     ;; ignore current or allow toggle between
		     ;; inner/whole here as well?
		     objed--object
		     #'objed--beg)
		    :test (lambda (_ a)
			    (let ((as (objed--beg (car (nthcdr 3 a)))))
			      ;; make sure position is not below start
			      (> as (objed--skip-forward (point) 'ws)))))))
  (when blocks
     (let ((sdiff (abs (- (point) (objed--beg))))
	   (ediff (abs (- (point) (objed--end)))))
       (objed--restore-state (pop blocks))
       (goto-char (cond ((or (> ediff sdiff)
			     init)
			 (objed--beg))
			(t
			 (objed--end)))))))))




(let ((blocks nil))
  (defun objed-beg-of-block ()
  "Jump to beginning of line based objects.

Moves to beginning of line/indentation and activates the text
moved over. On repeat proceed to beginning of the indentation
block, paragraph and other 'line based' objects.

See also `objed--block-objects'."
  (interactive)
  (cond ((eq last-command this-command)
	 (when blocks
	   (let ((end (objed--end)))
	     (objed--restore-state (pop blocks))
	     (objed--change-to :end end :iend end)
	     (goto-char (objed--beg)))))
	(t
	 (objed--switch-to 'line
			   (unless (<= (point)
				       (objed--indentation-position))
			     'inner))
	 (objed--change-to :end (point) :iend (point))
	 (goto-char (objed--beg))
	 ;; get all which make sense from starting point
	 (setq blocks
	       (cl-remove-duplicates
		(objed--get-blocks
		 nil ;;'line after inner...
		 #'objed--beg)
		:test (lambda (a b)
			(let ((as (objed--beg (car (nthcdr 3 a))))
			      (bs (objed--beg (car (nthcdr 3 b)))))
			  (or (eq  as bs)
			      (>= as (point)))))))))))


(let ((blocks nil))
  ;; line end of block
  (defun objed-end-of-block ()
    "Jump to end of line based objects.

Moves to end of line and activates the text moved over. On repeat
proceed to end of the indentation block, paragraph and other
'line based' objects.

See also `objed--block-objects'."
    (interactive)
    (cond ((eq last-command this-command)
	   (when blocks
	     (let ((beg (objed--beg)))
	       (objed--restore-state (pop blocks))
	       (objed--change-to :beg beg :ibeg beg)
	       (goto-char (objed--end)))))
	  (t
	   (objed--switch-to 'line 'inner)
	   (objed--change-to :beg (point) :ibeg (point))
	   (goto-char (objed--end))
	   ;; get all which make sense from starting point
	   (setq blocks
		 (cl-remove-duplicates
		  (nreverse
		   (objed--get-blocks
		    'line
		    #'objed--end
		    ;; better for most cases
		    'inner))
		  :test (lambda (a b)
			  (let ((as (objed--end (car (nthcdr 3 a))))
				(bs (objed--end (car (nthcdr 3 b)))))
			    (or (eq  as bs)
				(<= as (point)))))))))))


(defun objed-forward-symbol ()
  "Move point forward one symbol.

Skips strings and comments."
  (interactive)
  (let ((pos (point)))
    (if (not (eq objed--object 'symbol))
	(progn
	  (objed--switch-to 'symbol)
	  (goto-char (objed--end))
	  ;; at end of word...
	  (unless (or (> (point) pos)
		      (eobp))
	    (objed--next-symbol)
	    (objed--switch-to 'symbol)
	    (goto-char (objed--end))))
    (objed--next-symbol)
    (objed--switch-to 'symbol)
    (goto-char (objed--end)))))


(defun objed-backward-symbol ()
  "Move point backward one symbol.

Skips strings and comments."
  (interactive)
  (let ((pos (point)))
    (if (not (eq objed--object 'symbol))
	(progn
	  (objed--switch-to 'symbol)
	  (goto-char (objed--beg))
	  (unless (< (point) pos)
	    ;; in case we got the fallback
	    (objed--prev-symbol)
	    (objed--prev-symbol)
	    (objed--switch-to 'symbol)
	    (goto-char (objed--beg))))
      (objed--prev-symbol)
      (objed--switch-to 'symbol)
      (goto-char (objed--beg)))))


;; * Context Objects (programming constructs)

(defvar objed--context-objects '(string bracket tag textblock comment)
  "List of objects to be choosen by context.")

(defun objed--basic-p ()
  "Return non-nil if current object is a basic object.

From basic objects `objed' starts expanding to context objects."
  (memq objed--object '(line word char symbol region buffer)))

(defun objed--get-context-state (from)
  "Get state to be used by expand commands.

FROM is a list of objects to use for guessing the object at point."
  (let (o)
    (objed--save-state
     (save-excursion
       (cond ((setq o (objed--at-p
		       (remq objed--object
			     ;; maybe at other at line beginning as well
			     ;; add section back here as well?
			     '(defun))))
	      (objed--switch-to o))
	     ((setq o (objed--at-p (remq objed--object from)))
	      (objed--switch-to o))
	     ((setq o (save-excursion
			(when (<= (point) (objed--indentation-position))
			  (objed--skip-ws))
			(or (objed--at-p (remq objed--object from))
			    ;; never test for struct inside, its to broad
			    (objed--in-p (remq objed--object from)))))
	      (when (<= (point) (objed--indentation-position))
		(objed--skip-ws))
	      (objed--switch-to o))
	     (t
	      (goto-char (objed--beg))
	      (skip-chars-backward " \t")
	      (when (setq o (or (objed--at-p objed--context-objects)
				(objed--in-p objed--context-objects)))
		(let ((c (objed--current)))
		  (objed--switch-to o)
		  (when (equal (objed--current) c)
		    ;; FIX: more general
		    (forward-char -1)
		    (when (setq o (or (objed--at-p objed--context-objects)
				      (objed--in-p objed--context-objects)))
		      (objed--switch-to o)))))))
       (when o (objed--get-current-state))))))


(defun objed-context-object ()
  "Guess and activate object at point.

Tries to guess the object at point and moves to start of it.

Repeated calls will continue guessing objects and try to expand
to an object containing the current one."
  ;; stop with defun for now
  (unless (eq objed--object 'defun)
    (let ((objed--context-objects
	   ;; TODO: docstrings as well
	   (if (or (and (objed--in-comment-p)
			(not (objed--at-comment-p)))
		   (and (derived-mode-p 'text-mode)
			;; sentences in p tags?
			(not (derived-mode-p 'sgml-mode))))
	       (append (list 'sentence) objed--context-objects)
	     objed--context-objects))
	  (s nil))
      ;; TODO: make direction, position dependend and stay at beg/end?
      (if (setq s (objed--get-context-state objed--context-objects))
	(progn (objed--restore-state s)
	       (force-mode-line-update)
	       (goto-char (objed--beg)))
	;; fallback if nothing else found
	 (or (objed--switch-to 'defun 'inner)
	     (objed--switch-to 'line 'inner))))))


(defun objed-current-or-previous-context (&optional arg)
  "Move to end of object at point and activate it.

On repeat move to the previous instance of this object type. With
postitive prefix argument ARG move to the nth previous object."
  (interactive "p")
  (if (objed--basic-p)
      (objed-context-object)
    ;; toggle side if coming from next?
    (objed--goto-previous (or arg 1))))

(defun objed-current-or-next-context (&optional arg)
  "Move to beginning of object at point and activate it.

On repeat move to the next instance of this object type. With
postitive prefix argument ARG move to the nth next object."
  (interactive "p")
  (if (objed--basic-p)
      (progn (objed-context-object)
	     (goto-char (objed--end)))
  (objed--goto-next (or arg 1))))

(defun objed-expand-context ()
  "Expand to objects based on context.

Starts at the inner part of the object point is in or at. Any
whitespace following point is skipped. Point moves to the start."
  (interactive)
  (if (objed--basic-p)
      (progn (objed-context-object)
	     (objed-toggle-state))
    (if (objed--inner-p)
	(let ((curr (objed--current)))
	  (objed-toggle-state)
	  (when (equal curr (objed--current))
	    (objed-context-object)
	    (goto-char (objed--beg))))
    (objed-context-object)
    (goto-char (objed--beg)))))

(defun objed-upto-context ()
  "Toggle between inner sides of object at point.

Moves to end of (inner) object at point and activates the text
moved over. On repeat toggle between beginning/end inside the
object.

Object is choosen based on context."
  (interactive)
  (objed--toggle-ends
   (lambda ()
     (objed-context-object)
     (objed-toggle-state))))


;; * General object commands

(defun objed-object-x ()
  "Choose and switch to object with completion."
  (interactive)
  (let (objects obj)
     (map-keymap (lambda (_ev cmd)
                  (let ((name (symbol-name cmd)))
                    (when (string-match
                           "\\`objed-\\(.*?\\)-object\\'"
                           name)
                      (push (match-string 1 name)
                            objects))))
                objed-object-map)
     (when (setq obj (objed--with-allow-input
                      (completing-read "Objects: " objects)))
       (when (objed--switch-to obj)
	 (goto-char (objed--beg))))))


(defun objed-activate ()
  "Activate objed.

Uses `objed-initial-object' for initialization."
  (interactive)
  (objed--init objed-initial-object))


(defun objed--goto-char (pos)
  "Move to position POS possibly skipping leading whitespace."
  (goto-char
   (if (eq objed--object 'char)
       pos
     (objed--skip-forward pos 'ws))))


(defun objed-toggle-side ()
  "Move to other side of object.

Default to sexp at point."
  (interactive)
  (let ((sdiff (abs (- (point) (objed--beg))))
	(ediff (abs (- (point) (objed--end)))))
    (cond ((> ediff sdiff)
	   (goto-char (objed--end))
	   (objed--skip-ws t))
	  (t
	   (goto-char (objed--beg))
	   (objed--skip-ws)))))

(defun objed-toggle-state ()
  "Toggle state of object."
  (interactive)
  (let ((sdiff (abs (- (point) (objed--beg))))
	(ediff (abs (- (point) (objed--end)))))
    (objed--reverse)
    (goto-char (cond ((> ediff sdiff)
		      (objed--beg))
		     (t
		      (objed--end))))))


(defun objed--indentation-position ()
  "Get buffer position of indentation on current line."
  (save-excursion
    (back-to-indentation)
    (point)))


(defun objed-select-object ()
  "Select current object with active region."
  (interactive)
  (push-mark (objed--end) t t)
  (goto-char (objed--beg)))

(defun objed-contents-object ()
  "Switch to reference of an object.

Reference is how you would refer to an object. As an example for
a defun it would be the function name. This defaults to the
textual content of an object via the content object."
  (interactive)
  (let ((ref (objed--object :ref)))
    (if ref (objed--switch-to ref)
      (setq objed--content-bounds
	    (objed--bounds))
      (objed--switch-to 'content))
    (goto-char (objed--beg))
    (force-mode-line-update)))


(defun objed-ace ()
  "Jump to an object with `avy'."
  (interactive)
  (unless (and objed--avy-avail-p
               objed-use-avy-if-available-p)
    (user-error objed--avy-err-msg))
  (let* ((avy-action #'goto-char)
         (avy-style 'at-full)
         (avy-all-windows t)
         (posns (objed--collect-object-positions
                 (window-start) (window-end))))
    (cond (posns
           (if (> (length posns) 1)
               (avy--process
                posns (avy--style-fn avy-style))
             (goto-char (caar posns)))
           (objed--update-current-object))
          (t
           (message "No objects found.")))))

(defun objed-occur ()
  "Complete initial lines and jump to object."
  (interactive)
  (let* ((lines (objed--collect-object-lines))
	 (ivy-sort-function-alist nil)
	 (completion-table
	  ;; avoid auto sorting of completing read
	  (lambda (string pred action)
	    ;; FIX: its upside down for sections
	    (if (eq action 'metadata)
		'(metadata (display-sort-function . identity)
			   (cycle-sort-function . identity))
	      (complete-with-action action lines string pred))))
	 (res (objed--with-allow-input
	       (cdr (assoc (completing-read "Objects: " completion-table)
			   lines)))))
    (when res
      (goto-char res)
      (objed--update-current-object)
      (goto-char (objed--beg)))))


(defun objed-mark (&optional arg)
  "Mark/unmark objects.

Marks the object at point or unmarks it if it is marked already.
If the object contains marked objects those are unmarked.

Afterwards move to the next object. With positive numeric prefix
argument ARG mark that many objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      (objed--toggle-mark)
      (objed--goto-next))))


(defun objed--merge-marked ()
  "Merge marked objects.

Unmarks all and marks the text between the first and last marked
object instead and moves to its start."
  (interactive)
  (if (cdr objed--marked-ovs)
      (let (first last)
	(setq last (pop objed--marked-ovs))
	(setq objed--marked-ovs (nreverse objed--marked-ovs))
	(setq first (pop objed--marked-ovs))
	(let ((beg (overlay-start first))
	      (end (overlay-end last)))
	  ;; reset marked
	  (delete-overlay first)
	  (delete-overlay last)
	  (dolist (ov objed--marked-ovs)
	    (delete-overlay ov))
	  (setq objed--marked-ovs nil)
	  (objed--change-to :beg beg :end end)
	  (goto-char beg))
	(message "Merged marked objects before execution."))
    (message "At least two marked objects needed.")))


(defun objed-unmark (&optional arg)
  "Unmark objects.

Like `objed-mark' but unmarks. With positive numeric prefix
argument ARG unmark that many objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      (objed--unmark-object)
      (objed--goto-next))))

(defun objed-toggle-mark ()
  "Like `objed-mark', but don't move on to next object."
  (interactive)
  (objed--toggle-mark))


(defun objed-toggle-mark-backward (&optional arg)
  "Move to previous object and toggle mark it.

With positive numeric prefix argument ARG unmark that many
previous objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      ;; enf. top down order
      (objed--toggle-mark nil 'append)
      (objed--goto-previous))))

(defun objed-last ()
  "Resume to last state.

This is for quickly resuming to states on navigation. Note that
the state is only restored correctly if the buffer was not
modified."
  (interactive)
  (objed--restore-state))


;; * Region Commands as Operations

(defvar objed--cmd-cache nil
  "Caching results for `objed--read-cmd'.")

(defun objed--init-cmd-cache (sym)
  "Add SYM to `objed--cmd-cache' if it is a region command."
  (require 'help)
  ;; don't trigger autoloads
  (let ((spec (when (not (and (symbolp sym)
                              (autoloadp (indirect-function sym))))
                (cadr (interactive-form sym)))))
    (when (or (and spec (stringp spec)
                   (string-match "\\`\\*?r" spec))
              (and (commandp sym)
                   (string-match "\\`(\\(start\\|begi?n?\\) end"
                                 (format "%s" (help-function-arglist sym t)))))
      (push sym objed--cmd-cache))))

(defun objed--read-cmd (&optional force)
  "Completing read an operation from available region commands.

Uses `objed--cmd-cache' to save previous results. If FORCE is
non-nil, invalidate the cache.

Returns symbol of choosen region command."
  (objed--with-allow-input
   (when (or force (not objed--cmd-cache))
     (setq objed--cmd-cache nil)
     (mapatoms #'objed--init-cmd-cache))
   (intern (completing-read "Operation: " objed--cmd-cache))))


(defun objed--create-op (f arg)
  "Create an operation.

An operation is a function which acts on an object. It recieves
two arguments beeing the buffer positions of an object. Provided
function F is a command or a function from which to create the
operation function. ARG is the prefix argument to be used by the
operation."
  (if (commandp f)
      (objed--create-op-from-rcmd f arg)
    ;; try to pass arg
    (lambda (beg end)
      (condition-case nil
          (funcall f beg end arg)
        ((wrong-number-of-arguments)
         (when arg (message "Prefix arg ignored, not supported by operation."))
         (funcall f beg end))))))


(defun objed--create-op-from-rcmd (rcmd &optional prefix)
  "Create an operation from a region command.

The returned function (operation) recieves two positions as
arguments and should have the same effect on the text between
those positions as the region command would have had on selected
text between those positions.

Any arguments of region command RCMD will be determined the same
way as if RCMD would be called interactively and thus might query
the user for input at first use.

Because any interactive input is captured before the operation is
used this might not work correctly when interactive input depends on
the positional arguments of the region command.

When PREFIX is given it will be used by RCMD as
`current-prefix-arg'."
  (let ((current-prefix-arg prefix)
        (handler (if (fboundp #'funcall-interactively)
                     #'funcall-interactively #'funcall))
        (oargs (objed--eval-rspec rcmd)))
    (lambda (beg end)
      (save-mark-and-excursion
       ;; in case the region commands expects an active region...
       (goto-char beg)
       (push-mark end t t)
       (objed--with-allow-input
        (apply handler rcmd beg end oargs))))))


(defun objed--eval-rspec (rcmd)
  "Eval the interactive spec of region command RCMD.

Returns the result (skipping the positional arguments)."
  (save-mark-and-excursion
   ;; some commands error when evaling the interactive spec if
   ;; there is no mark
   (unless (marker-position (mark-marker))
     (set-marker (mark-marker) (point)))
   (objed--with-allow-input
    (cddr
     (advice-eval-interactive-spec
      (cadr (interactive-form rcmd)))))))

;; * State Info

(defvar objed--last-states nil
  "Stack of last states.

See `objed--get-current-state' for details.

Each state is a list of the form:

    (`current-buffer', `point', variable/`objed--object',
      `objed--current-obj', position data of the marked objects
      if any")

(defun objed--get-current-state ()
  "Return current state data."
  (list (current-buffer)
	(point) objed--object
	(objed--copy-object) objed--obj-state
	(and objed--marked-ovs
	     (let ((os nil))
	       (dolist (ov objed--marked-ovs (nreverse os))
		 (push (cons (overlay-start ov)
			     (overlay-end ov))
		       os))))))

(defun objed--push-state ()
  "When using objed commands, push state to `objed--last-states'.

To be used in `pre-command-hook'."
  (unless (eq real-this-command 'objed-last)
    (let ((curr (objed--get-current-state)))
      (unless (equal curr (car objed--last-states))
	(push (objed--get-current-state)
	      objed--last-states)))))



(defun objed--restore-state (&optional state)
  "Restore state STATE.

STATE defaults to first of `objed--last-states'. Note that the
state is only restored correctly if the buffer was not modified."
  (if (and (or state (setq state (pop objed--last-states)))
	   (eq (current-buffer) (pop state)))
      (let* ((pos (pop state))
	     (obj (pop state))
	     (range (pop state))
	     (os (pop state))
	     (ovps (pop state)))
        (prog1 (objed--switch-to obj os range)
	  (goto-char pos)
	  ;; FIXME
	  (objed--unmark-all)
	  (when ovps
	    (objed--mark-ovps ovps))))
    (prog1 nil
      (message "No previous state to restore."))))


;; * Operation definitions

(defun objed-op-x (&optional arg)
  "Choose and apply an operation from region commands with completion.

With numeric prerfix ARG of 0 invalidate the used cache. Any
other value of ARG gets passed as prefix argument to the choosen
region command."
  (interactive "P")
  (let* ((rcmd (objed--read-cmd (= 0 (prefix-numeric-value arg))))
         (cmd (objed--create-op
               rcmd
               (and (not (= 0 (prefix-numeric-value arg)))
                    arg))))
    (if objed--marked-ovs
        (objed--ov-apply this-command cmd objed--marked-ovs)
      (objed--ob-apply this-command  cmd (objed--current)))))

(defun objed-kill ()
  "Kill objects."
  (interactive)
  (objed--kill-op this-command #'kill-region t))

(defun objed-delete ()
  "Delete objects."
  (interactive)
  (objed--kill-op this-command #'delete-region))

(defun objed-copy ()
  "Copy objects."
  (interactive)
  (when (and (eq last-command 'kill-region)
	     (not (eq real-last-command 'append-next-kill)))
    (objed--goto-next))
  (objed--kill-op 'ignore #'copy-region-as-kill t)
  ;; append on repeat
  (setq this-command 'kill-region)
  (message "Copied to `kill-ring.'"))


(defvar objed--electric-event nil
  "Saves the event used for `objed-electric'.")

(defun objed-electric-pair (beg end)
  "Wrap region between BEG, END like `electric'."
  (let ((event (or objed--electric-event
		   (setq objed--electric-event (read-event)))))
    (objed-electric beg end event)))


(defun objed-electric (beg end &optional event)
  "Wrap region between BEG and END like `electric'.

EVENT is used for wrapping according to
`electric-pair-post-self-insert-function' and defaults to
`last-command-event'."
  (interactive "r")
  (require 'elec-pair)
  (let ((electric-pair-mode t)
	(last-command-event (or event last-command-event))
        (epos nil)
	;; make sure to go to beginning
	(rbeg (if (> beg end) end beg))
	(rend (if (> beg end) beg end)))
    (save-mark-and-excursion
     ;; skip ws optionally?
     (push-mark (objed--skip-backward rend 'ws) t t)
     (goto-char rbeg)
     (objed--skip-ws)
     (insert last-command-event)
     (setq epos (point))
     (electric-pair-post-self-insert-function))
    ;; leave point like electric would for region
    (goto-char epos)))


(defun objed-yank (arg)
  "Yank and indent.

ARG is passed to `yank'. On repreat `yank-pop'."
  (interactive "*P")
  (let ((start (point)))
    (if (eq last-command 'yank)
	(yank-pop arg)
      (yank arg)
      (objed--switch-to 'region))
    (indent-region start (point))
    (indent-according-to-mode)
    (objed--update-current-object)))

(defun objed-indent (beg end)
  "Indent region between BEG and END.

Moves point over any whitespace afterwards."
  (indent-region beg end)
  (indent-according-to-mode))

(defun objed-narrow (&optional arg)
  "Narrow to object.

With prefix argument ARG call `edit-indirect-region' if
`edit-indirect' is available."
  (interactive "P")
  (if objed--marked-ovs
      (message "Narrowing not possible with multiple objects.")
    (if (and (require 'edit-indirect nil t)
             arg)
        (switch-to-buffer
         (apply #'edit-indirect-region (objed--current)))
      (apply 'narrow-to-region (objed--current))))
  (objed--exit-objed))


(defvar eval-sexp-fu-flash-mode nil)
;; adapted from lispy
(defun objed--eval-func (beg end &optional replace)
  "Evaluate code between BEG and END.

If REPLACE is non-nil replace the region with the result."
  (if (and (= beg (point-min))
           (= end (point-max)))
      (ignore-errors
        (eval-region beg end) t)
    (let ((p (point))
          (eval-sexp-fu-flash-mode nil))
      (goto-char beg)
      (let ((e-sexp (read (current-buffer)))
            (regionp (/= (point) end))
            (str nil))
        (goto-char p)
        (when (and (consp e-sexp)
                   (not regionp))
          ;; treat variable and face definitions special so they can be
          ;; redefined; ignore reader macro if included in region
          (cond ((and (memq (car e-sexp) (list 'defvar 'defcustom 'defvar-local))
                      (consp (cdr e-sexp))
                      (boundp (cadr e-sexp)))
                 (set (cadr e-sexp) (eval (car (cddr e-sexp)))))
                ((eq (car e-sexp) 'defface)
                 (elisp--eval-defun-1 (macroexpand e-sexp)))
                ((memq (car e-sexp) '(\, \,@))
                 (setq e-sexp (cadr e-sexp)))))
        (when (setq str (condition-case e
                            (if regionp
                                (progn (eval-region beg end t)
                                       t)
                              (prin1-to-string (eval e-sexp lexical-binding)))
                          (error (prog1 nil
                                   (message (error-message-string e))))))
          (if (not (stringp str))
              t
            (let ((message-log-max nil))
              (message str))
            (if (not replace)
                str
              (when str
                (delete-region beg end)
                (insert str)))))))))


(defun objed-eval (&optional replace)
  "Eval objects.

If REPLACE is non-nil replace evaluated code with result."
  (interactive "P")
  (if objed--marked-ovs
      (save-excursion
        (dolist (ov (nreverse (copy-sequence objed--marked-ovs)))
          (let ((beg (overlay-start ov))
                (end (overlay-end ov)))
            (delete-overlay ov)
            (when (and beg end)
              (goto-char beg)
              (funcall 'objed--eval-func beg end replace)))))
    (apply 'objed--eval-func
           (append (objed--current) (list replace))))
  (objed--exit-objed))

(defun objed-pipe-region (beg end cmd &optional variant)
  "Pipe region text between BEG and END through a shell CMD.

VARIANT is either the char r or e to either replace the text with
the result or to echo it."
  (interactive (let ((variant (read-char-choice "Replace [r] Echo [e] "
                                                (list ?r ?e)))
                     (cmd (read-shell-command "Shell command: ")))

                 (list (region-beginning) (region-end)
                       cmd
                       variant)))
  (let* ((cbuf (current-buffer))
         (obuf (generate-new-buffer " *objed-pipe-out*"))
         (args (split-string cmd " "))
         (cmd-name (pop args))
         (skip-nline nil)
         (ecode (with-temp-buffer
                  (insert-buffer-substring cbuf beg end)
                  ;; make sure text ends with newline so
                  ;; it get processed by command
                  (unless (memq (char-before)
                                (list 10 13))
                    (setq skip-nline t)
                    (insert "\n"))
                  (apply 'call-process-region (point-min) (point) cmd-name
                         nil (list obuf) nil args)))
         (output (with-current-buffer obuf
                   ;; dont include newline if manually added
                   (when skip-nline
                     (skip-chars-backward "\n"))
                   (buffer-substring (point-min) (point)))))
    (cond ((string= "" output)
           (kill-buffer obuf)
           (message
            "Command had empty ouput. Exit code %s." ecode))
          ((string= output
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
           (kill-buffer obuf)
           (message
            "Command did not change text. Exit code %s" ecode))
          ((not (zerop ecode))
           (pop-to-buffer obuf)
           (message
            "Command exited with exit code %s." ecode))
          (t
           (cl-case variant
             (?r
              (kill-buffer obuf)
              (delete-region beg end)
              (insert output)
              (message "Replaced region with output."))
             (?e
              (pop-to-buffer obuf)))))
    (objed--exit-objed)))

(defun objed--duplicate-1 (f beg end &optional arg)
  "Duplicate and apply F an region between BEG and END.

Apply function F on region before duplicating it. ARG is passed
to F as third argument."
  (let* ((end (set-marker (make-marker) end))
         (reg (buffer-substring beg end)))
    (apply f (list beg end arg))
    (goto-char end)
    (if (or (eobp)
            (= (line-number-at-pos beg)
               (line-number-at-pos end)))
        (newline)
      (skip-chars-forward "\r\n" (1+ (point))))
    (skip-chars-forward " \t")
    (let ((pos (point)))
      (save-excursion
        (insert reg)
        (indent-according-to-mode)
        (save-excursion
          (goto-char (objed--skip-forward pos 'ws))
          (indent-according-to-mode))
        (indent-region pos (point)))
      (goto-char (objed--skip-forward pos 'ws)))))

(defun objed-duplicate-down (beg end &optional arg)
  "Duplicate region between BEG and END below.

If numeric ARG is given duplicate ARG times."
  (objed--duplicate-1 'ignore beg end arg))

(defun objed-comment-duplicate (beg end &optional arg)
  "Comment and duplicate region between BEG and END.

ARG has the same meaning as for `comment-region.'"
  (objed--duplicate-1 #'comment-region beg end arg))

(defun objed-comment-or-uncomment-region (beg end &optional arg)
  "Similar to `comment-or-uncomment-region'.

Comments empty line, too. BEG, END and ARG are passed to
`comment-or-uncomment-region'."
  (interactive "*r\nP")
  (if (and (eq (save-excursion (goto-char beg)
			       (beginning-of-line))
	       (save-excursion (goto-char end)
			       (beginning-of-line)))
	   (save-excursion
	     (beginning-of-line)
	     (looking-at "^ *$")))
      (progn (goto-char (save-excursion (beginning-of-line)
					;; dummy so comment region does its
					;; thing
					(insert "a")
					(prog1 (point-marker)
					  (comment-or-uncomment-region beg end arg))))
	     (delete-char -1))
    (comment-or-uncomment-region beg end arg)))

(defun objed-replace-op (beg end str)
  "Replace region with string read from minibuffer.

Use the region between BEG and END. STR is the string to use for
replacement."
  (interactive
   (let ((beg (region-beginning))
         (end (region-end)))
     (list beg end (read-string "Replace with: "))))
  (delete-region beg end)
  (insert str))


(defun objed-case-op (beg end case)
  "Change case of region.

Use region between BEG and END. CASE is a character which
determines the case operation:

u: upcase
d: downcase
c: capitalize."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-char-choice "Upcase [u], Downcase [d], Capitalize [c]: "
                           (list ?u ?d ?c))))
  (cl-case case
    (?u
     (upcase-region beg end))
    (?d
     (downcase-region beg end))
    (?c
     (capitalize-region beg end))))


(defun objed-undo-op (_beg _end)
  "Undo in current object range."
  (interactive "r")
  ;; region is created by `objed--create-op-from-rcmd'
  (if (fboundp 'undo-tree-undo)
      (undo-tree-undo '(4))
    (undo '(4))))


;; * OP execution


(defun objed--ob-apply (name action range)
  "Apply an operation on a text object.

NAME is the symbol of the operation.

ACTION is a function which recieves the the two buffer
positions of the text object range.

RANGE is a list of the beginning and and position of
the text object to act on."
  (let ((text (apply #'buffer-substring range)))
    (apply action range)
    (objed-exit-op name text)))

(defun objed--ov-apply (name action ovs)
  "Apply and operation to marked objects.

NAME is the symbol of the operation.

ACTION is a function which recieves the two buffer
positions of a marked object range.

OVS is the list of marked objects."
  (save-excursion
    (dolist (ov (nreverse (copy-sequence ovs)))
      (let ((beg (overlay-start ov))
	    (end (overlay-end ov)))
	(when (and beg end)
	  (goto-char beg)
	  (funcall action beg end))
	(delete-overlay ov))))
    (objed-exit-op name))


(defun objed--kill-op (op cmd &optional append)
  "Op execution for `kill', `copy' and `delete' operations.

OP is the internally used name for the operation and CMD the function used
for execution. If APPEND is non-nil append to ‘kill-ring’ when
killing marked objects.

Marked object sequences are merged to built a single text object."
  (cond ((and (cdr objed--marked-ovs)
	      (objed--ov-sequence-p
	       (nreverse (copy-sequence objed--marked-ovs))))
	 ;; seqences are auto merged for convenience
	 ;; this is usually what you want
	 (objed--merge-marked)
	 (objed--ob-apply op cmd (objed--current)))
	(objed--marked-ovs
	 (dolist (ov (nreverse (copy-sequence objed--marked-ovs)))
	   (let ((beg (overlay-start ov))
		 (end (overlay-end ov)))
	     (delete-overlay ov)
	     (when (and beg end)
	       (goto-char beg)
	       (apply cmd (list beg end)))
	     ;; append subsequent kills
	     (when append
	       (setq last-command 'kill-region))))
	   (setq objed--marked-ovs nil)
	   (objed-exit-op op))
	(t

	 ;; no marked objects
	 (objed--ob-apply op cmd (objed--current))
	 ;; for possible repeats like default conf. (kill line...)
	 (unless (eq op 'ignore)
	   (objed--change-to :beg (point)
			     :ibeg (point)))
	 )))

(defun objed--ov-sequence-p (ovs)
  "Return non-nil if OVS build a sequence.

OVS are overlays. If the overlays are only sepearated by
whitespace they build a sequence."
  (let ((posns nil))
    (dolist (ov ovs)
      (goto-char (overlay-end ov))
      (skip-chars-forward " \t\r\n")
      (push (point) posns))
    ;; skip last
    (pop posns)
    ;; get same order
    (setq posns (nreverse posns))
    ;; skip first
    (pop ovs)
    (not
     ;; empty if sequence...
     (cl-remove-if
      (lambda (ov)
	(= (overlay-start ov)
	   (pop posns)))
      ovs))))


;; * Exit active state

(defun objed--line-p (text)
  "Determine how TEXT spans over lines.

Return non-nil if text spans over an entire single line or
multiple ones."
  (with-temp-buffer
    (insert text)
    (not (= -1  (forward-line -1)))))

(defun objed-exit-op (op &optional text)
  "Handle exit of an operation.

OP is the operation (ignored for now). If TEXT is given it
carries the textual content of the object the operation acted
on."
  ;; TODO: improve exit behaviour for default operations
  (let ((exitf (assq op objed--exit-alist)))
    ;; (objed--update-current-object)
    (cond (exitf
	   (funcall (cdr exitf) text))
	  ((eq op 'ignore))
	  (t
	   (if (and text (objed--line-p text))
	       (objed--switch-to 'line)
	     (objed--switch-to 'char)
	     (goto-char (objed--beg)))))))

(defun objed-quit ()
  "Quit and deactivate `objed-map'."
  (interactive)
  (setq mark-active nil)
  (objed--exit-objed))

(defun objed--reset ()
  "Reset variables and state information."
  (when objed--buffer
    (with-current-buffer objed--buffer

      (setq objed--opoint nil)
      (setq objed--electric-event nil)

      (when objed--marked-ovs
        (dolist (ov objed--marked-ovs)
          (delete-overlay ov))
        (setq objed--marked-ovs nil))

      (while objed--saved-vars
        (let ((setting (pop objed--saved-vars)))
          (if (consp setting)
              (set (car setting) (cdr setting))
            (kill-local-variable setting))))

      (when objed--saved-cursor
	(set-cursor-color objed--saved-cursor))

      (when objed--hl-cookie
	(face-remap-remove-relative objed--hl-cookie))

      (when objed-modeline-hint-p
        (funcall objed-modeline-setup-func objed-mode-line-format 'reset))

      (unless objed--hl-line-keep-p
        (hl-line-mode -1))

      (when (> (length objed--last-states) objed-states-max)
        (setq objed--last-states
	      (cl-subseq objed--last-states 0 objed-states-max)))

      (setq objed--block-p nil)
      (remove-hook 'pre-command-hook 'objed--push-state t)
      (setq objed--buffer nil))))



(provide 'objed)
;;; objed.el ends here
