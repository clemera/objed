;;; objed-game.el --- Game for learning Objed -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Version: 0.8.1
;; Maintainer: Clemens Radermacher <clemera@posteo.net>
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

;; Tutorial game for objed.  Use the command `objed-game' to start a
;; new game.

;;; Code:

;;;; Requirements:

(require 'objed)
(require 'cl-lib)

;;;; Types:

(cl-defstruct objed-game-level
  "Current level state in the Objed game."
  name next frames completed)

(cl-defstruct objed-game-frame
  "Current frame state in the Objed game."
  name (attempts 0))

;;;; Variables:

(defvar-local objed-game--show-stats nil
  "Whether to show stats for the current frame.")

(defvar-local objed-game--message nil
  "Overlay object displaying objed game message, if any.")

(defvar-local objed-game--win-condition nil
  "Function to check if win state is achieved in the objed game.")

(defvar-local objed-game--entered nil
  "Commands or keys the user has entered so far in this frame.")

(defvar-local objed-game--correct nil
  "Commands or keys needed to complete the current frame.")

(defvar-local objed-game--current-frame nil
  "Current frame being displayed.")

(defvar-local objed-game--current-level nil
  "Current level being played.")

(defvar-local objed-game--continue nil
  "Function to continue to the next frame/level.")

;;;; Header line:

(defun objed-game-header--stats ()
  "Statistics about the current level."
  (if (and objed-game--show-stats objed-game--current-level)
      (concat (mapconcat (lambda (frame)
                           (if frame
                               (if (<= (objed-game-frame-attempts frame) 0)
                                   "."
                                 "x")))
                         (reverse (objed-game-level-completed objed-game--current-level)))
              (if (member (objed-game-frame-name objed-game--current-frame)
                          (objed-game-level-frames objed-game--current-level))
                  (if (= 0 (objed-game-frame-attempts objed-game--current-frame))
                      "o"
                    "x"))
              (make-string (max 0 (1- (length (objed-game-level-frames objed-game--current-level)))) ?-))))

(defun objed-game-header--center ()
  "Centered text for the objed game header."
  (concat (propertize " "
                      'display '(space :align-to (- center 9.5)))
          "Objed Tutorial Game"))

(defun objed-game-header--right ()
  "Right-aligned text for the objed game header."
  (let ((text (or (and objed-game--current-level
                       (documentation (objed-game-level-name objed-game--current-level))))))
    (concat (propertize " "
                        'display `(space :align-to (- right ,(length text))))
            text)))

;;;; Objed game mode:

(defun objed-game-continue ()
  "Continue to the next level."
  (interactive)
  (if objed-game--continue
      (funcall objed-game--continue)))

(defun objed-game--post-command-function ()
  "Function which calls `objed-game--win-condition' in the `post-command' hook."
  (when objed-game--win-condition
    (pcase (funcall objed-game--win-condition)
      ('partial)
      ('t
       (setq objed-game--win-condition nil)
       (objed-game--success))
      ('nil
       (cl-incf (objed-game-frame-attempts objed-game--current-frame))
       (setq objed-game--win-condition nil)
       (objed-game--failure)))))

(defvar-keymap objed-game-mode-map
  :doc "Keymap for the Objed game."
  "C-c q" #'quit-window
  "C-c C-q" #'quit-window
  "C-c C-c" #'objed-game-continue)

(defvar objed-game-mode-syntax-table (make-syntax-table emacs-lisp-mode-syntax-table)
  "Syntax table for the Objed game.")

(define-derived-mode objed-game-mode fundamental-mode "Objed game"
  "Major mode for *Objed game* buffers."
  (objed-local-mode)
  (add-hook 'post-command-hook #'objed-game--post-command-function nil t)
  (setq header-line-format
        '(""
          (:eval (objed-game-header--stats))
          (:eval (objed-game-header--center))
          (:eval (objed-game-header--right)))))

;;;; Macros:

(defmacro objed-game-frame (name &rest init)
  "Create a frame of the objed game."
  (declare (indent 1) (doc-string 2))
  (let ((doc-string (and (stringp (car init)) (pop init)))
        (commands (prog1 (plist-get init :commands) (cl-remf init :commands)))
        (keys (prog1 (plist-get init :keys) (cl-remf init :keys)))
        (read-only (prog1 (plist-get init :read-only) (cl-remf init :read-only)))
        (no-stats (prog1 (plist-get init :no-stats) (cl-remf init :no-stats))))
    `(defun ,name ()
       ,doc-string
       (read-only-mode ,(if read-only 1 -1))
       (unless (and objed-game--current-frame
                    (eq ',name (objed-game-frame-name objed-game--current-frame)))
         (setq objed-game--current-frame (make-objed-game-frame :name ',name)))
       (setq objed-game--show-stats ,(not no-stats))
       (remove-overlays)
       (setq objed-game--message (make-overlay 0 0))
       (let ((inhibit-read-only t))
         (erase-buffer)
         ,@init)
       ,(if (and (eq nil keys) (eq nil commands))
            '(setq objed-game--win-condition nil)
          `(progn
             (setq objed-game--continue nil)
             (setq objed-game--entered nil)
             (setq objed-game--correct ',(or keys commands))
             (setq objed-game--win-condition
                   ',(if keys
                         'objed-game--verify-keys
                       'objed-game--verify-commands)))))))

;;;###autoload
(defmacro objed-game-level (name &rest frames)
  "Create a level for the Objed game."
  (declare (indent 1) (doc-string 2))
  (let ((doc-string (and (stringp (car frames)) (pop frames)))
        (next (prog1 (plist-get frames :next) (cl-remf frames :next)))
        (intro (prog1 (plist-get frames :intro) (cl-remf frames :intro))))
    `(defun ,name ()
       ,doc-string
       (interactive)
       (switch-to-buffer (get-buffer-create "*Objed game*"))
       (objed-game-mode)
       (setq objed-game--current-level
             (make-objed-game-level
              :name ',name
              :next ',next
              :frames (copy-sequence ',frames)))
       ,(if intro
            `(progn
               (setq objed-game--continue 'objed-game--next-frame)
               (,intro))
          '(objed-game--next-frame)))))

;;;; Utility expressions:

(defun objed-game--verify-keys ()
  "Check the keys a user has entered against `objed-game--correct'.

If user's keys match `objed-game--correct', return `t'.
If they partially match, return `partial'
Otherwise, return nil."
  (push (key-description (this-command-keys)) objed-game--entered)
  (let ((typed-keys (seq-subseq (reverse objed-game--entered) 1)))
    (if (equal typed-keys objed-game--correct)
        t
      (if (equal typed-keys (seq-subseq objed-game--correct 0 (length typed-keys)))
          'partial
        nil))))

(defun objed-game--verify-commands ()
  "Check the commands a user has entered against `objed-game--correct'.

If user's commands match `objed-game--corect', return `t'.
If they partially match, return `partial'
Otherwise, return nil."
  (push this-command objed-game--entered)
  ;; Ignore first command and reverse order
  (let ((typed-commands (seq-subseq (reverse objed-game--entered) 1)))
    (catch 'mismatch
      (dotimes (i (length typed-commands))
        (if (listp (nth i objed-game--correct))
            (unless (member (nth i typed-commands)
                            (cdr (nth i objed-game--correct)))
              (throw 'mismatch nil))
          (unless (eq (nth i typed-commands)
                      (nth i objed-game--correct))
            (throw 'mismatch nil))))
      (if (not (= (length typed-commands)
                  (length objed-game--correct)))
          'partial
        t))))

(defun objed-game--success ()
  "Create a success message and allow C-c C-c"
  (move-overlay objed-game--message (point-max) (point-max))
  (overlay-put objed-game--message 'after-string
               (substitute-command-keys
                "\n\n\nGreat job! Press \\[objed-game-continue] to continue."))
  (setq objed-game--continue 'objed-game--next-frame))


(defun objed-game--failure ()
  "Create a failure message and allow C-c C-c"
  (move-overlay objed-game--message (point-max) (point-max))
  (overlay-put objed-game--message 'after-string
               (concat
                (substitute-command-keys
                 "\n\n\nNot quite. Press \\[objed-game-continue] to try again.")
                (if (> (objed-game-frame-attempts objed-game--current-frame) 2)
                    (concat "\n\nHint: "
                            (mapconcat (lambda (c)
                                         (propertize (format "%s" c) 'font-lock-face 'help-key-binding))
                                       objed-game--correct ", ")))))
  (setq objed-game--continue (objed-game-frame-name objed-game--current-frame)))

(defun objed-game--next-frame ()
  "Call the next frame in the current level.

Calls the next level if no more frames exist."
  (let ((current-frame (and objed-game--current-frame
                            (objed-game-frame-name objed-game--current-frame)))
        (level-frames (objed-game-level-frames objed-game--current-level)))
    (when (member current-frame level-frames)
      (setq level-frames (delq current-frame level-frames))
      (setf (objed-game-level-frames objed-game--current-level) level-frames)
      (push objed-game--current-frame (objed-game-level-completed objed-game--current-level)))
    (if level-frames
        (funcall (nth (random (length level-frames)) level-frames))
      (objed-game--level-completed))))

(objed-game-frame objed-game--level-completed
  "Level completed message."
  :read-only t
  (insert "\n\nCongratulations!\n\nYou've completed "
          (documentation (objed-game-level-name objed-game--current-level))
          (substitute-command-keys "\n\nPress \\[objed-game-continue] to "))
  (if-let ((next (objed-game-level-next objed-game--current-level)))
      (progn
        (setq objed-game--continue next)
        (insert "continue to the next level."))
    (setq objed-game--continue 'objed-game)
    (insert "return to the main menu"))
  (beginning-of-buffer))

;;;; Objed game

;;;###autoload
(objed-game-level objed-game
  "Main menu"
  objed-game--menu)

(objed-game-frame objed-game--menu
  "Main menu for the objed game."
  :read-only t
  :no-stats t
  (newline 2)
  (insert "Welcome to the Objed game!")
  (newline 2)
  (insert "Please choose a level:")
  (newline 2)
  (insert-button "Introduction" 'action (lambda (_) (objed-game-intro)))
  (newline 2)
  (insert-button "Level 1: Entering Objed" 'action (lambda (_) (objed-game-level-1)))
  (newline 2)
  (insert-button "Level 2: Exiting Objed" 'action (lambda (_) (objed-game-level-2)))
  (newline 2)
  (insert-button "Level 3: Object state" 'action (lambda (_) (objed-game-level-3)))
  (newline 2)
  (insert-button "Level 4: Switch types" 'action (lambda (_) (objed-game-level-4)))
  (newline 2)
  (insert-button "Level 4a: Switch types - Avy" 'action (lambda (_) (objed-game-level-4a)))
  (newline 2)
  (insert-button "Level 5: Movement" 'action (lambda (_) (objed-game-level-5)))
  (newline 2)
  (insert-button "Level 6: Editing objects" 'action (lambda (_) (objed-game-level-6)))
  (newline 2)
  (insert-button "Combined: All levels" 'action (lambda (_) (objed-game-combined)))
  (beginning-of-buffer))

;;;; Intro

(objed-game-level objed-game-intro
  "Level 0: Introduction"
  :next objed-game-level-1
  :intro objed-game-intro-frame)

(objed-game-frame objed-game-intro-frame
  :read-only t
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                           INTRODUCTION

**********************************************************************

Objed is a global minor-mode to navigate and edit text
objects. Objed enables modal editing and composition of commands,
too. It combines ideas of other Editors like Vim or Kakoune and
tries to align them with regular Emacs conventions.

For more information also see:

- My Blog: https://www.with-emacs.com/categories/objed/
- Project Readme: https://github.com/clemera/objed/blob/master/README.asc
- Project News: https://github.com/clemera/objed/blob/master/News.asc.

Text objects are textual patterns like a line, a top level
definition, a word, a sentence or any other unit of text. When
objed-mode or objed-local-mode is enabled, certain editing
commands (configurable) will activate objed and enable its modal
editing features. When active, keys which would usually insert a
character are mapped to objed commands. Other keys and commands
will continue to work as they normally would and exit this
editing state again.


>> Type \\[objed-activate] in order to activate objed, choosing
   the initial object based on `last-command' and
   `objed-cmd-alist'

>> Type \\[keyboard-quit] to quit objed mode


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue."))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (beginning-of-buffer))

;;;; Level 1:

(objed-game-level objed-game-level-1
  "Level 1: Entering Objed"
  :intro objed-game-level-1-intro
  :next objed-game-level-2
  objed-game-level-1-1
  objed-game-level-1-2
  objed-game-level-1-3
  objed-game-level-1-4
  objed-game-level-1-5
  objed-game-level-1-6
  objed-game-level-1-7)

(objed-game-frame objed-game-level-1-intro
  :read-only t
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                       ENTERING OBJED MODE

**********************************************************************

When you type \\[objed-activate], notice that objed shows the
current object type in the `mode-line'. The textual content of
the object is highlighted visually in the buffer and the cursor
color is changed, too.

\\[objed-activate] is not the only way to enter
`objed-mode'. Most other text movement commands also activate
objed with the relevant object type.

>> Type \\[forward-word] to move the cursor forward a single word
   and activate `objed-mode' with the `word' object type

>> Type \\[next-line] to move the cursor forward a single line
   and activate `objed-mode' with the `line' object type

>> Type \\[beginning-of-buffer] to move the cursor to the
   beginning of the buffer and activate `objed-mode' with the
   `buffer' object type.

>> Type \\[right-char] to move the cursor forward a single
   character and activate `objed-mode' with the `char' object
   type

>> Type \\[forward-sexp] to move the cursor forward to the next
   s-expression and activate `objed-mode' with the `sexp' object
   type

Once in `objed-mode', you can change the object type by using a
standard movement key.


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue."))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (beginning-of-buffer))

(objed-game-frame objed-game-level-1-1
  :read-only t
  :commands (next-line)
  (insert "\n\n               \nSelect this line and enter objed mode.")
  (goto-char 18))

(objed-game-frame objed-game-level-1-2
  :read-only t
  :commands (previous-line)
  (insert "\n\nSelect this line and enter objed mode.\n             "))

(objed-game-frame objed-game-level-1-3
  :read-only t
  :commands (forward-word)
  (insert  "\n\nSelect the word \"cat\" and enter objed mode.")
  (goto-char 18))

(objed-game-frame objed-game-level-1-4
  :read-only t
  :commands (backward-word)
  (insert "\n\nSelect the word \"dog\" and enter objed mode.")
  (goto-char 24))

(objed-game-frame objed-game-level-1-5
  :read-only t
  :commands (right-char)
  (insert "\n\nSelect the character 'a' and enter objed mode.")
  (goto-char 24))

(objed-game-frame objed-game-level-1-6
  :read-only t
  :commands (left-char)
  (insert "\n\nSelect the character 'b' and enter objed mode.")
  (goto-char 26))

(objed-game-frame objed-game-level-1-7
  :read-only t
  :commands (forward-sexp)
  (insert "\n\nSelect the s-expression (emacs-init-time) and enter objed mode.")
  (goto-char 26))

;;;; Level 2:

(objed-game-level objed-game-level-2
  "Level 2: Exiting objed"
  :intro objed-game-level-2-intro
  :next objed-game-level-3
  objed-game-level-2-1
  objed-game-level-2-2
  objed-game-level-2-3
  objed-game-level-2-4
  objed-game-level-2-5)

(objed-game-frame objed-game-level-2-intro
  :read-only t
  (insert (substitute-command-keys (concat "\\<objed-mode-map>
**********************************************************************

                         EXITING OBJED

**********************************************************************

By default important editing keys like Space, DEL or Return are
not bound to modal commands and will execute the regular command
and exit objed. Character movement exits objed, as well. This
makes it convenient to move around and continue by
adding/deleting text.


You can also type " (propertize "g" 'font-lock-face 'help-key-binding) "
or \\[keyboard-quit] to exit `objed-mode'


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue.")))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (beginning-of-buffer))

(objed-game-frame objed-game-level-2-1
  :keys ("SPC")
  (insert "\n\nExit objed mode and add a space between applepear.")
  (goto-char 48)
  (objed-activate 'char))

(objed-game-frame objed-game-level-2-2
  :commands (forward-char)
  :read-only t
  (insert "\n\nMove the cursor one character to the right and exit objed mode.")
  (goto-char 48)
  (objed-activate 'char))

(objed-game-frame objed-game-level-2-3
  :commands (backward-char)
  :read-only t
  (insert "\n\nMove the cursor to the left one character and exit objed mode.")
  (goto-char 48)
  (objed-activate 'char))

(objed-game-frame objed-game-level-2-4
  :keys ("RET")
  (insert "\n\nCreate a new line at the end of this sentence and exit objed mode.\n")
  (goto-char 69)
  (objed-activate 'line))

(objed-game-frame objed-game-level-2-5
  :read-only t
  :commands ((or objed-quit keyboard-quit))
  (insert "\n\nExit objed mode without moving the cursor.\n")
  (goto-char 30)
  (objed-activate 'line))


;;;; Level 3:

(objed-game-level objed-game-level-3
  "Level 3: Object state"
  :intro objed-game-level-3-intro
  :next objed-game-level-4
  objed-game-level-3-1
  objed-game-level-3-2
  objed-game-level-3-3
  objed-game-level-3-4
  objed-game-level-3-5
  objed-game-level-3-6
  objed-game-level-3-7)

(objed-game-frame objed-game-level-3-intro
  :read-only t
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                           OBJECT STATE

**********************************************************************

In `objed-mode', the object state is either inner or whole and is
indicated in the modeline by (i) or (w) after the object
type. With inner state, anything that would be considered
delimiters or padding around an object is excluded.

While in `objed-mode', the following commands can be used to
modify the state of a selected object:

- \\<objed-map>\\[objed-expand-context] `objed-expand-context'

  Activate the inner part of the object at point and move to the
  start. This is useful to act on the content of the string,
  brackets and so on. On repeat expand to other objects around
  current position

- \\[objed-toggle-state] `objed-toggle-state'

  Toggle object state. Switches between inner and whole object
  state

- \\[objed-toggle-side] `objed-toggle-side'

  Move point to the other side of the current object


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue."))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (beginning-of-buffer))


(objed-game-frame objed-game-level-3-1
  :read-only t
  :commands (forward-sexp objed-toggle-state)
  (insert "\n\nSelect the inner part of the expression (emacs-init-time)")
  (goto-char 42)
  (objed-activate 'sexp))

(objed-game-frame objed-game-level-3-2
  :read-only t
  :commands (forward-word objed-toggle-side)
  (insert "\n\nSelect the word \"hippo\" then move the cursor to the other side.")
  (goto-char 18))

(objed-game-frame objed-game-level-3-3
  :read-only t
  :commands (forward-sexp objed-toggle-state)
  (insert "\n\nSelect the inner part of the string in the following expression.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 79))

(objed-game-frame objed-game-level-3-4
  :read-only t
  :commands ((or objed-toggle-state objed-expand-context))
  (insert "\n\nSelect the outer part of the string \"barbecue\".")
  (goto-char 44)
  (objed-activate 'string)
  (objed-toggle-state))

(objed-game-frame objed-game-level-3-5
  :read-only t
  :commands (next-line objed-toggle-state)
  (insert "\n\n               \n    Select the inner part of this line.    ")
  (goto-char 18))

(objed-game-frame objed-game-level-3-6
  :read-only t
  :commands (objed-toggle-side)
  (insert "\n\nMove the cursor to the opposite side of the defun.

  (defun my-icomplete-setup ()
    (setq-local completion-styles '(basic flex))) ")
  (goto-char 57)
  (objed-activate 'sexp))

(objed-game-frame objed-game-level-3-7
  :read-only t
  :commands (objed-expand-context objed-expand-context)
  (insert "\n\nExpand the context of the selection to include the entire `add-hook'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (emacs-init-time \"Emacs loaded in %.2fs\")))
          100) ")
  (goto-char 136)
  (objed-activate 'bracket))

;;;; Level 3:

(objed-game-level objed-game-level-4
  "Level 4: Switch types"
  :intro objed-game-level-4-intro
  :next objed-game-level-4a
  objed-game-level-4-1
  objed-game-level-4-2
  objed-game-level-4-3
  objed-game-level-4-4
  objed-game-level-4-5
  objed-game-level-4-6)

(objed-game-frame objed-game-level-4-intro
  :read-only t
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                       SWITCH OBJECT TYPES

**********************************************************************

While in `objed-mode', you can use basic movement commands
without the modifier keys to change the object type and move the
cursor \\<objed-map>

- \\[objed-right-char]/\\[objed-left-char]
  Move forward/backward one char and activate the char object

- \\[objed-objed--forward-sexp]/\\[objed-objed--backward-sexp]
  Move forward/backward one sexp and activate the sexp object

- \\[objed-forward-word]/\\[objed-backward-word]
  Move forward/backward one word and activate the word object

- \\[objed-next-line]/\\[objed-previous-line]
  Move to the next/previous line and activate the line object

- \\[objed-backward-paragraph]/\\[objed-forward-paragraph]
  Move forward/backward paragraph and switch to paragraph object


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue."))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (beginning-of-buffer))

(objed-game-frame objed-game-level-4-1
  :read-only t
  :keys ("l")
  (insert "\n\nMove the cursor one character to the right without exiting objed mode.")
  (goto-char 30)
  (objed-activate 'word))

(objed-game-frame objed-game-level-4-2
  :read-only t
  :keys ("h")
  (insert "\n\nMove the cursor one character to the left without exiting objed mode.")
  (goto-char 30)
  (objed-activate 'word))

(objed-game-frame objed-game-level-4-3
  :read-only t
  :keys ("f")
  (insert "\n\nSelect the symbol `major-mode' without exiting objed mode.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 104)
  (objed-activate 'sexp))

(objed-game-frame objed-game-level-4-4
  :read-only t
  :keys ("b")
  (insert "\n\nSelect the string in the following expression without exiting
objed mode.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 120)
  (objed-activate 'sexp))

(objed-game-frame objed-game-level-4-5
  :read-only t
  :keys ("s")
  (insert "\n\nSelect the following word without exiting objed mode")
  (goto-char 28)
  (objed-activate 'line))

(objed-game-frame objed-game-level-4-6
  :read-only t
  :keys ("r")
  (insert "\n\nSelect the previous word without exiting objed mode")
  (goto-char 35)
  (objed-activate 'line))

;;;; Level 4:

(objed-game-level objed-game-level-4a
  "Level 4a: Switch types - Avy"
  :intro objed-game-level-4a-intro)

(objed-game-frame objed-game-level-4a-intro
  :read-only t
  (insert (substitute-command-keys (concat "\\<objed-mode-map>
**********************************************************************

                       SWITCH OBJECT TYPES (Avy)

**********************************************************************

Another way to switch object types is by using one of three
prefix keys, followed by the single letter identifier of the
object type to switch to. Here is a list of all the available
object types:

\\{objed-object-map}

The prefix keys are as follows

- " (propertize "c" 'font-lock-face 'help-key-binding) "

  Switch to the nearest instance of the type selected.

  For example, press " (propertize "c l" 'font-lock-face 'help-key-binding) "
  to switch to the line object type selecting the current line.

- " (propertize "#" 'font-lock-face 'help-key-binding) "

  Switch to another object using `avy'.

  This is similar to " (propertize "c" 'font-lock-face 'help-key-binding) "
  except instead of selecting the nearest textual object, it will
  use `avy' to select any object instance in the visible portion
  of the buffer.

- " (propertize "=" 'font-lock-face 'help-key-binding) "

  Switch to another object inside the current one using `avy'.

  This is similar to " (propertize "#" 'font-lock-face 'help-key-binding) "
  except instead of allowing you to select any object in the
  visible portion of the buffer, it will restrict `avy' to find
  instances within the currently selected object.


Press \\<objed-game-mode-map>\\[objed-game-continue] to continue.")))
  (let ((fill-column 70))
    (fill-region 1443 (point-max)))
  (beginning-of-buffer))


;;;; Combined:

(objed-game-level objed-game-combined
  "Combined: All levels"
  objed-game-level-1-1
  objed-game-level-1-2
  objed-game-level-1-3
  objed-game-level-1-4
  objed-game-level-1-5
  objed-game-level-1-6
  objed-game-level-1-7
  objed-game-level-2-1
  objed-game-level-2-2
  objed-game-level-2-3
  objed-game-level-2-4
  objed-game-level-2-5
  objed-game-level-3-1
  objed-game-level-3-2
  objed-game-level-3-3
  objed-game-level-3-4
  objed-game-level-3-5
  objed-game-level-3-6
  objed-game-level-3-7
  objed-game-level-4-1
  objed-game-level-4-2
  objed-game-level-4-3
  objed-game-level-4-4
  objed-game-level-4-5
  objed-game-level-4-6)
