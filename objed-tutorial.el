
;;; objed.el --- Navigate and edit text objects. -*- lexical-binding: t -*-
;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Version: 0.8.3
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
;; Tutorial for `objed'

;;; Code:

(require 'org)

(defun objed-tutorial () "Open a buffer with the objed tutorial."
  (interactive)
  (switch-to-buffer (generate-new-buffer
		     "OBJED TUTORIAL"))
  (objed-mode t)
  (org-mode)
  (outline-show-all)
  (insert (substitute-command-keys (concat
"\\<objed-mode-map>* OBJED TUTORIAL
----------------

Objed tutorial. See end for copying conditions.

* INTRODUCTION
--------------

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




For this tutorial, the characters `>>' at the left margin
indicate directions for you to try using a command.  For
instance:

>> Type \\[objed-activate] in order to activate objed, choosing
   the initial object based on `last-command' and
   `objed-cmd-alist'

>> Type \\[keyboard-quit] to quit objed mode





* ENTERING OBJED MODE
---------------------

When you type \\<objed-mode-map>\\[objed-activate], notice that
objed shows the current object type in the `mode-line'. The
textual content of the object is highlighted visually in the
buffer and the cursor color is changed, too.

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

>> Type \\[forward-sentence] to move the cursor forward a single
   sentence and activate `objed-mode' with the `sentence' object
   type.

Once in `objed-mode', you can change the object type by using a
standard movement key.

>> On the following line, use \\[move-end-of-line] to move the
   cursor to the end of the line and activate the text moved
   over. Type \\[backward-word] to switch the object type from
   `line' to `word' and move the cursor backward a single word.

>> Select the last word in this sentence.




Similar to \\[objed-activate], objed defines some keybindings to
enter `objed-mode' without cursor movement:

- \\[objed-activate-object] `objed-activate-object' Choose an
  object and activate with it

- \\[objed-beg-of-object-at-point]/\\[objed-end-of-object-at-point]
  Activate object at point (determined from context) and move to
  its start/end.

- \\[objed-until-beg-of-object-at-point]/\\[objed-until-end-of-object-at-point]
  Move to beginning/end of object at point and activate text
  moved over.




>> Place the cursor inside the word \"piano\" then type
   \\[objed-activate-object] `w' to select the 'word' object type
   at point.

>> Repeat the previous exercise but with
   \\[objed-activate-object] `l' to activate the line object.




* EXITING OBJED MODE
--------------------

By default important editing keys like Space, DEL or Return are
not bound to modal commands and will execute the regular command
and exit objed. Character movement exits objed, as well. This
makes it convenient to move around and continue by
adding/deleting text.


>> Move the cursor to the end of the following line with
   \\[move-end-of-line], then type \\[forward-char] to move the
   cursor to the next line and exit `objed-mode'. You can now
   insert text freely.

>> Enter text below me!


You can also type " (propertize "g" 'font-lock-face 'help-key-binding) "
or \\[keyboard-quit] to exit `objed-mode'





* OBJECT STATE
--------------

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



>> Enter `objed-mode' and place the cursor within the following
   quote. Select the full inner part of the quote with
   \\[objed-expand-context]. Type \\[objed-toggle-state] to
   toggle adding the quotation marks to the selection


   \"Toggle selecting quote and surrounding quotation marks\"


>> Repeat the previous exercise and type \\[objed-toggle-side]
   to toggle the position of the cursor.





* SWITCH OBJECT TYPE
--------------------

While in `objed-mode', you can use basic movement commands to
change the object type and move the cursor
\\<objed-map>

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


>> In the following code block, execute these maneuvers:

>> 1. Activate `objed-mode' with \\<objed-mode-map>\\[objed-activate]

>> 2. Use \\<objed-map>\\[objed-next-line] to select the comment
   string starting with `FIRST'

>> 3. Try using \\[objed-right-char]/\\[objed-left-char] to
   navigate around the characters within the comment string

>> 4. Try using \\[objed-forward-word]/\\[objed-backward-word] to
   navigate around the words within the comment string

>> 5. Use \\[objed-objed--backward-sexp]/\\[objed-objed--forward-sexp]
   to navigate between `message' calls within the function



#+begin_src elisp
  (defun demo-function ()
    \"FIRST navigate around the words in this sentence\"
    (message \"SECOND navigate sexps in this function\")
    (message \"Other sexp\")
    (message \"Another sexp\")
    (message \"A fourth sexp\")
 #+end_src





There are additional commands to use for blocks of
text. Repeating these commands expands the selection by changing
the object type.

- \\[objed-expand-block] `objed-expand-block'
  Activate (line based) object at point and move to its start. On
  repeat proceed to beginning of the indentation block, comment
  block, paragraph or other block objects.

- \\[objed-beg-of-block] `objed-beg-of-block'
  Move to beginning of line and activate the text moved over. On
  repeat proceed to beginning of blocks like explained above.

- \\[objed-end-of-block] `objed-end-of-block'
  Move to end of line and activate the text moved over. On repeat
  proceed to end of blocks like explained above.

- \\[objed-extend] `objed-extend'
  Extend the current object. Allows you to extend and shrink the
  current selection by changing the object type.



>> In the following randomly generated paragraph, execute the
   following maneuvers

>> 1. Activate `objed-mode' with \\<objed-mode-map>\\[objed-activate]

>> 2. Navigate to the middle of some line and type
   \\<objed-map>\\[objed-beg-of-block]. Notice that only the text
   between the cursor and the beginning of the line are selected

>> 3. Type \\[objed-beg-of-block] once more to select everything
   to the beginning of the paragraph from your original cursor
   position

>> 4. Navigate back to the middle of a line and type
   \\[objed-expand-block]. Notice that the entire line is
   selected regardless of where the cursor started.

>> 5. Type \\[objed-expand-block] again to select the entire
   paragraph.

>> 6. From somewhere within the paragraph, type \\[objed-extend]
   and use the menu to extend and shrink the current object
   selection


It was a scrape that he hardly noticed. Sure, there was a bit of
blood but it was minor compared to most of the other cuts and
bruises he acquired on his adventures. There was no way he could
know that the rock that produced the cut had alien genetic
material on it that was now racing through his bloodstream. He
felt perfectly normal and continued his adventure with no
knowledge of what was about to happen to him.





`objed-mode' also defines an `identifier' object type which is
useful in code editing for finding references to a particular
function or variable name.

- \\[objed-goto-next-identifier] `objed-goto-next-identifier'
  Switch to identifier object or move to next

- \\[objed-goto-prev-identifier] `objed-goto-prev-identifier'
  Switch to identifier object and move to previous




>> In the following code segment, enter `objed-mode' and place
   the cursor in or around `my-var'. Type
   \\[objed-goto-next-identifier] and
   \\[objed-goto-prev-identifier] to navigate around references
   to `my-var'


#+begin_src elisp
  (setq my-var 3)
  (setq my-other-var 5)

  (message (format \"Multiplication: %d\" (* my-var my-other-var)))
  (message (format \"Subtraction: %d\" (- my-var my-other-var)))
  (message (format \"Addition: %d\" (+ my-var my-other-var)))
#+end_src




Another way you may switch object type is pressing " (propertize "c" 'font-lock-face 'help-key-binding) " plus the
single letter identifier of the type to switch to. Here is a
list of all the available object types:

\\{objed-object-map}



>> Enter objed mode and press " (propertize "c m" 'font-lock-face 'help-key-binding) "
   to switch to the email object type and place
   the cursor at the beginning of the next email
   address in the buffer

real@email.com


>> Enter objed mode and type " (propertize "c TAB email" 'font-lock-face 'help-key-binding) "
   to repeat the last exercise with minibuffer completion




You may also use `avy' to switch to another object type by using
the previous method prefixed with " (propertize "#" 'font-lock-face 'help-key-binding) " instead of " (propertize "c" 'font-lock-face 'help-key-binding)".



>> Enter `objed-mode', then type " (propertize "# l" 'font-lock-face 'help-key-binding) " to start `avy' finding a
   line object. Enter the letters appearing before the line below
   that says `THIS LINE' to navigate to that line


Not this line
Not this line
THIS LINE
Not this line
Not this line



If you would like to limit avy to search within your current
selection, use the prefix " (propertize "=" 'font-lock-face 'help-key-binding) " instead of " (propertize "#" 'font-lock-face 'help-key-binding) "



>> Select the following randomly generated paragraph by typing
   \\[objed-forward-paragraph], then type " (propertize "= w" 'font-lock-face 'help-key-binding) " to start avy
   searching for a word object within your selection. Type the
   letters appearing before the word `imagined' in order to
   navigate to that word.



The chair sat in the corner where it had been for over 25
years. The only difference was there was someone actually sitting
in it. How long had it been since someone had done that? Ten
years or more he imagined. Yet there was no denying the presence
in the chair now.




Lastly, you can switch to another object of the same type using
avy by typing \\[objed-ace]


>> Enter `objed-mode' then type " (propertize "c s" 'font-lock-face 'help-key-binding) "
   to switch to the `string' object type. Press \\[objed-ace] and
   type the letter(s) appearing before the name `Edwin' in order
   to move the cursor to that string.

\"Mike\"
\"Katy\"
\"Jodie\"
\"Edwin\"
\"Ayla\"
\"Shawn\"





* MOVEMENT COMMANDS
-------------------


Once you have an object type selected, you may use these commands
to move the cursor around a buffer.

- \\[objed-previous]​/\\[objed-next]
  Move to the start of previous/next instance of current object
  type

- \\[objed-top-object]/\\[objed-bottom-object]
  Goto first/last instance of current object type

- \\[objed-last] `objed-last'
  Pop to last state, which restores the last position and any
  object data.


* INDENTING
-----------

- \\[objed-indent-left]/\\[objed-indent-right]
  Move/indent all lines in object right/leftward

- \\[objed-indent-to-left-tab-stop]/\\[objed-indent-to-right-tab-stop]
  Move/indent all lines in object to right/leftward to tab stop.

* MOVING OBJECTS
----------------

- \\[objed-forward-barf-sexp]/\\[objed-forward-slurp-sexp]
  Slurp following sexp into current object/Barf last sexp out of
  current object.

- " (propertize "S-left" 'font-lock-face 'help-key-binding) "/" (propertize "S-right" 'font-lock-face 'help-key-binding) "/" (propertize "S-up" 'font-lock-face 'help-key-binding) "/" (propertize "S-down" 'font-lock-face 'help-key-binding) "/\\[objed-move-object-forward]/\\[objed-move-object-backward]
  Move current object type forward/backward

- \\[objed-move-char-forward]/\\[objed-move-char-backward]
  Switch to char object and move it forward/backward

- \\[objed-move-word-forward]/\\[objed-move-word-backward]
  Switch to word object and move it forward/backward

- \\[objed-move-line-forward]/\\[objed-move-line-backward]
  Switch to line object and move it forward/backward



* EDITING OBJECTS
-----------------

Commands to edit objects (applying operations to them). When the
region is active the operation acts on the current region. To act
on multiple objects at once you can mark them first using
\\[objed-mark]/\\[objed-toggle-mark-backward]


- \\[objed-del-insert]
  Delete current object and exit to insert state

- " (propertize "k" 'font-lock-face 'help-key-binding)"/\\[objed-kill]
  Kill current object(s) and continue to next/previous instance from point

- " (propertize "d" 'font-lock-face 'help-key-binding)"/\\[objed-delete]
  Delete current object(s). Continues by selecting the next/previous
  instance from point

- \\[objed-copy]
  Copy current object(s) to kill ring. On repeat add text to objed
  register

- \\[objed-yank]
  Yank last killed text at point. On repeat, cycle through kill ring

- \\[objed-objed-indent]
  Indent object(s)

- \\[objed-objed-comment-or-uncomment-region]
  Toggle commenting object(s)

- \\[objed-replace]
  Query replace narrowed to current object

- \\[objed-raise]
  Replace object with inner part (raise)

- \\[objed-execute]
  Run object contents as shell commands

- \\[object-objed-pipe-region]
  Pipe object region through shell command

- \\[objed-objed-ipipe]
  Incrementally construct command chain to replace text

- \\[objed-objed-run-or-eval]
  Evaluate current object in REPL (need eval-in-repl to be installed)

- \\[objed-objed-duplicate-down]
  Duplicate object

- \\[objed-objed-comment-duplicate]
  Comment and duplicate object

- \\[objed-flyspell-region]
  Spell check textual content of object using `flyspell'

- \\[objed-undo-in-object]
  Undo in current object region

- \\[objed-objed-electric-pair]
  Add surroundings to object(s) with any pair using `electric` (built-in)

- " (propertize "x" 'font-lock-face 'help-key-binding) "
  Prefix to access other operations, see `objed-op-map' for
  available operations and `objed-define-op' to add your own.





* COPYING
--------- 

This tutorial descends from a long line of Emacs tutorials
starting with the one written by Stuart Cracraft for the original
Emacs.

This version of the tutorial is not a part of GNU Emacs, but
derived from the standard Emacs tutorial,

Copyright (C) 1985, 1996, 1998, 2001-2019 Free Software
Foundation, Inc.")))
(goto-char 1))

(provide 'objed-tutorial)
