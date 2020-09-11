;;; objed-objects.el --- Part of the objed package -*- lexical-binding: t; -*-
;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Version: 0.8.1
;; Maintainer: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/objed

;; This program is free software; you can redistribute it and/or modify
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
;; Code for text objects used by `objed'.
;;; Code:

;; * Bytecomp

(require 'cl-lib)
(require 'subword)
(require 'face-remap)

;; info for byte-comp
(defvar objed-map)
(defvar objed-object-map)
(declare-function avy-process "ext:avy")
(declare-function avy--style-fn "ext:avy")
(declare-function avy-goto-char "ext:avy")
(declare-function stripe-buffer-mode "ext:stripe-buffer")

(declare-function sgml-skip-tag-backward "ext:sgml-mode")
(declare-function sgml-skip-tag-forward "ext:sgml-mode")

(declare-function org-at-heading-p "ext:org")
(declare-function org-back-to-heading "ext:org")

(declare-function outline-back-to-heading "ext:outline")
(declare-function outline-next-heading "ext:outline")
(declare-function outline-previous-heading "ext:outline")
(declare-function outline-next-visible-heading "ext:outline")
(declare-function outline-previous-visible-heading "ext:outline")

(declare-function objed--object-dispatch "ext:objed")
(declare-function objed-current-or-next-context "ext:objed")
(declare-function objed-current-or-previous-context "ext:objed")
(declare-function objed--get-current-state "ext:objed")

(declare-function objed--install-advices "ext:objed")
(declare-function objed--install-advices-for "ext:objed")
(declare-function objed-goto-next-identifier "ext:objed")
(declare-function objed-goto-prev-identifier "ext:objed")
(declare-function objed-next-identifier "ext:objed")
(declare-function objed-prev-identifier "ext:objed")
(declare-function objed-first-identifier "ext:objed")
(declare-function objed-last-identifier "ext:objed")




;; * Macros

(defvar-local objed--object nil
  "The symbol of the current object.")


(eval-and-compile
  (require 'rx)
  (defun objed--get-regex-object (bregex eregex)
    "Return regex object between BREGEX and EREGEX.

The inner object part will be the text between the matches for
those two expressions.

BREGEX is the regular expression for the start of the object. If
the regular expressions contains a group, any text which is part
of this group will belong to the inner object part.

EREGEX is the regular expression for the end of the object. If
the regular expressions contains a group, any text which is part
of this group will belong to the inner object part.

EREGEX can also be an empty string. In this case objects are
separated by the BREGEX expression and reach until the next one
or until the buffer end if no next instance can be found."
    (let* ((obounds ())
           (ibounds ())
           (opos (point)))
      (save-mark-and-excursion
        ;; try to move into object when at boundary
        (if (looking-at bregex)
            (goto-char (or (match-end 1)
                           (match-end 0)))
          (if (looking-back eregex (line-beginning-position))
              (goto-char (or (match-beginning 1)
                             (match-beginning 0)))
            (re-search-forward eregex nil t)
            (goto-char (or (match-beginning 1)
                           (match-beginning 0)))))
        (when (and ;; goto possible start
               (re-search-backward bregex nil t)
               (push (or (match-beginning 1)
                         (match-end 0))
                     ibounds)
               (push (match-beginning 0)
                     obounds)
               ;; goto possible end
               (goto-char (or (match-end 1)
                              (match-end 0)))
               (cond ((string= "" eregex)
                      ;; no end provided, objects are separated by beginning
                      ;; regex
                      (cond ((re-search-forward bregex nil t)
                             (push (match-beginning 0) obounds)
                             ;; no way to tell so just skip ws
                             ;; to have a sensible default
                             (goto-char (car obounds))
                             (objed--skip-ws t)
                             (push (point) ibounds))
                            (t
                             ;; if there is no match that means the buffer is
                             ;; the object end
                             (goto-char (point-max))
                             (push (point) obounds)
                             (objed--skip-ws t)
                             (push (point) ibounds))))
                     (t
                      (re-search-forward eregex nil t)
                      (push (or (match-end 1)
                                (match-beginning 0))
                            ibounds)
                      (push (match-end 0)
                            obounds)))
               ;; when point was within start and end
               (<= (cadr obounds) opos (car obounds)))
          (list (nreverse obounds)
                (nreverse ibounds))))))

  (defun objed--transform-pos-data (plist)
    (cond  ((and (plist-get plist :beg)
                 (or (plist-get plist :end)
                     (stringp (plist-get plist :beg))))
            (let ((np nil)
                  (alt nil)
                  (skip nil))
              ;; filter :beg :end keywords
              (dolist (item plist)
                (if (memq item '(:beg :ibeg :end :iend))
                    (progn (push item alt)
                           (setq skip t))
                  (if (and skip
                           (not (keywordp item)))
                      (push item alt)
                    (push item np)
                    (setq skip nil))))
              ;; new and alternate plists
              (setq np (nreverse np))
              (setq alt (nreverse alt))

              ;; merge ... :get-obj "regex search"
              (if (and (stringp (plist-get plist :beg))
                       (or (stringp (plist-get plist :end))
                           (not (plist-get plist :end))))
                  (let ((bregex (plist-get plist :beg))
                        (eregex (or (plist-get plist :end)
                                    "")))
                    (append np
                            (list :try-prev)
                            (if (string= "" eregex)
                                (list `(when (re-search-backward ,bregex)
                                         (goto-char (match-end 0))))
                                (list `(when (re-search-backward ,eregex)
                                     (goto-char (match-beginning 0)))))
                            (list :try-next)
                            (list `(when (re-search-forward ,bregex)
                                         (goto-char (match-end 0))))
                            (list :get-obj)
                            (list
                             `(objed--get-regex-object ,bregex
                                                       ,eregex))))
                ;; merge ... :get-obj (objed-make-object :beg ... :end...)
                (let ((make nil))
                  (dolist (el alt)
                    (when (keywordp el)
                      (progn
                        (push el make)
                        (push (plist-get alt el) make))))
                  (setq make (nreverse make))
                  (push 'objed-make-object make)
                  (append np
                          (list :get-obj)
                          ;; TODO:save-mark-and-excursion still needed?
                          ;; is wrapped already?
                          (list (append (list 'save-mark-and-excursion)
                                        (list make))))))))
           (t
            (user-error "Malformed macro"))))


  (defun objed--get-arg-plist (keylst valid &optional wrapped)
    "Wraps any forms of keys in keylst in `progn' and returns property list.
KEYLST is the list of keys and forms for object creation. VALID
is a list of valid keyword for the returned list whic is a
property list where each key has an associated progn."
    (let* ((keyw (pop keylst))
           (vkeyw (and keyw (keywordp keyw) (memq keyw valid) keyw))
           forms)
      (cond ((memq vkeyw '(:mode :no-skip :commands))
             ;; skip
             (objed--get-arg-plist (cdr keylst) valid wrapped))
            (vkeyw
             (while (and (not (keywordp (car keylst)))
                         keylst)
               (push (pop keylst) forms))
             (push keyw wrapped)
             ;; allowed to move point
             (cond ((memq vkeyw '(:try-next :try-prev :ref))
                    (push `(let ((objed--block-p t))
                             ,@(nreverse forms))
                          wrapped))
                   ((memq vkeyw '(:beg :end :ibeg :iend))
                    (if (and (not (cdr forms))
                             (stringp (car forms)))
                        (push (car forms) wrapped)
                      (if (and (not (cdr forms))
                               (eq (caar forms) 'rx))
                          (push (macroexpand-1 (car forms))
                                wrapped)
                        (push `(let ((objed--block-p t))
                                 ,@(nreverse forms))
                              wrapped))))
                   (t
                    ;; objed--block-p: dont run objeds advices here
                    (push `(let ((objed--block-p t))
                             (save-mark-and-excursion
                               ,@(nreverse forms)))
                          wrapped)))

             (objed--get-arg-plist keylst valid wrapped))
            (keylst
             (error "Malformed Object. Keyword %s not recognized" keyw))
            (t
             (nreverse wrapped))))))


(defmacro objed-define-object (package name &rest args)
  "Declare a text object for `objed'.

Usage:

  (objed-define-object package name
     [:keyword [code-form]...]...)


This macro creates a command named objed-<name>-object. This
command can be used to activate objed for the defined object and
is used internally to query for information needed for objed
commands.

PACKAGE is the name of the package the object should be loaded
for. If non-nil this will defer loading until PACKAGE is
available.

NAME is a symbol which defines the name which will be used to
refer to this object. ARGS is a list of keyword arguments and
corresponding values according to the following descriptions:

:get-obj

Code to run which returns object positions as a list of the form:

    ((object-start object-end)
     (inner-start inner-end))

The function `objed-make-object' can be used to create such a
list. For convenience it is also possible that the code returns a
cons cell of the bounds of object (like what the built-in
`bounds-of-thing-at-point' variations return). If inner positions
are omitted they are determined by `objed--inner-default'. If
there is no object at point the code should return nil.

:beg, :ibeg, :end, :iend

These keywords can be used instead of :get-obj above. The value
for each is the code to run which should return the point
position corresponding to the keyword. Point is allword to move
between the keyword expression. The code runs in the same order
the keywords are provided.

It is also possible to use only :beg and :end with regular
expressions to define an object. See `objed--get-regex-object'
for details of their format. If :end is omitted the regexp
provided by :beg separates the objects on its own. This can be
used for text objects which don't have an end marker.

:try-next (optional)

Code to run which moves point to the next available object.
If :no-skip is not set the code can assume it runs after point is
moved out to the end of the current one if any. This will get
called until :get-obj returns non-nil. To indicate that search
needs to be stopped, throw an error.


:try-prev (optional)

Code to run which moves point to the previous available object.
The code can assume it runs after point is moved to the beginning
of the current one if any. This will get called until :get-obj
returns non-nil. To indicate that search needs to be stopped,
throw an error.

:mode (optional)

Object defintions which use this keyword derive from an already
existing object with the same NAME. If given it should be a
symbol of a `major-mode'. Any keyword definitions of the mode
specific version will override the ones from the non mode
specific version.

:atp (optional)

Code to run which returns non-nil if point is right before the
object.

:ref (optional)

Code to run which returns an object symbol which can be used to
navigate references of an object. This defaults to the textual
content of an object.

:max-search-forward (optional)

Code to run which returns the maximal position an object of this type
is searched for (in forward direction).

:no-skip (optional)

If this keyword is provided with a non-nil value, the current object
is not skipped before search for the next one via :try-next.

:commands (optional)

If given the value should be a list of commands for which objed
should activate (when variable `objed-mode' is on) with the object beeing
defined."
  (declare (indent 2))
  (let* ((mode (plist-get args :mode))
         (noskip (plist-get args :no-skip))
         (commands (plist-get args :commands))
         (fname (if mode
                    (intern (format "objed-%s-%s-object" name mode))
                  (intern (format "objed-%s-object" name))))
         ;; wrap code chunks
         (args (objed--get-arg-plist
                args
                '(:mode :no-skip :max-search-forward :commands :atp :ref :get-obj :try-next :try-prev
                        :beg :ibeg :iend :end)))
         ;; transform to final form if necessary
         (args (if (plist-get args :get-obj) args
                 (objed--transform-pos-data args)))
         (arg (make-symbol "arg"))
         (cbody nil)
         (doc (format "%s object." (capitalize (symbol-name name))))
         (atp (plist-get args :atp))
         (max (plist-get args :max-search-forward))
         (obj (plist-get args :get-obj))
         (next (plist-get args :try-next))
         (prev (plist-get args :try-prev))
         (ref  (plist-get args :ref)))

    (unless mode
      (push `((and (eq real-this-command ',fname)
                   (not ,arg))
              (objed--object-dispatch ',name))
            cbody))
    (when atp
      (push `((eq ,arg :atp)
              ,atp)
            cbody))

    (when max
      (push `((eq ,arg :max-search-forward)
              ,max)
            cbody))

    (when ref
      (push `((eq ,arg :ref)
              ,ref)
            cbody))
    (when obj
      (push `((eq ,arg :get-obj)
              (let ((pdata ,obj))
                (if (and (consp pdata)
                         (not (consp (cdr pdata))))
                    (objed-make-object :obounds pdata)
                  pdata)))
            cbody))
    (when next
      (push `((eq ,arg :try-next)
              ,next)
            cbody))
    (when prev
      (push `((eq ,arg :try-prev)
              ,prev)
            cbody))

    (cond (mode
           (let ((res (if package
                          (list `',package 'with-eval-after-load)
                        (list 'progn))))
             (when noskip
               (push `(put ',fname 'objed-no-skip t)
                     res))
             (when commands
               (push `(with-eval-after-load 'objed
                        (objed--install-advices-for ',commands ',name))
                     res))
             ;; catch all return arg if not present
             (push `(t ,arg) cbody)
             (push `(defun ,fname (,arg)
                      ,doc
                      (cond ,@(nreverse cbody)))
                   res)
             (nreverse res)))
          (t
           (let ((res (if package
                          (list `',package 'with-eval-after-load)
                        (list 'progn))))

             (when noskip
               (push `(put ',fname 'objed-no-skip t)
                     res))
             (when commands
               (push `(objed--install-advices-for ',commands ',name)
                     res))
             (push `(defun ,fname (,arg)
                      ,doc
                      (interactive "i")
                      (cond ,@(nreverse cbody)))
                   res)
             (nreverse res))))))


(defun objed--define-kpair (map key name)
  "Use MAP to define KEY for object NAME."
  (let ((cmd (objed--name2func name 'nomode)))
    (define-key map key cmd)))

(defun objed-define-global-object-keys (&rest kpairs)
  "Define global object keys.

KPAIRS are pairs of the key and the object name."
  (let ((map (default-value 'objed-object-map)))
    (while kpairs
      (objed--define-kpair map (pop kpairs) (pop kpairs)))))

(defun objed-define-local-object-keys (&rest kpairs)
  "Define object keys locally for current buffer.

This function is intended to be used inside mode hooks to add
mode specific object bindings to the currently existing ones. If
you want to replace the object key bindings entirely with local
ones see `objed-define-local-object-keys*'.

KPAIRS are pairs of the key and the object name."
  (unless (local-variable-p 'objed-map)
    (setq-local objed-map
                (make-composed-keymap nil (default-value 'objed-map))))
  (unless (local-variable-p 'objed-object-map)
    (setq-local objed-object-map
                (make-composed-keymap nil (default-value 'objed-object-map))))
  (while kpairs
    (objed--define-kpair objed-object-map (pop kpairs) (pop kpairs)))
  (let ((switchk (where-is-internal (default-value 'objed-object-map)
                                    (default-value 'objed-map) t)))
    (define-key objed-map switchk objed-object-map)))


(defun objed--init-local-map (lmap map)
  "Initilize local LMAP to shadow bindings in MAP."
  (map-keymap (lambda (ev def)
                (if (keymapp def)
                    (objed--init-local-map lmap def)
                  (define-key lmap (vector ev) nil)))
              map))

(defun objed-define-local-object-keys* (&rest kpairs)
  "Define object keys locally for current buffer.

This function is intended to be used inside mode hooks and
creates new object key bindings for the current buffer. This
means any previous object key bindings bindings are replaced by
the bindings defined in KPAIRS.

If you only want to add or change a few mode specific bindings
see `objed-define-local-object-keys'.

KPAIRS are pairs of the key and the object name."
  (unless (local-variable-p 'objed-map)
    (setq-local objed-map
                (make-composed-keymap nil (default-value 'objed-map))))
  (setq-local objed-object-map (make-sparse-keymap))
  (objed--init-local-map objed-object-map (default-value 'objed-map))
  (while kpairs
    (objed--define-kpair objed-object-map (pop kpairs) (pop kpairs)))
  (let ((switchk (where-is-internal (default-value 'objed-object-map)
                                    (default-value 'objed-map) t)))
    (define-key objed-map switchk objed-object-map)))


(defmacro objed--with-narrow-for-text (&rest body)
  "Execute BODY narrowed to string or comment."
  `(save-restriction
    ;; stay inside
     (unless (derived-mode-p 'text-mode)
       (objed--narrow-if-string-or-comment))
     ,@body))


;; * Global vars saving object information
;; TODO: use defstruct object instead



(defvar objed--current-obj nil
  "The current object position data.

Positions are stored in a list of the form:

    ((object-start object-end)
     (inner-start inner-end))")

(defvar-local objed--obj-state nil
  "The state used to get object positions.

Either the symbol `whole' or `inner'.")


(defvar objed--marked-ovs nil
  "List of overlays of marked objects.")

;; * Internal object access functions

(defun objed--inside-object-p (obj)
  "Return non-nil if point point inside object OBJ."
  (let*  ((objed--object obj)
          (objed--obj-state 'whole)
          (obj (if (symbolp obj)
                   (objed--get)
                 obj)))
    (when (and obj (not (objed--distant-p obj)))
      obj)))

(defun objed--beg (&optional obj)
  "Get beginning position of object.

Ignores current object state. OBJ is the object to use and
defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (caar obj)))

(defun objed--end (&optional obj)
  "Get end position of object.

Ignores current object state. OBJ is the object to use and
defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (cadr (car obj))))

(defun objed--object-at-point (obj &optional state)
  "Return object data for OBJ and STATE a point.

Does return nil when there is no such object."
  (let* ((objed--object obj)
         (objed--obj-state (or state 'whole))
         (o (ignore-errors (objed--object :get-obj))))
    (when o
      (if (eq state 'inner)
          (nreverse o)
        o))))

(defun objed--other (&optional obj)
  "Return object position opposite to point.

Ignores current object state. OBJ is the object to use and
defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (if (/= (point) (objed--end))
        (objed--end obj)
      (objed--beg obj))))

(defun objed--min (&optional obj)
  "Get minimal position of current object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (objed--apply #'min obj)))


(defun objed--max (&optional obj)
  "Get maximal position of current object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (objed--apply #'max obj)))

(defvar objed--basic-objects
  '(sexp line identifier word char region buffer)
  "Basic objects.

Basic object are objects which have no next/previous or which
have their own movement commands.")


(defun objed--basic-p ()
  "Return non-nil if current object is a basic object.

From basic objects `objed' starts expanding to context objects.
Thus this should be objects which have their own movement
commands."
  (memq objed--object objed--basic-objects))

(defun objed--current (&optional obj)
  "Get the current range of interest.

If the region is active the range is defined by the region bounds
otherwise the its the head of object OBJ which defaults to
`objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (cond ((region-active-p)
           (list (region-beginning)
                 (if (objed--basic-p)
                     (region-end)
                   (max (objed--end)
                        (region-end)))))
          (t
           (car obj)))))


(defun objed--bounds (&optional obj)
  "Get the current object bounds.

If the region is active bounds are the region bounds otherwise
the head of object OBJ which defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (if (region-active-p)
        (cons (region-beginning) (region-end))
      (cons (objed--beg obj)
            (objed--end obj)))))

(defun objed--alt (&optional obj)
  "Get the current tail of `objed--current-obj'.

OBJ is the object to use and defaults to `objed--current-obj'."
    (let ((obj (or obj objed--current-obj)))
      (cadr obj)))

(defun objed--alt-beg (&optional obj)
  "Get beginning position of tail of object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let* ((obj (or obj objed--current-obj))
         (posn (objed--alt obj)))
    (car posn)))

(defun objed--alt-end (&optional obj)
  "Get end position of tail of object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let* ((obj (or obj objed--current-obj))
         (posn (objed--alt obj)))
    (cadr posn)))

(defun objed--obeg (&optional obj)
  "Get beginning position of object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (if (objed--inner-p)
        (objed--alt-beg obj)
      (objed--beg obj))))

(defun objed--oend (&optional obj)
  "Get end position of object.

OBJ is the object to use and defaults to `objed--current-obj'"
  (let ((obj (or obj objed--current-obj)))
    (if (objed--inner-p)
        (objed--alt-end obj)
      (objed--end obj))))

(defun objed--ibeg (&optional obj)
  "Get inner beginning position of object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (if (objed--inner-p)
        (objed--beg obj)
      (objed--alt-beg obj))))

(defun objed--iend (&optional obj)
    "Get inner end position of object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (if (objed--inner-p)
        (objed--end obj)
      (objed--alt-end obj))))

(defun objed--get-left-boundary ()
  "Get left boundary of current object."
  (buffer-substring (objed--obeg) (objed--ibeg)))

(defun objed--get-right-boundary ()
  "Get right boundary of current object."
  (buffer-substring (objed--iend) (objed--oend)))

(defun objed--object-string ()
  "Return object string content."
  (filter-buffer-substring (objed--beg)
                           (objed--end)))

(defun objed--at-object-p (obj)
  "Return non nil when point is at object OBJ."
  (funcall (objed--name2func obj) :atp))


(defun objed--goto-char (pos)
  "Move to position POS possibly skipping leading whitespace."
  (goto-char
   (if (eq objed--object 'char)
       pos
     (objed--skip-forward pos 'ws))))

(defun objed--collect-backward (pos min &optional collf)
  "Collect object positions in backward direction.

Start from position POS and stop at MIN position.

The returned list contains cons cells of the start positions of
the objects and the current window. If COLLF is t collect the
end positions instead. Leading or trailing ws are ignored by
default.

If COLLF is function it recieves the object as argument and
should return the data to collect."
  (let ((cw (get-buffer-window))
        (sobj nil)
        (posns nil)
        (objed--obj-state 'whole)
        (obj nil))
    (save-excursion
      (goto-char pos)
      (while (and (> pos min)
                  (setq obj (objed--get-prev (point)))
                  (not (equal obj sobj)))
        (setq sobj obj)
        (goto-char (setq pos (objed--beg obj)))
        (cond ((and collf
                    (not (eq collf t)))
               (push (funcall collf obj) posns))
              (t
               (push (cons (if (eq collf t)
                               (objed--skip-backward
                                (objed--max obj) 'ws)
                             (objed--skip-forward pos 'ws))
                           cw)
                     posns))))
      posns)))

(defun objed--collect-forward (pos max &optional collf)
  "Collect object positions in forward direction.

Start from position POS and stop at MAX position.

The returned list contains cons cells of the start positions of
the objects and the current window. If COLLF is t collect the
end positions instead. Leading or trailing ws are ignored by
default.

If COLLF is function it recieves the object as argument and
should return the data to collect."
  (let ((cw (get-buffer-window))
        (sobj nil)
        (posns nil)
        (objed--obj-state 'whole)
        (obj nil))
    (save-excursion
      (goto-char pos)
      (while (and (< pos max)
                  (setq obj (objed--get-next (point)))
                  (not (equal obj sobj)))
        (setq sobj obj)
        (if (objed--no-skipper-p)
            (goto-char (setq pos (objed--beg obj)))
          (goto-char (setq pos (objed--end obj))))
        (cond ((and collf  (not (eq collf t)))
               (push (funcall collf obj) posns))
              (t
               (push (cons (if (eq collf t)
                               (objed--skip-backward
                                (objed--max obj) 'ws)
                             (objed--skip-forward (objed--beg obj) 'ws))
                           cw)
                     posns)))))
    (setq posns (nreverse posns))))

(defun objed--no-skipper-p ()
  "If current object should be skipped."
  (get (objed--name2func objed--object)
       'objed-no-skip))

(defun objed--collect-object-positions (beg end &optional fromp collf)
  "Collect object positions.

Returns object positions between BEG and END.

If FROMP is non-nil collect from that position otherwise collect before
and after current object.

By default the returned list contains cons cells of the start
positions of the objects and the current window.

COLLF has the same meaning as for `objed--collect-forward' and
`objed--collect-backward'."
  (save-restriction
    (narrow-to-region beg end)
    (append (objed--collect-backward
             (or fromp (objed--min))
             beg collf)
            (objed--collect-forward
             (or fromp (if (objed--no-skipper-p)
                           (objed--min) (objed--max)))
             end collf))))

(defun objed--map (fun &optional obj beg end)
  "Call FUN with object data for each object of current type.

Return a list of the results.

If OBJ is on-nil it should be the symbol of the object type to
search for. By default search the whole buffer, alternatively
provide BEG and END position for region to search."
  (let ((objed--object (or obj objed--object)))
    (objed--collect-object-positions
     (or beg (point-min))
     (or end (point-max))
     (point) fun)))

(defun objed--objects (&optional obj beg end)
  "Return list of all objects of current type.

If OBJ is on-nil it should be the symbol of the object type to
search for. By default search the whole buffer, alternatively
provide BEG and END position for region to search."
  (objed--map #'identity obj beg end))

(defun objed--collect-object-lines ()
  "Collect first lines of objects before and after current object.

Each string has its position as property pos on the first
character of the string."
  (let (lines llb lle lb le)
    (dolist (pos2win (objed--collect-object-positions
                      (point-min)
                      (point-max))
                     (nreverse lines))
      (save-excursion
        (setq lle le)
        (setq llb lb)
        (goto-char (car pos2win))
        (let ((str (buffer-substring
                    (setq lb (objed--skip-forward
                              (line-beginning-position) 'ws))
                    (setq le (line-end-position)))))
          ;; one object per line
          (when (and (or (not llb)
                         (< lle lb)
                         (not lle)
                         (> llb le))
                     (not (string= "" str)))
            (push (cons str (point)) lines)))))))


(defvar avy-action)
(defvar avy-all-windows)
(defun objed--ace-until (&optional start back)
  "Get position of object using `avy'.

Start at pos START. Default to forward unless BACK is non-nil."
  (let* ((avy-action #'identity)
         (avy-style 'at-full)
         (avy-all-windows t)
         (posns (if back
                    (objed--collect-backward
                     (or start (point))
                     (window-start) t)
                  (objed--collect-forward
                   (or start (point))
                   (window-end)))))
    (save-excursion
      (cond (posns
             (let ((pos (if (> (length posns) 1)
                            (avy-process posns (avy--style-fn avy-style))
                          (caar posns))))
               (when (integer-or-marker-p pos)
                 (objed--get back pos))))
             (t
              (prog1 nil
                (message "No objects found.")))))))


(defun objed--get (&optional dir pos)
  "Get object at current position.

Direction defaults to forward unless DIR is non-nil which means
to search backwards.

POS defaults to point. When no object is found at current
position returns the next accessible one in DIR. Object position
order depends on `objed--obj-state'. To exit early from search
objects can throw an error."
  (save-excursion
    (let ((darg (if dir :try-prev :try-next))
          (max (if dir nil (objed--object :max-search-forward)))
          (stop (if dir #'bobp #'eobp)))
      (when pos
         (goto-char pos))
      (let (invisible break nobj obj)
        ;; while there is no new object found which is visible
        ;; and the buffer boundary is not reached
        (while (and (not break)
                    (or (not max)
                        (<= (point) max))
                    (or (not (and (setq nobj (objed--object :get-obj))
                                  (and nobj (not (equal obj nobj)))))
                        (and nobj
                             ;; update the last seen one
                             (setq obj nobj)
                             (setq invisible
                                   (objed--invisible-p (objed--beg nobj)))))
                    (not (funcall stop)))
          (cond (invisible
                 (setq invisible nil)
                 (goto-char (if dir
                                (objed--prev-visible-point)
                              (objed--next-visible-point))))
                (t
                 (let ((f (if dir  '> '<))
                       (step (if dir -1 1)))
                   (unless (funcall stop)
                     (let ((pos (point)))
                       (objed--object darg)
                       ;; if point has not moved
                       ;; fallback to move one char
                       ;; in right direction
                       (when (= (point) pos)
                         (forward-char step))
                       ;; check for valid move direction to avoid inf. loop if
                       ;; the code of object misbehaves
                       (when (funcall f (point) pos)
                         (setq break t))))))))
        (if (objed--inner-p)
            (nreverse nobj)
          nobj)))))


(defun objed--copy-object (&optional obj)
  "Make a deep copy of object OBJ.

OBJ defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (list (copy-sequence (car obj))
          (copy-sequence (cadr obj)))))

(defun objed--get-object (o &optional s)
  "Get object O with state S.

When object O is already current return it."
  (if (eq objed--object o)
      objed--current-obj
    (let ((objed--object o)
          (objed--obj-state (or s 'whole)))
      (objed--get))))

(defun objed--name2func (name &optional no-mode)
  "Return function name for object with NAME.

If NO-MODE is non-nil, ignore mode specific versions."
  (let ((name (or (and (symbolp name) (symbol-name name))
                  name)))
    (or (and (not no-mode)
             (intern-soft (format "objed-%s-%s-object" name major-mode)))
        (intern-soft (format "objed-%s-object" name))
        (error "Object unknown: %s" name))))

(defun objed--object (query &optional obj no-mode)
  "Call current object function with QUERY.

OBJ defaults to variable `objed--object'. If NO-MODE is non-nil ignore mode
specific versions of object."
  (let* ((obj (or obj objed--object))
         (objf (objed--name2func obj no-mode))
         ;; for calling the object func
         (inhibit-message t)
         (overriding-terminal-local-map nil)
         (res (objed--handle-query query objf)))
    (if (keywordp res)
        ;; basic inheritence...
        (objed--object res obj t)
      res)))

(defun objed--handle-query (query objf)
  "Hand query QUERY for object function OBJF."
  (cond ((memq query '(:try-next :try-prev))
         (condition-case nil
             (funcall objf query)
           ((search-failed end-of-buffer beginning-of-buffer scan-error)
            (error "No %s %s found"
                   (if (eq query :try-next) "next" "previous")
                   objed--object))))
        (t
         (funcall objf query))))

(defun objed--next-visible-point (&optional pos)
  "Get next visible position.

Should start from an invisible position POS. POS defaults to
point."
  (next-single-char-property-change
   (or pos (point)) 'invisible))

(defun objed--prev-visible-point (&optional pos)
  "Get previous visible position.

Should start from an invisible position POS. POS defaults to
point."
  (previous-single-char-property-change
   (or pos (point)) 'invisible))

(defun objed--invisible-p (&optional pos)
  "Check if point is at an invisible position.

Position POS defaults to point."
  (cl-dolist (ol (overlays-at (or pos (point))))
    ;; see, reveal.el
    ;; overlay alive
    (when (overlay-start ol)
      (let ((inv (overlay-get ol 'invisible)))
        (when (and inv
                   ;; make sure it's actually invisible
                   (consp buffer-invisibility-spec)
                   (cdr (assq inv buffer-invisibility-spec)))
          (cl-return t))))))

;; * Public access functions for objects

(defun objed-bounds-at-point (obj &optional state)
  "Return beg and end position of object at point.

The positions are returned as a cons: (beg . end). OBJ is a
symbol of a known object. STATE is either `whole' or `inner' and
defaults to `whole'.

Does return nil when there is no such object at point."
  (let ((o (objed--object-at-point obj state)))
    (when o
      (cons (objed--beg o)
            (objed--end o)))))


(defun objed-bounds-at (pos obj &optional state)
  "Return beg and end position of object at POS.

The positions are returned as a cons: (beg . end). OBJ is a
symbol of a known object. STATE is either `whole' or `inner' and
defaults to `whole'.

Does return nil when there is no such object at point."
  (save-excursion
    (goto-char pos)
    (objed-bounds-at-point obj state)))


;; * Object creation/manipulation

(defun objed-make-empty-object (&optional pos)
  "Return an empty object at POS which default to point."
  (let ((pos (or pos (point))))
    (list (list pos pos)
          (list pos pos))))


(cl-defun objed-make-object (&key obounds beg end ibounds ibeg iend)
  "Helper to create internal used object format from positions.

Positions of the whole object can be provided by BEG, END or a
cons cell OBOUNDS.

The positions of the inner part can be provided by IBEG, IEND or
a cons cell IBOUNDS. If inner positions are omitted
`objed--inner-default' is used to determine them."
  (cl-assert (and (not (and obounds beg end))
                  (not (and ibounds ibeg iend))))
  ;; return nil
  (when (or obounds
            (and beg end
                 (not (= beg end))))
    (cond ((and (integer-or-marker-p beg)
                (integer-or-marker-p end)
                (integer-or-marker-p ibeg)
                (integer-or-marker-p iend))
           (list (list (objed--pos-or-marker beg)
                       (objed--pos-or-marker end))
                 (list (objed--pos-or-marker ibeg)
                       (objed--pos-or-marker iend))))
          ((and (integer-or-marker-p beg)
                (integer-or-marker-p end))
           (cond ((consp ibounds)
                  (list (list (objed--pos-or-marker beg)
                              (objed--pos-or-marker end))
                        (list (objed--pos-or-marker (car ibounds))
                              (objed--pos-or-marker (cdr ibounds)))))
                 ((or (functionp ibeg)
                      (functionp iend))
                  (list (list (objed--pos-or-marker beg)
                              (objed--pos-or-marker end))
                        (list (objed--pos-or-marker
                               (or (and (functionp ibeg)
                                        (funcall ibeg beg))
                                   ibeg))
                              (objed--pos-or-marker
                               (or (and (functionp iend)
                                        (funcall iend end))
                                   iend)))))
                 (t
                  (list (list (objed--pos-or-marker beg)
                              (objed--pos-or-marker end))
                        (objed--inner-default beg end)))))
          ((consp obounds)
           (cond ((consp ibounds)
                  (list (list (objed--pos-or-marker (car obounds))
                              (objed--pos-or-marker (cdr obounds)))
                        (list (objed--pos-or-marker (car ibounds))
                              (objed--pos-or-marker (cdr ibounds)))))
                 ((and (integer-or-marker-p ibeg)
                       (integer-or-marker-p iend))
                  (list (list (objed--pos-or-marker (car obounds))
                              (objed--pos-or-marker (cdr obounds)))
                        (list (objed--pos-or-marker ibeg)
                              (objed--pos-or-marker iend))))
                 ((or (functionp ibeg)
                      (functionp iend))
                  (list (list (objed--pos-or-marker (car obounds))
                              (objed--pos-or-marker (cdr obounds)))
                        (list (objed--pos-or-marker
                               (or (and (functionp ibeg)
                                        (funcall ibeg beg))
                                   ibeg))
                              (objed--pos-or-marker
                               (or (and (functionp iend)
                                        (funcall iend end))
                                   iend)))))
                 (t
                  (list (list (objed--pos-or-marker (car obounds))
                              (objed--pos-or-marker (cdr obounds)))
                        (objed--inner-default  (car obounds) (cdr obounds)))))))))


(cl-defun objed--change-to (&key beg end ibeg iend)
  "Change position data of current object.

BEG: the beginning position
END: the end position
IBEG: the beginning position of the inner part
IEND: the end position of the inner part"
  (let ((beg (and beg (objed--pos-or-marker beg)))
        (end (and end (objed--pos-or-marker end)))
        (ibeg (and ibeg (objed--pos-or-marker ibeg)))
        (iend (and iend (objed--pos-or-marker iend))))
    (cond ((eq objed--obj-state 'whole)
           (when beg
             (setf (car (car objed--current-obj)) beg))
           (when end
             (setf (car (cdar objed--current-obj)) end))
           (when ibeg
             (setf (car (cadr objed--current-obj)) ibeg))
           (when iend
             (setf (cadr (cadr objed--current-obj)) iend)))
          ((eq objed--obj-state 'inner)
           (when ibeg
             (setf (car (car objed--current-obj)) ibeg))
           (when iend
             (setf (car (cdar objed--current-obj)) iend))
           (when beg
             (setf (car (cadr objed--current-obj)) beg))
           (when end
             (setf (cadr (cadr objed--current-obj)) end)))
          (t
           (error "No valid `objed--obj-state'")))))


;; * Helpers to work with object format


(defun objed--update-current-object (&optional range)
  "Update positions of current object.

Update `objed--current-obj' to RANGE which defaults to object at
point. If RANGE is a single item list only update the head of
current object position data."
  (cond ((null range)
         (unless objed--object
           (setq objed--object 'char))
         ;; get current object at point
         (setq objed--current-obj (objed--get)))
        ((and (consp range)
              (not (consp (cdr range))))
         (error "Wrong format for object data"))
        (t
         (setq objed--current-obj range))))


(defun objed--markify-current-object ()
  "Convert current object into marker object."
  (unless (markerp (objed--beg))
    (objed--update-current-object
     (objed-make-object
      :ibeg (set-marker (make-marker)
                        (objed--ibeg))
      :beg (set-marker (make-marker)
                       (objed--obeg))
      :iend (set-marker (make-marker)
                        (objed--iend))
      :end (set-marker (make-marker)
                       (objed--oend))))))


(defun objed--switch-to (o &optional state odata)
  "Switch to object O.

STATE is the state for the object and defaults to whole. If ODATA
is non-nil it is used as object position data, otherwise
calculate the data of the object at current position using
`objed--get'."
  (let ((odata (let* ((objed--object o)
                      (objed--obj-state (or state 'whole))
                      (tryb t))
                 (or odata
                     ;; FIXME: all default objects should throw an error
                     ;; if try-next, try-prev fails.
                     (condition-case nil
                         (or (objed--get)
                             (setq tryb nil)
                             (objed--get t))
                       (error
                        (when tryb
                          (ignore-errors (objed--get t)))))))))
    (if odata
        (setq objed--object o
              objed--obj-state (or state 'whole)
              objed--current-obj odata)
      (prog1 nil
        (message "No %s found." o)))))

(defun objed--get-next (&optional from)
  "Get next object from position or object.

If FROM is a position search from there otherwise search starts
from end of object FROM."
  (let ((obj (or from objed--current-obj)))
    (save-excursion
      (when (and obj (not (objed--no-skipper-p)))
        (if (integer-or-marker-p obj)
            (goto-char obj)
          (goto-char (objed--max obj))))
      (unless (eobp)
        (when (and (not (number-or-marker-p from))
                   (<= (point) (objed--beg obj)))
            (objed--skip-ws))
        (ignore-errors
          (objed--object :try-next)
          (objed--get))))))

(defun objed--get-prev (&optional from)
  "Get previous object from position or object.

If FROM is a position search from there otherwise search starts
from beginning of object FROM."
  (let ((obj (or from objed--current-obj)))
    (save-excursion
      (when obj
        (if (integer-or-marker-p obj)
            (goto-char obj)
          (goto-char (objed--min obj))))
      (unless (bobp)
        (ignore-errors
        (objed--object :try-prev)
        (objed--get t))))))


(defun objed--distant-p (o)
  "Determine if point is outside object O."
  (and o
       (not (<= (objed--min o)
                (point) (objed--max o)))))

(defun objed--do-all (f)
  "Apply function F to all accessible objects.

F recieves object range as its argument which is determined
according to `objed--obj-state'."
  (let ((obj nil)
        (sobj nil)
        (inhibit-message t)
        (n 1))
    ;; current one
    (funcall f objed--current-obj)
    ;; backward
    (goto-char (objed--min))
    (setq sobj objed--current-obj)
    (while (and (> (point) (point-min))
                (setq obj (objed--get-prev (point)))
                (not (equal obj sobj)))
      (goto-char (objed--min obj))
      (funcall f obj)
      (cl-incf n)
      (setq sobj obj)
      (goto-char (objed--min obj)))
    ;; FIXME
    (when objed--marked-ovs
      (setq objed--marked-ovs
            (nreverse objed--marked-ovs)))
    ;; forward
    (goto-char (objed--max))
    (setq sobj objed--current-obj)
    (while (and (< (point) (point-max))
                (setq obj (objed--get-next (point)))
                (not (equal obj sobj)))
      (funcall f obj)
      (cl-incf n)
      (setq sobj obj)
      (goto-char (objed--max obj)))
    n))


(defun objed--in-p (c &optional inner)
  "Return object name point is in.

C is a list of object names to test for. If INNER is given check
object data for inner state."
  (let ((o nil))
    (cl-dolist (cand c)
      (when (and (setq o (objed--object :get-obj cand))
                 (if inner (setq o (nreverse o)) o)
                 (<= (objed--beg o)
                     (point)
                     (objed--end o)))
        (cl-return cand)))))

(defun objed--at-p (c)
  "Return object name at point.

C is a list of object names to test for."
  (cl-dolist (cand c)
    (when (objed--object :atp cand)
      (cl-return cand))))

(defun objed--indentation-position ()
  "Get buffer position of indentation on current line."
  (save-excursion
    (back-to-indentation)
    (point)))

(defun objed--apply (func &optional obj)
  "Apply function FUNC on postions of object.

OBJ defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (when obj
      (apply func
             (append (car obj)
                     (cadr obj))))))


(defun objed--merge (obj1 obj2)
  "Merge objects OBJ1 and OBJ2 to build a new object."
  (objed-make-object :beg (objed--min obj1)
                     :end (objed--max obj2)))


(defun objed--reverse ()
  "Exchange current objects head and tail."
  (if (objed--inner-p)
      (setq objed--obj-state 'whole)
    (setq objed--obj-state 'inner))
  (objed--update-current-object
   (nreverse objed--current-obj)))


(defun objed--inner-p ()
  "Return non-nil if current objects state is inner."
  (eq objed--obj-state 'inner))

(defun objed--goto-next (&optional arg)
  "Move to the next object.

With postitive prefix argument ARG move to the nth next object."
  (let ((arg (or arg 1))
        (obj nil))
    (dotimes (_ arg obj)
      (when (setq obj  (objed--get-next))
        (objed--update-current-object obj)
        (objed--goto-char (objed--beg obj))))))


(defun objed--goto-previous (&optional arg)
  "Move to the previous object.

With postitive prefix argument ARG move to the nth previous
object."
  (let ((arg (or arg 1))
        (obj nil))
    (dotimes (_ arg obj)
      (when (setq obj (objed--get-prev))
        (objed--update-current-object obj)
        (objed--goto-char (objed--beg obj))))))

(defun objed--make-object-overlay (&optional obj)
  "Create an overlay to mark current object.

OBJ is the object to use and defaults to `objed--current-obj'."
  (let ((obj (or obj objed--current-obj)))
    (objed--make-mark-overlay (objed--beg obj)
                              (objed--end obj))))


(defun objed--make-mark-overlay (beg end &optional face keep)
  "Make an objed overaly over region between BEG, END.

Uses FACE `objed-mark' by default. If KEEP is non-nil keep
overlays without content."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'objed t)
    (overlay-put ov 'objed--object objed--object)
    (overlay-put ov 'face (or face 'objed-mark))
    (overlay-put ov 'evaporate (not keep))
    (overlay-put ov 'rear-nonsticky t)
    ov))

(defun objed--mark (beg end)
  "Mark region between BEG and END."
  (push (objed--make-mark-overlay beg end)
        objed--marked-ovs))

(defun objed--mark-object (&optional obj append)
  "Mark current object.

If OBJ is given use it instead `objed--current-obj' If APPEND is
non-nil append to the list of marked objects."
  (let ((obj (or obj objed--current-obj)))
    (if append
        (setq objed--marked-ovs
              (append objed--marked-ovs
                      (list (objed--make-object-overlay obj))))
      (push (objed--make-object-overlay obj)
            objed--marked-ovs))))

(defun objed-unmark-all ()
  "Remove all marked objects."
  (interactive)
  (while objed--marked-ovs
    (delete-overlay (pop objed--marked-ovs))))

(defun objed--mark-ovps (ovsps)
  "Mark objects using position data OVSPS."
  (dolist (el ovsps)
    (objed--mark (car el) (cdr el))))

(defun objed--toggle-mark (&optional obj append)
  "Unmark/Mark object.

If OBJ is given use it instead `objed--current-obj' If APPEND is
non-nil append to list of marked objects when marking."
  (let ((obj (or obj objed--current-obj)))
    (unless (objed--unmark-object obj)
      (objed--mark-object obj append))))

(defun objed--unmark-object (&optional obj)
  "Unmark objects within current object.

If OBJ is given use it instead `objed--current-obj'."
  (let ((obj (or obj objed--current-obj))
        (unmarked nil))
    (cl-dolist (ov (overlays-in (objed--beg obj)
                                (objed--end obj))
                   unmarked)
      (when (overlay-get ov 'objed)
        (setq unmarked t)
        (setq objed--marked-ovs
              (delq ov objed--marked-ovs))
        (delete-overlay ov)))
    unmarked))


;; * Creating objects


(defvar objed--block-p nil
  "Block advices installed by `objed'.")


(defun objed-bounds-from-region-cmd (cmd)
  "Return buffer positions of region created by command CMD.

Like for `bounds-of-thing-at-point' the positions are returned as
a cons cell."
  (let ((mark-active nil)
        (last-command nil)
        (inhibit-message t)
        (objed--block-p t)
        (message-log-max nil)
        (current-prefix-arg nil))
    (save-mark-and-excursion
     (goto-char (line-beginning-position))
     (objed--skip-ws)
     (funcall-interactively cmd)
     (cons (region-beginning) (region-end)))))

(defun objed--inner-default (beg end)
  "Return positions for inner range.

BEG and END are the positions of the whole object.

Leading and trailing whitespace is skipped to determine the inner
positions."
  (let ((ibeg (objed--skip-forward beg 'ws))
        (iend (objed--skip-backward end 'ws)))
    (if (<= beg ibeg iend end)
        (list (objed--pos-or-marker ibeg)
              (objed--pos-or-marker iend))
      ;; fallback
      (list (objed--pos-or-marker (point))
            (objed--pos-or-marker (1+ (point)))))))


(defun objed--pos-or-marker (pos)
  "Return marker or position POS.

If `multiple-cursors-mode' is non-nil return marker for
position POS, otherwise just return POS."
  (if (and (bound-and-true-p multiple-cursors-mode)
           (not (markerp pos)))
      (set-marker (make-marker) pos)
    pos))

;; * Object definition helpers

(defun objed--in-string-p (&optional syn ignore-atp)
  "Return non-nil if point is inside or at string.

If SYN is given use it instead of syntax at point.

If IGNORE-ATP is non-nil dont test if point is at a string
only if its withing one."
  (let ((syn (or syn (syntax-ppss))))
    (if (and (nth 3 syn)
             (nth 8 syn))
        (nth 8 syn)
      (and (not ignore-atp)
           (objed--at-string-p)))))

(defun objed--at-string-p ()
  "Return non-nil if point is at string."
  (let ((syn nil))
    (and (not (nth 4 (syntax-ppss)))
         (or (and (not (eobp))
                  (eq ?\" (char-syntax (char-after)))
                  (save-excursion
                    (nth 3 (setq syn (syntax-ppss (1+ (point))))))
                  (nth 8 syn))
             (and (not (bobp))
                  (eq ?\" (char-syntax (char-before)))
                  (save-excursion
                    (nth 3 (setq syn (syntax-ppss (1- (point))))))
                  (nth 8 syn))))))

(defun objed--inner-string (&optional bounds)
  "Return inners bounds of string at point.

If BOUNDS is given use it instead of string at point."
  (let ((bounds (or bounds (objed--bounds-of-string-at-point))))
    (when bounds
      (let ((beg (save-excursion (goto-char (car bounds))
                                 (while (eq ?\" (char-syntax (char-after)))
                                   (forward-char 1))
                                 (when (< (point) (cdr bounds))
                                   (point))))
            (end (save-excursion (goto-char (cdr bounds))
                                 (while (eq ?\" (char-syntax (char-before)))
                                   (forward-char -1))
                                 (when (> (point) (car bounds))
                                   (point)))))
        (when (and beg end)
          (cons beg end))))))

(defun objed--in-comment-p (&optional syn)
  "Return non-nil when in point is in or at comment.

If SYN is given use it instead of syntax at point."
  (let ((syn (or syn (syntax-ppss))))
    (or (and (nth 4 syn)
             (nth 8 syn))
        (objed--at-comment-p))))

(defun objed--at-comment-p ()
  "Return non-nil if point is at comment."
  (let ((syn nil))
    (or (save-excursion
          (and (not (eobp))
               (char-after)
               (eq ?\< (char-syntax (char-after)))
               (skip-syntax-forward "<")
               (nth 4 (setq syn (syntax-ppss)))
               (nth 8 syn)))
        (save-excursion
          (and (not (bobp))
               (char-after)
               (eq ?\> (char-syntax (char-after)))
               (prog1 t (unless (bolp) (skip-syntax-backward ">")))
               (nth 4 (setq syn (syntax-ppss)))
               (nth 8 syn))))))

(defun objed--bounds-of-comment-at-point ()
  "Return bounds of comment at point."
  (let ((beg (objed--in-comment-p)))
    (when beg
      (cons beg
            (save-excursion (goto-char beg)
                            (forward-comment 1)
                            (objed--skip-ws t)
                            (point))))))

(defun objed--bounds-of-string-at-point ()
  "Return bounds of string at point."
  (let* ((beg (objed--in-string-p))
         (end (and beg (save-excursion
                         (goto-char beg)
                        (when (ignore-errors (forward-sexp 1) t)
                          (point))))))
    (when (and beg end)
      (cons beg
            (save-excursion (goto-char beg)
                            (forward-sexp 1)
                            (point))))))

(defun objed--comment-block ()
  "Return bounds of comment block at point if point in comment."
  (let ((start (objed--in-comment-p)))
    (when start
      (save-excursion
        (goto-char start)
        (let ((beg (objed--skip-backward (point) nil 'comment))
              (end (objed--skip-forward (point) nil 'comment)))
          (goto-char end)
          (skip-chars-forward " \t\r\n" (line-beginning-position 2))
          (cons beg (point)))))))

(defun objed--inner-comment-block ()
  "Get range of inner comment."
  (let ((bounds (objed--comment-block)))
    (cons (objed--skip-forward (car bounds)'ws)
          (objed--skip-backward (cdr bounds)'ws))))

(defun objed--skip-ws (&optional back limit)
  "Skip whitspace.

Defaults to forward, if BACK is non-nil skip backwards.
Skips until LIMIT."
  (if back
      (skip-chars-backward " \r\n\t" limit)
    (skip-chars-forward " \r\n\t" limit)))


(defun objed--skip-forward (from &optional ws comment)
 "Skip whitespace or comment forward from position FROM.

If WS is given skip whitespace. COMMENT non-nil to skip
comments."
  (save-excursion
    (goto-char from)
    (cond (ws
           (objed--skip-ws)
           (if comment
               (objed--skip-forward (point) nil 'comment)
             (point)))
          (comment
           (let ((s (objed--in-comment-p)))
             (when s
               (goto-char s))
             (while (and (not (eobp))
                         (forward-comment 1)))
             (objed--skip-ws t)
             (point))))))

(defun objed--skip-backward (from &optional ws comment)
  "Skip whitespace or comment backward from position FROM.

If WS is given skip whitespace. COMMENT non-nil to skip
comments."
  (save-excursion
    (goto-char from)
    (cond (ws
           (objed--skip-ws t)
           (if comment
               (objed--skip-backward (point) nil 'comment)
             (point)))
          (comment
           (let ((s (objed--in-comment-p)))
             (when s
               (goto-char s))
             (while (and (not (bobp))
                         (forward-comment -1)))
             (objed--skip-ws)
             (point))))))

(defun objed--narrow-if-string-or-comment ()
  "In comment or string narrow to them."
  (let* ((bounds nil)
         (ibounds (cond ((setq bounds (objed--bounds-of-string-at-point))
                         (objed--inner-string bounds))
                        ((setq bounds (objed--bounds-of-comment-at-point))
                         ;; include trailing ws
                         (objed--comment-block)))))
    (when ibounds
      (narrow-to-region (car ibounds) (cdr ibounds)))))


(defun objed--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (begin (nth 8 sp)))
    (when (or (eq (char-after begin) ?\")
              (nth 4 sp))
      begin)))

(defvar align-region-separate)
(defvar align-mode-rules-list)
(defvar align-rules-list)
(defvar align-exclude-rules-list)
(defvar align-mode-exclude-rules-list)
(declare-function align-region "ext:align")
(defun objed--get-align-sections ()
  "Get region bounds of align sections."
  (require 'align)
  (let ((separator
         (or (if (and (symbolp align-region-separate)
                      (boundp align-region-separate))
                 (symbol-value align-region-separate)
               align-region-separate)
             'entire))
        (regions ()))
    (align-region nil nil separator
                  (or align-mode-rules-list align-rules-list)
                  (or align-mode-exclude-rules-list align-exclude-rules-list)
                  (lambda (beg end mode)
                    (when (consp mode)
                      (push (cons beg end)
                            regions))))
    regions))


;; * Object definitions


(objed-define-object nil char
  :atp
  (looking-at ".")
  :get-obj
  (if (eobp)
      (objed-make-empty-object)
    (objed-make-object :beg (point)
                       :end (1+ (point))))
  :try-next
  ;; current one is skipped, for chars this means we are already at
  ;; the next..
  (forward-char 0)
  :try-prev
  (forward-char -1))

(objed-define-object nil ace
  :get-obj
  (unless (eq objed--object 'ace-object)
    (let ((stripe (and (bound-and-true-p stripe-buffer-mode)
                       stripe-buffer-mode)))
      (when (fboundp 'stripe-buffer-mode)
        (stripe-buffer-mode 1))
      (unwind-protect
          (objed-make-object
           :beg (save-excursion
                  (call-interactively 'avy-goto-line)
                  ;; indicate input
                  (redisplay)
                  (line-beginning-position))
           :end (save-excursion
                  (when (fboundp 'stripe-buffer-mode)
                    (stripe-buffer-mode 1))
                  (call-interactively 'avy-goto-line)
                  (1+ (line-end-position))))
        (unless (or stripe
                    (not (fboundp 'stripe-buffer-mode)))
          (stripe-buffer-mode -1)))))
  :try-next
  (user-error "Not possible")
  :try-prev
  (user-error "Not possible"))

(objed-define-object nil trailing
  :atp
  (looking-at " ")
  :get-obj
  (objed-make-object
   :beg (point)
   :ibeg (point)
   :end (progn (goto-char (objed--end))
               (objed--skip-ws) (point))
   :iend (point))
  :try-next
  (when (search-forward " " nil t)
    (forward-char -1))
  :try-prev
  (search-backward " " nil t))


(objed-define-object nil leading
  :atp
  (looking-back " " (1- (point)))
  :get-obj
  (objed-make-object
   :end  (point)
   :iend (point)
   :beg (progn (goto-char (objed--beg))
               (objed--skip-ws t) (point))
   :ibeg (point))
  :try-next
  (when (search-forward " " nil t)
    (forward-char -1))
  :try-prev
  (search-backward " " nil t))


(defun objed--inner-word-bounds ()
  "Return bounds of subword at point."
  (let* ((subword-mode t)
         (superword-mode nil)
         (find-word-boundary-function-table
          subword-find-word-boundary-function-table))
    (if (eq this-command 'forward-word)
        (save-excursion
          (forward-word -1)
          (bounds-of-thing-at-point 'word))
      (bounds-of-thing-at-point 'word))))


(objed-define-object nil word
  :atp
  (looking-at "\\<")
  :ref
  (when (equal (bounds-of-thing-at-point 'word)
               (bounds-of-thing-at-point 'symbol))
    'identifier)
  :get-obj
  (if (eobp)
      (objed-make-empty-object)
    (if (objed--inner-p)
        ;; don't confuse objed-next/prev which
        ;; use the outer bounds for navigation
        ;; but a word can contain multiple innner words
        (objed-make-object
         :obounds (objed--inner-word-bounds))
      (objed-make-object
         :obounds (bounds-of-thing-at-point 'word)
         :ibounds (objed--inner-word-bounds))))
  :try-next
  (if (objed--inner-p)
      (let* ((subword-mode t)
             (superword-mode nil)
             (find-word-boundary-function-table
              subword-find-word-boundary-function-table))
        (forward-word 1)
        (forward-word -1))
    (re-search-forward  "\\<." nil t))
  :try-prev
  (if (objed--inner-p)
      (let* ((subword-mode t)
             (superword-mode nil)
             (find-word-boundary-function-table
              subword-find-word-boundary-function-table))
        (forward-word -1))
    (re-search-backward  ".\\>" nil t)))


(defun objed--next-symbol ()
  "Move to next symbol."
  (re-search-forward  "\\_<" nil t)
  (let (syn start)
    (while (setq start (nth 8 (setq syn (syntax-ppss))))
      (goto-char start)
      (when (cond ((nth 3 syn)
                   (forward-sexp 1) t)
                  ((nth 4 syn)
                   (forward-comment 1) t))
        (re-search-forward  "\\_<." nil t)))))


(defun objed--prev-symbol ()
  "Move to previous symbol."
  (re-search-backward  "\\_>" nil t)
  (let (start)
    (while (setq start (nth 8 (syntax-ppss)))
      (goto-char start)
      (re-search-backward  "\\_>" nil t))))


(objed-define-object nil symbol
  :atp
  (and (not (objed--in-string-or-comment-p))
       (or (looking-at "\\_<")
           (looking-back "\\_>" 1)))
  :ref 'identifier
  :get-obj
  (when (not (objed--in-string-or-comment-p))
    (bounds-of-thing-at-point 'symbol))
  :try-next
  (objed--next-symbol)
  :try-prev
  (objed--prev-symbol))


(objed-define-object nil subword
  :get-obj
  (objed--inner-word-bounds)
  :try-next
  (let* ((subword-mode t)
         (superword-mode nil)
         (find-word-boundary-function-table
          subword-find-word-boundary-function-table))
    (forward-word 1)
    (forward-word -1))
  :try-prev
  (let* ((subword-mode t)
         (superword-mode nil)
         (find-word-boundary-function-table
          subword-find-word-boundary-function-table))
    (forward-word -1)))



(defun objed--at-sexp-p ()
  "Return sexp object if point at strutured expression."
  (let* ((opos (point))
         (objed--block-p t)
         (real-this-command 'forward-sexp)
         (instring (objed--in-string-p nil t))
         (other nil)
         (atp (or (when (or (bobp)
                            ;; prevent the annoying "feature" that sexp
                            ;; movement works across strings
                            (not instring)
                            (not (eq (char-syntax (char-after)) ?\")))
                    (save-excursion
                      (ignore-errors
                        (forward-sexp 1)
                        (setq other (point))
                        (forward-sexp -1)
                        (= (point) opos))))
                  (when (or (eobp)
                            ;; prevent the annoying "feature" that sexp
                            ;; movement works across strings
                            (not instring)
                            (not (eq (char-syntax (char-before)) ?\")))
                    (save-excursion
                         (ignore-errors
                           (forward-sexp -1)
                           (setq other (point))
                           (forward-sexp 1)
                           (= (point) opos)))))))

    (when atp
      (cons (min opos other)
            (max opos other)))))


(objed-define-object nil sexp
  :atp
  (objed--at-sexp-p)
  :ref
  (let ((s (save-excursion
             (goto-char (objed--beg))
             (skip-syntax-forward ".'")
             (bounds-of-thing-at-point 'symbol))))
    (when (and s
               (or (equal s (cons (objed--beg)
                                  (objed--end)))
                   (equal s (cons (objed--alt-beg)
                                  (objed--alt-end)))))
      (goto-char (car s))
      'identifier))
  :get-obj
  (let ((bounds (or (objed--at-sexp-p)
                    ;; for commands which are not symetric
                    ;; like C-M-f at beg of python funtions
                    (save-excursion
                      (ignore-errors
                        (let* ((pos (point))
                               (real-this-command 'forward-sexp))
                          (forward-sexp 1)
                          (when (/= pos (point))
                            (cons pos
                                  (point)))))))))
    (when bounds
      (objed-make-object
       :obounds bounds
       :ibeg (1+ (car bounds))
       :iend (1- (cdr bounds)))))
  :try-next
  (or (ignore-errors
        (forward-sexp 1)
        (forward-sexp -1) t)
      (ignore-errors
        (up-list 1)
        t)
      (ignore-errors
        (forward-word 1)
        (forward-sexp -1)
        t))
  :try-prev
  (or (ignore-errors
        (forward-sexp -1) t)
      (ignore-errors
        (up-list -1)
        t)
       (ignore-errors
         (forward-word -1)
         t)))


(objed-define-object nil file
  :atp
  (looking-at "/\\|\\\\")
  :get-obj
  (let* ((bounds (bounds-of-thing-at-point 'filename))
         (file (and bounds (buffer-substring (car bounds) (cdr bounds)))))
    (when (and file (string-match (rx (or (and bos (or "/" "\\"))
                                          (and "." (* alnum) eos)))
                                  file))
      (objed-make-object :obounds bounds
                         :ibounds
                         (let ((ifile (or (file-name-directory file)
                                          (file-name-sans-extension file))))
                           (when ifile
                             (goto-char (car bounds))
                             (search-forward ifile)
                             (cons (match-beginning 0) (match-end 0)))))))
  :try-next
  (re-search-forward  (rx (or (or "/" "\\")
                              (and "." (* alnum))))
                      nil t)
  :try-prev
  (re-search-backward  (rx (or (or "/" "\\")
                               (and "." (* alnum))))
                       nil t))

(objed-define-object nil email
  :get-obj
  (bounds-of-thing-at-point 'email)
  :try-next
  (re-search-forward "[a-z0-9!#$%&'*+/=?^_`{|}~-]@[a-z0-9]")
  :try-prev
  (re-search-backward "[a-z0-9!#$%&'*+/=?^_`{|}~-]@[a-z0-9]"))

(objed-define-object nil url
  :get-obj
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (when bounds
      (objed-make-object :obounds bounds
                         :ibounds
                         (progn
                           (goto-char (car bounds))
                           (re-search-forward "https?://" nil t)
                           (cons (point)
                                 (if (search-forward "/" (cdr bounds) t)
                                     (1- (point))
                                   (cdr bounds)))))))
  :try-next
  (re-search-forward "http")
  :try-prev
  (re-search-backward "http"))

(objed-define-object nil page
  :atp
  (looking-at page-delimiter)
  :get-obj
  (bounds-of-thing-at-point 'page)
  :try-next
  (forward-line 1)
  :try-prev
  (forward-line -1))

(objed-define-object nil defun
  :get-obj
  (objed-bounds-from-region-cmd #'mark-defun)
  :try-next
  (beginning-of-defun -1)
  :try-prev
  (beginning-of-defun 1))

(objed-define-object nil line
  :atp
  (or (looking-at "^")
      (looking-back "^ *" (line-beginning-position)))
  :get-obj
  (if (eobp)
      (objed-make-empty-object)
    (objed-make-object :beg (line-beginning-position)
                       :end (save-excursion
                            ;; include hidden parts...
                              (end-of-visible-line)
                              (if (eobp)
                                  (point)
                                (1+ (point))))))
  :try-next
  (skip-chars-forward " \t\r\n")
  :try-prev
  (skip-chars-backward " \t\r\n"))



(objed-define-object nil textblock
  :max-search-forward
  (save-excursion
    (objed--with-narrow-for-text
     (forward-paragraph 1)
     (point)))
  :get-obj
  (when (or (not (derived-mode-p 'prog-mode))
            (derived-mode-p 'text-mode)
            (objed--in-comment-p)
            (objed--in-string-p))
    (objed--with-narrow-for-text
     (let ((bounds (objed--get-textblock-bounds)))
       (when bounds
         (objed-make-object :obounds bounds)))))
  :try-next
  (objed--with-narrow-for-text
   (forward-sentence 1))
  :try-prev
  (objed--with-narrow-for-text
   (forward-sentence -1)))


(defun objed--column (pos)
  "Get column at position POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun objed--get-indent-bounds (&optional inner block)
  "Get range of indentation block.

If INNER is non-nil get the range for inner state. If BLOCK is
non-nil the indentation block can contain empty lines."
  (let* ((oi (objed--indentation-position))
         (ic (objed--column oi))
         (pos nil)
         (opos nil)
         (beg (save-excursion
               (while (and (not (bobp))
                           (progn
                             (forward-line -1)
                             (or  (and block (looking-at  "^ *$"))
                                  (and (= (objed--column (setq pos (objed--indentation-position)))
                                          ic)
                                       (or (not inner) (not (looking-at " *$")))
                                       ;; dont accept empty
                                       (or (not (eolp)) (not (bolp)))
                                       (setq opos pos))))))
               (prog1 (or opos oi)
                 (setq opos nil))))
        (end (save-excursion
               (save-excursion
                 (while (and (not (eobp))
                             (progn
                               (forward-line 1)
                               (or (and block (looking-at  "^ *$"))
                                   (and (= (objed--column (setq pos (objed--indentation-position)))
                                           ic)
                                        (or (not inner) (not (looking-at " *$")))
                                        (or (not (eolp)) (not (bolp)))
                                        (setq opos pos)))))))
               (when opos
                 (goto-char opos))
               (line-end-position))))
    (when (and beg end)
      (save-excursion
        (goto-char beg)
        (cons (line-beginning-position)
               end)))))


(objed-define-object nil indent
  :get-obj
  (let ((bounds (objed--get-indent-bounds 'inner)))
    (when bounds
      (objed-make-object
       :obounds
       (cons (car bounds)
             (if (eq (cdr bounds)
                     (point-max))
                 (cdr bounds)
               (1+ (cdr bounds)))))))
  :try-next
  ;;(error "No next indent")
  (objed--skip-ws)
  :try-prev
  (objed--skip-ws t))



(objed-define-object nil block
  :get-obj
  (let ((bounds (objed--get-indent-bounds nil t)))
    (when bounds
      (objed-make-object
       :obounds
       (cons (car bounds)
             (if (eq (cdr bounds)
                     (point-max))
                 (cdr bounds)
               (1+ (cdr bounds)))))))
  :try-next
  (objed--skip-ws)
  :try-prev
  (objed--skip-ws t))


(objed-define-object nil sentence
  :atp
  (when (or (derived-mode-p 'text-mode)
            (eq major-mode 'fundamental-mode)
            (objed--at-comment-p)
            (objed--in-string-or-comment-p))
    (let ((ip (ignore-errors
                (save-excursion
                  (forward-sentence)
                  (backward-sentence)
                  (point)))))
      (and ip (= (point) ip))))
  :max-search-forward
  (save-excursion
    (objed--with-narrow-for-text
     (forward-paragraph 1)
     (point)))
  :get-obj
  (when (or (derived-mode-p 'text-mode)
            (eq major-mode 'fundamental-mode)
            (objed--at-comment-p)
            (objed--in-string-or-comment-p))
    (objed--with-narrow-for-text
     (let ((s (bounds-of-thing-at-point 'sentence)))
       (when s
         (goto-char (car s))
         (objed-make-object
          :beg (if (derived-mode-p 'text-mode)
                   (car s)
                 (while (or (< 0 (skip-syntax-forward "<"))
                            (< 0 (skip-chars-forward "[:space:]\n"))))
                 (point))
          :ibeg (point)
          :end (progn
                 (goto-char (cdr s))
                 ;; include trailing space
                 (skip-chars-forward
                  "\s-"
                  (+ (point)
                     (if sentence-end-double-space
                         2
                       1)))
                 (point))
          :iend (with-syntax-table text-mode-syntax-table
                  (skip-syntax-backward ".-" (car s))
                  (point)))))))
  :try-next
  (objed--with-narrow-for-text
   ;; call twice if at start of sentence.
    (unless (>= (point)
                (save-excursion (backward-sentence)
                                (forward-sentence)
                                (point)))
      (forward-sentence 1))
    (forward-sentence 1)
    ;; move to start
    (forward-sentence -1))
  :try-prev
  (objed--with-narrow-for-text
   ;; call twice at end of sentence
    (unless (<= (point)
                (save-excursion (forward-sentence)
                                (backward-sentence)
                                (point)))
      (backward-sentence 1))
    (backward-sentence 1)))


(objed-define-object nil paragraph
  :atp
  (and (or (bobp)
           (save-excursion (forward-line -1)
                           (looking-at "^ *$")))
       (looking-back "^ *" (line-beginning-position)))
  :get-obj
  (objed-bounds-from-region-cmd #'mark-paragraph)
  :try-next
  (skip-chars-forward " \t\r\n")
  :try-prev
  (skip-chars-backward " \t\r\n"))



(defun objed--bounds-within-comment-or-string-p (bounds)
  "Return non-nil if BOUNDS are inside comment or string."
  (let ((cbounds (cond ((objed--in-string-p)
                        (objed--bounds-of-string-at-point))
                       ((objed--in-comment-p)
                        (objed--comment-block)))))
    (when cbounds
      (objed--bounds-within-bounds-p bounds cbounds))))

(defun objed--bounds-within-bounds-p (b c)
  "Return non-nil if bounds B are inside C."
  (and (>= (car b) (car c))
       (<= (cdr b) (cdr c))))


(defun objed--get-textblock-bounds ()
  "Get cons cell of beginnine and end position of textblock."
  (save-excursion
    (let ((po nil)
        (opos (point)))
    (cl-letf (((symbol-function 'fill-region-as-paragraph)
               (lambda (beg end &rest _)
                 (setq po (cons beg end))
                 (throw 'done t))))
      ;; let inner not move point in general?
      (catch 'done
        (fill-paragraph))
      (when (consp po)
        (goto-char (car po))
        (objed--skip-ws)
        (when (or (<= (point) opos (cdr po))
                  (<= (cdr po) opos (point)))
          (cons (point) (cdr po))))))))


(objed-define-object nil buffer
  :atp
  (bobp)
  :get-obj
  (objed-bounds-from-region-cmd #'mark-whole-buffer))


(objed-define-object nil region
  :get-obj
  (objed-make-object
   :beg (mark)
   :end (point)))


(objed-define-object nil bracket
  :atp
  (unless (objed--in-string-or-comment-p)
    (or (looking-at "(\\|\\[\\|{")
        (looking-back ")\\|\\]\\|}" 1)))
  :get-obj
  (unless (objed--in-string-or-comment-p)
    (cond ((and (not (bobp))
                (eq (char-syntax (char-before)) ?\)))
           (let ((end (point))
                 (beg (scan-sexps (point) -1)))
             (objed-make-object :beg beg
                                :end end
                                :ibeg #'1+
                                :iend #'1-)))
          ((and (not (eobp))
                (eq (char-syntax (char-after)) ?\())
           (let ((beg (point))
                 (end (scan-sexps (point) 1)))
             (objed-make-object :beg beg
                                :end end
                                :ibeg #'1+
                                :iend #'1-)))
          (t
           ;; get bracket expression point is in
           (let* ((beg (cadr (syntax-ppss)))
                  (end (when beg (scan-sexps beg 1))))
             (objed-make-object :beg beg
                                :end end
                                :ibeg #'1+
                                :iend #'1-)))))

  :try-next
  (when (re-search-forward "(\\|\\[\\|{" nil t)
    (forward-char -1))
  :try-prev
  (when (re-search-backward ")\\|\\]\\|}" nil t)
    (forward-char 1)))

(objed-define-object nil string
  :atp
  (objed--at-string-p)
  :get-obj
  (let ((bounds (objed--bounds-of-string-at-point)))
    (objed-make-object
     :obounds bounds
     :ibounds (and bounds (objed--inner-string bounds))))
  :try-next
  (let ((cont t)
        (syn nil))
    (while cont
      (while (and (not (eobp))
                  (not (eq ?\" (char-syntax (char-after)))))
        (forward-char 1))
      (if (not (nth 4 (setq syn (syntax-ppss))))
          (setq cont nil)
        (goto-char (nth 8 syn))
        (forward-comment 1))))
  :try-prev
   (let ((cont t)
         (syn nil))
     (while cont
       (while (and (not (bobp))
                   (not (eq ?\" (char-syntax (char-before)))))
         (forward-char -1))
       (if (not (nth 4 (setq syn (syntax-ppss))))
           (setq cont nil)
         (goto-char (nth 8 syn))))))

(objed-define-object nil comment
  :atp
  (objed--at-comment-p)
  :get-obj
  (let ((bounds (objed--comment-block)))
    (objed-make-object :obounds bounds
                       ;; TODO: support multi char/multi line comments
                       ;; act different for them?
                       :ibounds (when bounds
                                  (goto-char (car bounds))
                                  (skip-chars-forward
                                   (format "%s \t" (or comment-start "")))
                                  (cons (point)
                                        (objed--skip-backward (cdr bounds) 'ws)))))
  :try-next
  (comment-search-forward (point-max) t)
  :try-prev
  (comment-search-backward (point-min) t))



;; TODO: get rid of this
(defvar objed--content-bounds nil
  "Holding bounds of current object for `objed-reference'.")

(defun objed--next-content ()
  "Search and set for next content object."
  (let ((curr (apply 'buffer-substring (objed--current))))
    (when objed--content-bounds
      (goto-char (cdr objed--content-bounds)))
    (if (search-forward curr nil t)
        (setq objed--content-bounds
              (cons (match-beginning 0) (match-end 0)))
      (setq objed--content-bounds nil))))


(defun objed--prev-content ()
  "Search and set for prev content object."
  (let ((curr (apply 'buffer-substring (objed--current))))
    (when objed--content-bounds
      (goto-char (car objed--content-bounds)))
    (if (search-backward curr nil t)
        (setq objed--content-bounds
              (cons (match-beginning 0) (match-end 0)))
      (setq objed--content-bounds nil))))


;; special objects, for fallback of objed-reference
(objed-define-object nil content
  :get-obj
  ;; has to be set by commands using this
  (objed-make-object
   :obounds
   (or objed--content-bounds
       (error "Last in current direction")))
    :try-next
  (objed--next-content)
  :try-prev
  (objed--prev-content))

(objed-define-object nil identifier
  :atp (or (looking-at "\\_<")
           (looking-back "\\_>" 1))
  :get-obj
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (objed-make-object
       :obounds bounds
       :ibounds (save-excursion
                  ;; use the symbol prefix by default
                  (goto-char (car bounds))
                  (objed--inner-word-bounds)))))

  :try-next
  (objed--next-identifier)
  :try-prev
  (objed--prev-identifier))


(defun objed--next-identifier ()
  "Move to next identifier."
  (let ((bds nil))
    (if (not (setq bds (bounds-of-thing-at-point 'symbol)))
        (re-search-forward  "\\_<" nil t)
      (let ((sym (and (or bds (setq bds (bounds-of-thing-at-point 'symbol)))
                      (buffer-substring (car bds) (cdr bds)))))
        (when bds
          (goto-char (cdr bds)))
        (if (re-search-forward (format "\\_<%s\\_>"
                                       (regexp-quote sym)) nil t)
            (goto-char (match-beginning 0))
          (goto-char (car bds))
          (when (or (eq real-this-command #'objed-next-identifier)
                    (eq real-this-command #'objed-goto-next-identifier))
            (objed-first-identifier)
            (objed--flash-object 'match)))))))

(defun objed--flash-object (face &optional time)
  "Flash current object using FACE for TIME (defaults to 0.4)."
  (let ((cookie (face-remap-add-relative
                 'objed-hl face)))
    (run-at-time (or time .3) nil
                 (lambda ()
                   (face-remap-remove-relative cookie)))))

(defun objed--prev-identifier ()
  "Move to previous identifier."
  (let ((bds nil))
    (if (not (setq bds (bounds-of-thing-at-point 'symbol)))
        (re-search-backward  "\\_<" nil t)
      (let ((sym (and (or bds (setq bds (bounds-of-thing-at-point 'symbol)))
                      (buffer-substring (car bds) (cdr bds)))))
        (when bds
          (when (looking-back "\\_>" 1)
            (goto-char (car bds)))
          (if (re-search-backward (format "\\_<%s\\_>"
                                          (regexp-quote sym)) nil t)
              (goto-char (match-beginning 0))
            (goto-char (car bds))
            (when (or (eq real-this-command #'objed-prev-identifier)
                      (eq real-this-command #'objed-goto-prev-identifier))
              (objed-last-identifier)
              (objed--flash-object 'match))))))))






(objed-define-object nil section
  :atp
  (and (bound-and-true-p outline-minor-mode)
       (bolp) (not (string= "" outline-regexp))
       (eq (aref comment-start 0) (char-after))
       (looking-at (format "^%s" outline-regexp)))
  :get-obj
  (when (and (bound-and-true-p outline-minor-mode)
             (ignore-errors (outline-back-to-heading) t))
    (objed-make-object :beg (point)
                       :ibeg (line-end-position)
                       :end (progn
                              (outline-next-heading)
                              (or (eobp)
                                  (move-end-of-line 0))
                              (point))
                       :iend (progn (skip-chars-backward " \t\r\n")
                                    (point))))
  :try-next
  (when (bound-and-true-p outline-minor-mode)
    (outline-next-heading))
  :try-prev
  (when (bound-and-true-p outline-minor-mode)
    (outline-previous-heading)))


;; * Mode specific objects

(objed-define-object elisp-mode defun
  :mode emacs-lisp-mode
  :atp
  (looking-at "^(")
  :ref
  (down-list 1)
  (forward-symbol 2)
  (forward-symbol -1)
  'identifier
  :try-next
  ;; (beginning-of-defun -1) does not work for adjacent toplevel
  ;; parens in lisp becaus try next is called after moving beyond the
  (end-of-defun 1)
  (beginning-of-defun 1)
  :get-obj
  (let ((bounds (objed-bounds-from-region-cmd #'mark-defun)))
    (when bounds
      (objed-make-object
       :obounds bounds
       :ibounds
       (save-restriction
         (narrow-to-region (car bounds) (cdr bounds))
         (goto-char (car bounds))
         (objed--skip-ws)
         (ignore-errors
           (cond ((looking-at "(defun")
                  (down-list 2)
                  (up-list 1)
                  (objed--skip-ws)
                  (when (objed--at-string-p)
                    (forward-sexp 1)
                    (objed--skip-ws))

                  (cons (point)
                        (progn (goto-char (point-max))
                               (down-list -1)
                               (point))))
                 (t
                  (cons (progn (down-list 1)
                               (point))
                        (progn (goto-char (point-max))
                               (down-list -1)
                               (point)))))))))))


(objed-define-object css-mode defun
  :mode css-mode
  :atp
  (looking-at "^[^[:space:]]")
  :try-next
  (search-forward "{")
  :try-prev
  (search-backward "{")
  :get-obj
  (unless (objed--in-comment-p)
    (let* ((pos (point))
           (end (and (search-forward "}" nil t) (point)))
           (beg (and end
                     (search-backward "{" nil t)
                     (or (and (re-search-backward "^ *$" nil t)
                              (1+ (match-end 0)))
                         (and (re-search-backward "^" nil t)
                              (line-beginning-position))))))
      (when (and beg end
                 (<= beg pos end))
        (objed-make-object
         :beg beg
         :ibeg (search-forward "{")
         :end end
         :iend (1- end))))))

(objed-define-object nil tag
  :atp
  (and (derived-mode-p 'sgml-mode)
       (or (looking-at "<")
           (looking-back ">" 1)))
  :get-obj
  ;; TODO: fix sgml-backward not working
  (when (derived-mode-p 'sgml-mode)
    ;; like with bracket detect at boundary
    (objed-make-object
     :beg (progn (unless (looking-at "<")
                   (sgml-skip-tag-backward 1))
                 (point))
     :ibeg (save-excursion
             (when (search-forward ">" nil t)
               (point)))
     :end (progn
            (sgml-skip-tag-forward 1)
            (point))
     :iend (progn (when (search-backward "<" nil t)
                    (point)))))
  :try-next
  (search-forward "<" nil t)
  :try-prev
  (search-backward ">" nil t))

(defvar hl-line-overlay)
(defun objed--what-face (&optional pos)
  "Return face at POS."
  (let* ((pos (or pos (point)))
         (ov (car (overlays-at (point) t)))
         (face (or (and ov
                        ;; exclude hl line
                        (not (eq hl-line-overlay ov))
                        (overlay-get ov  'face))
                   (get-char-property pos 'read-face-name)
                   (get-text-property pos 'face))))
     (unless (keywordp (car-safe face)) (list face))))

(defvar objed--last-face nil)
;; from `evil-textobj-syntax'

(defun objed--get-face-range ()
  "Return range of equal face before/after point."
  (let ((point-face (objed--what-face))
        (objed-cmd-alist (and (bound-and-true-p objed-cmd-alist)
                              objed-cmd-alist))
        (backward-none-space-point (point)) ; last none white space char
        (forward-none-space-point (point)) ; last none white space char
        (start (point))
        (end (point)))

    (when (or (eq real-this-command #'objed-face-object)
              (eq 'face (cdr (assq real-this-command objed-cmd-alist))))
      (setq objed--last-face point-face))

    (when (equal objed--last-face
                 (objed--what-face))
      ;; check chars backward,
      ;; stop when char is not white space and has different face
      (save-excursion
        (let ((continue t))
          (while (and continue (>= (- (point) 1) (point-min)))
            (backward-char)
            (let ((backward-point-face (objed--what-face)))
              (if (= 32 (char-syntax (char-after)))
                  (ignore)
                (if (equal point-face backward-point-face)
                    (setq backward-none-space-point (point))
                  (setq continue nil)))))))

      ;; check chars forward,
      ;; stop when char is not white space and has different face
      (save-excursion
        (let ((continue t))
          (while (and continue (< (+ (point) 1) (point-max)))
            (forward-char)
            (let ((forward-point-face (objed--what-face)))
              (if (= 32 (char-syntax (char-after)))
                  (ignore)
                (if (equal point-face forward-point-face)
                    (setq forward-none-space-point (point))
                  (setq continue nil)))))))

      (progn (setq start backward-none-space-point)
             (setq end forward-none-space-point)
             (cons start (+ end 1))))))


(objed-define-object nil face
  :get-obj
  (objed--get-face-range)
  ;; TODO: search for next same face as current...
  :try-next
  (while (not (equal objed--last-face
                     (objed--what-face)))
    (forward-char 1))
  :try-prev
  ;; get out of current
  (forward-char -1)
  (while (not (equal objed--last-face
                     (objed--what-face)))
    (forward-char -1)))

(defvar flycheck-mode)
(defvar flymake-mode)
(defvar flymake-wrap-around)
(declare-function flycheck-overlays-at "ext:flycheck")
(declare-function flycheck-next-error "ext:flycheck")
(declare-function flycheck-previous-error "ext:flycheck")
(declare-function flymake--overlays "ext:flymake")
(declare-function flymake-goto-next-error "ext:flymake")
(declare-function flymake-goto-prev-error "ext:flymake")
(defun objed--get-error-bounds ()
  "Return linter error at point."
  (cond ((bound-and-true-p flycheck-mode)
         (let ((ov (car (flycheck-overlays-at (point)))))
           (when ov
             (cons (overlay-start ov)
                   (overlay-end ov)))))
        (flymake-mode
         (let ((ov (car (flymake--overlays :beg (point)))))
           (when ov
             (run-at-time 0 nil
                          #'message
                          (funcall (overlay-get ov 'help-echo)
                                   (selected-window) ov (point)))
             (cons (overlay-start ov)
                   (overlay-end ov)))))))

(defun objed--next-error ()
  "Goto next linter error."
  (cond ((bound-and-true-p flycheck-mode)
         (flycheck-next-error))
        (flymake-mode
         (let ((flymake-wrap-around nil))
           (flymake-goto-next-error 1)))))

(defun objed--previous-error ()
  "Goto previous linter error."
  (cond ((bound-and-true-p flycheck-mode)
         (flycheck-previous-error))
        (flymake-mode
         (let ((flymake-wrap-around nil))
           (flymake-goto-prev-error 1)))))


(objed-define-object nil error
  :get-obj
  (objed--get-error-bounds)
  :try-next
  (objed--next-error)
  :try-prev
  (objed--previous-error))


(declare-function org-mark-element "ext:org")

(objed-define-object org section
  :mode org-mode
  :atp
  (org-at-heading-p)
  :get-obj
  (when (ignore-errors (org-back-to-heading) t)
    (objed-make-object
     :beg (point)
     :ibeg (line-end-position)
     :end (progn (outline-next-visible-heading 1)
                           (or (eobp) (move-end-of-line 0))
                           (point))
     :iend (progn (skip-chars-backward " \t\r\n")
                  (point))))
  :try-next
  (outline-next-visible-heading 1)
  :try-prev
  (outline-previous-visible-heading 1))

;; blocks are "elements" in current mode
(objed-define-object org block
  :mode org-mode
  :get-obj
  (let ((bounds (objed-bounds-from-region-cmd #'org-mark-element))
        (ibeg nil)
        (ibounds nil))
    (when bounds
      (goto-char (car bounds))
      (setq ibounds (cl-dolist (lr '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                                     ("#\\+BEGIN_SRC.*$" . "#\\+END_SRC")
                                     ("#\\+begin_src.*$" . "#\\+end_src")
                                     ("^#\\+begin_example.*$" . "#\\+end_example$")
                                     ("^#\\+BEGIN_EXAMPLE.*$" . "#\\+END_EXAMPLE$")))
                      (when (and (re-search-forward (car lr) (cdr bounds) t)
                                 (skip-chars-forward "\r\n \t" (line-beginning-position 2))
                                 (setq ibeg (point))
                                 (progn (goto-char (cdr bounds))
                                        (re-search-backward (cdr lr) (car bounds) t)))
                        (skip-chars-backward " \t")
                        (cl-return (cons ibeg (point))))))
      (goto-char (cdr bounds))
      (skip-chars-backward "\r\n \t")
      ;; padding
      (skip-chars-forward "\r\n" (1+ (1+ (point))))
      (setf (cdr bounds) (point))
      (objed-make-object :obounds bounds :ibounds ibounds))))

(declare-function org-table-beginning-of-field "ext:org")
(declare-function org-table-end-of-field "ext:org")
(objed-define-object org field
  :beg (if (looking-back "| ?\\( *\\)" (line-beginning-position))
           (match-beginning 1)
         (org-table-beginning-of-field 1)
         (point))
  :end (if (looking-at "\\( *\\) ?|")
           (match-end 1)
         (org-table-end-of-field 1)
         (point))
  :try-next
  (org-table-end-of-field 1)
  :try-prev
  (org-table-beginning-of-field 1))

(defvar comint-prompt-regexp)
(declare-function comint-next-prompt "ext:comint")
(declare-function comint-previous-prompt "ext:comint")
(objed-define-object nil output
  :atp
  (and (derived-mode-p 'comint-mode)
       (looking-back comint-prompt-regexp
                     (save-excursion
                       (forward-line 0)
                       (point))))
  :get-obj
  (when  (derived-mode-p 'comint-mode)
    (let* ((pos (point))
           (bounds (progn
                     (comint-next-prompt 1)
                     (move-end-of-line 0)
                     (if (> (point) pos)
                         (cons pos (point))
                       (move-end-of-line 2)
                       (cons
                        (line-beginning-position)
                        (point))))))
      (objed-make-object
       :obounds bounds
       :ibounds (progn (goto-char (car bounds))
                       (forward-line 1)
                       (cons (point)
                             (progn (goto-char (cdr bounds))
                                    (objed--skip-ws t)
                                    (point)))))))
  :try-next
  (comint-next-prompt 1)
  :try-prev
  (comint-previous-prompt 1))

(declare-function python-nav-end-of-block "ext:python")
(declare-function python-nav-beginning-of-block "ext:python")
(declare-function python-nav-forward-block "ext:python")
(declare-function python-nav-backward-block "ext:python")
(declare-function python-mark-defun "ext:python")

(objed-define-object python defun
  :mode python-mode
  :no-skip t
  :get-obj
  (let ((obounds (objed-bounds-from-region-cmd #'python-mark-defun)))
    (when obounds
      (goto-char (car obounds))
      (re-search-forward ": *\n +" nil t)
      (objed-make-object :obounds obounds
                         :ibeg (point)
                         :iend (objed--skip-backward (cdr obounds) 'ws))))
  :try-next
  (beginning-of-defun -1)
  :try-prev
  (beginning-of-defun 1))

(objed-define-object python block
    :mode python-mode
    :commands (python-nav-backward-block python-nav-forward-block)
    ;; don't skip current object on navigation because
    ;; python blocks can contain other python blocks...
    :no-skip t
    :try-next
    (python-nav-forward-block)
    :try-prev
    (python-nav-backward-block)
    :beg
    (python-nav-beginning-of-block)
    (objed--skip-ws t (line-beginning-position))
    (point)
    :ibeg
    (forward-line 1)
    (objed--skip-ws)
    (point)
    :iend
    (python-nav-end-of-block)
    (point)
    :end
    (forward-line 1)
    (point))



(provide 'objed-objects)
;;; objed-objects.el ends here

