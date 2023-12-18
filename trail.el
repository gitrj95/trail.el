;;; trail.el --- track buffer positions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ramakrishnan Jayaraman

;; Version: 0.1.0
;; Author: Ramakrishnan Jayaraman <chat@rj95.be>
;; Maintainer: Ramakrishnan Jayaraman <chat@rj95.be>
;; URL: https://github.com/gitrj95/trail.el
;; Keywords: convenience
;; Package-Requires ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Trail is a simple Emacs package that facilitates tracking of
;; positions in buffers and allows navigation among them.
;;
;; Buffer positions can be marked thereby leaving a trail to follow,
;; manually or automatically.  They can also be navigated as one would
;; expect with a semantic trail.  When buffers are alive, markers are
;; used, by etymological design.  The interface is described below.
;; Motivation, general usage patterns and other qualities are listed
;; in the README in the repository link:
;; https://github.com/gitrj95/trail.el
;;
;; In order to use trail, `trail-mode' must be enabled.  Moreover, if
;; you're using machinery that persists `trail-ring', this must happen
;; before `trail-mode' is enabled.

;;;; Interface:

;; `trail-mark' tracks the current buffer position.  If point is at a
;; known trail-mark, the existing trail-mark will be moved to the most
;; recent.  Adding trail-marks automatically can be achieved by adding
;; entries to `trail-mark-around-functions'.  The functions in this
;; list, when invoked, will mark positions before and after the
;; operation.
;;
;; The list of trail-marks can be inspected with a call to
;; `trail-list'.  From here, one can jump-to or delete trail-marks.

;; `trail-find-and-jump-next' and `trail-find-and-jump-previous' will
;; jump to the next and previous trail-mark if point is a known
;; trail-mark.  Otherwise, `trail-find-and-jump-previous' will jump to
;; the most recently marked or jumped-to position in the Emacs
;; session, and `trail-find-and-jump-next' will do nothing.

;;; Code:

(require 'ring)
(require 'cl-lib)
(require 'pulse)

(defgroup trail ()
  "Track and navigate among buffer positions."
  :group 'convenience
  :prefix "trail-")

(defvar trail-ring nil
  "The collection of all trail-marks.

Notably, this ring can be persisted with `savehist', but doing so
requires serializing `savehist-mode' -> `trail-mode'.")

(defvar trail--mark-ring nil
  "The markers backing the trail-marks corresponding to the persisted `trail-ring'.

Notably, raw markers cannot be persisted despite having the
benefit of relative semantics -- something absolute positions do
not.  To have our cake and eat it too, markers are used, if
available, and we fall back to absolute positions, otherwise.")

(defcustom trail-ring-max 20
  "The maximum number of buffer positions to track."
  :type 'natnum
  :group 'trail)

(defcustom trail-mark-around-functions nil
  "The list of functions for which buffer positions are tracked, before and after invocation."
  :type 'list
  :group 'trail)

(defvar trail--neighbor nil
  "The index of the most recently dropped or jumped to `trail--trail-mark'.")

(defvar trail-list-buffer nil
  "The \"*Trail List*\" buffer.

This buffer lists the \"Name\" and position of each trail-mark, given
the following rules:
- If the trail-mark buffer belongs to a file, this file is the name.
- If the trail-mark buffer has no file, the buffer name is the name.

In all cases, the position is the position of the trail-mark.
This position may update as the markers backing the trail-marks
define new absolute positions.")

(defvar trail-list-ellipsis "..."
  "The token used to truncate the `trail-mark' name in the middle instead of at the left or right ends.")

(cl-defstruct trail--trail-mark
  "The persisted `trail-mark' definition.

Namely, an aggregate of the file backing the buffer, the position
in the buffer, and the buffer's name.  The last member is
exercised only if the buffer has no associated file, relying on
`find-file' semantics otherwise.

When `trail--jump' is called (ie when jumping to a `trail-mark'),
markers, kept in a second ring, are preferentially used.
Otherwise, the positions used in `trail-ring' as defined by this
type are used."
  (file-name
   (buffer-file-name)
   :documentation "The file backing the trail-mark.")
  (position
   (point)
   :documentation "The position of the trail-mark, used as a fallback.")
  (buffer-name
   (buffer-name)
   :documentation "Buffer name, exercised only when the trail-mark is fileless."))

(defun trail--ring-length ()
  "Return the length of the ring backing trail-marks."
  (ring-length trail-ring))

(defun trail--update-positions-from-markers ()
  "Update `trail-ring' using the corresponding markers in `trail--mark-ring'.

Namely, when a buffer is closed, all the markers in that buffer
that we're tracking will have their absolute positions updated.
This is so we can get the most recent absolute position of a
marker for the next file open."
  (dotimes (i (trail--ring-length))
    (let ((trail-mark (ring-ref trail-ring i))
          (marker (ring-ref trail--mark-ring i)))
      (when (eq (current-buffer) (marker-buffer marker))
        (setf (trail--trail-mark-position trail-mark)
              (marker-position marker))))))

(defun trail--setup ()
  "Set up the state required to start using trail.

Some additional mutations to Emacs state are also done:

1. Advice is added \"around\" the specified functions in
`trail-mark-around-functions'.
2. Marker-to-position conversion of trail-marks is added to
`kill-buffer-hook'."
  (if trail-ring
      (ring-resize trail-ring trail-ring-max)
    (setq trail-ring (make-ring trail-ring-max)))
  (setq trail--mark-ring (make-ring trail-ring-max))
  (dotimes (_ trail-ring-max)
    (ring-insert trail--mark-ring (make-marker)))
  (dolist (fn trail-mark-around-functions)
    (advice-add fn :around #'trail--mark-around))
  (add-hook 'kill-buffer-hook #'trail--update-positions-from-markers))

(defun trail--teardown ()
  "Tear down the state required for trail.

Namely, we reset all state established in `trail--setup'."
  (when (buffer-live-p trail-list-buffer)
    (kill-buffer trail-list-buffer))
  (setq trail-ring nil
        trail--mark-ring nil
        trail--neighbor nil)
  (dolist (fn trail-mark-around-functions)
    (advice-remove fn #'trail--mark-around))
  (remove-hook 'kill-buffer-hook #'trail--update-positions-from-markers))

(defun trail--remove-at-index (index)
  "Remove an INDEX into the tracked trail-marks."
  (ring-remove trail-ring index)
  (ring-remove trail--mark-ring index))

(defun trail--switch-to-fileless-buffer (index)
  "Switch to the buffer of a fileless `trail-mark', if it exists, given an INDEX.

If not, optionally remove it."
  (if-let* ((trail-mark (ring-ref trail-ring index))
            (buffer-name (trail--trail-mark-buffer-name trail-mark))
            (buffer (get-buffer buffer-name)))
      (switch-to-buffer buffer)
    (when (yes-or-no-p (format "%s has been killed.  Remove this trail-mark? " buffer-name))
      (trail--remove-at-index index)
      (trail-list--revert)
      nil)))

(defun trail--jump (index)
  "Jump to the specified `trail-mark' given an INDEX into the paired rings.

Concretely, if the pursuant member in `trail--mark-ring' exists,
we use that.  Otherwise, we fall back to the entry in
`trail-ring'.  After the jump, both are updated to correspond to
a live marker in the former that has a position equaling the
position specified by the latter."
  (let ((trail-mark (ring-ref trail-ring index))
        (marker (ring-ref trail--mark-ring index)))
    (when
        (cond
         ((trail--trail-mark-file-name trail-mark)
          (find-file (trail--trail-mark-file-name trail-mark)))
         ((trail--trail-mark-buffer-name trail-mark)
          (trail--switch-to-fileless-buffer index)))
      (goto-char
       (if (and marker (marker-position marker))
           marker
         (trail--trail-mark-position trail-mark)))
      (move-marker marker (point))
      (setf (trail--trail-mark-position trail-mark)
            (marker-position marker))
      (setq trail--neighbor index)
      (pulse-momentary-highlight-one-line))
    marker))

(defun trail--mark ()
  "Tracks the buffer position by minting a `trail-mark' storing it.

Namely, during an Emacs session, trail-marks are mapped to
markers while the buffers are alive to allow relative position
semantics.  Corresponding instances of the `trail--trail-mark'
type track the absolute positions, and they're kept in line as
`trail--jump' is called.

If a marker is no longer active, the absolute position in the
corresponding `trail--trail-mark' instance is used as a fallback."
  (let* ((marker (make-marker))
         (marker (move-marker marker (point)))
         (marker-index (ring-member trail--mark-ring marker))
         (trail-mark (make-trail--trail-mark)))
    (when marker-index
      (trail--remove-at-index marker-index))
    (ring-insert trail-ring trail-mark)
    (ring-insert trail--mark-ring marker))
  (setq trail--neighbor 0)
  (trail-list--revert)
  (pulse-momentary-highlight-one-line))

(defun trail--mark-around (fn &rest args)
  "Call `trail--mark' before and after FN is invoked with ARGS and return its last form."
  (trail--mark)
  (let ((result (apply fn args)))
    (trail--mark)
    result))

(defun trail--next-index (index)
  "Calculate the next `trail-mark' given an INDEX referencing either a `trail-mark' or its corresponding marker."
  (ring-minus1 index (trail--ring-length)))

(defun trail--previous-index (index)
  "Calculate the previous `trail-mark' given an INDEX referencing either a `trail-mark' or its corresponding marker."
  (ring-plus1 index (trail--ring-length)))

(defun trail--find-and-jump (&rest args)
  "Jump to the next or previous `trail-mark' based on ARGS.

If `point' is at a known `trail-mark', we navigate as expected.
Otherwise, the previous `trail-mark' is bound to `trail--neighbor'
and the next `trail-mark' is nil and not a valid jump target."
  (let ((jump-index
         (let* ((marker (make-marker))
                (marker (move-marker marker (point))))
           (ring-member trail--mark-ring marker)))
	(direction (plist-get args :type)))
    (let ((new-marker
           (cond (jump-index
                  (trail--jump
                   (cond ((eq direction 'next)
                          (trail--next-index jump-index))
                         ((eq direction 'previous)
                          (trail--previous-index jump-index)))))
                 (trail--neighbor
                  (when (eq direction 'previous)
                    (trail--jump trail--neighbor))))))
      (unless new-marker
        (error "Not at a trail-mark"))
      (when (and jump-index (equal (ring-ref trail--mark-ring jump-index)
                                   new-marker)
                 (< 1 (trail--ring-length)))
        (message "Adjacent trail-marks have coalesced.  Deduplicating.")
        (trail--remove-at-index jump-index)))))

;;;###autoload
(define-minor-mode trail-mode
  "Track positions in buffers and navigate among them."
  :global t
  (if trail-mode
      (trail--setup)
    (trail--teardown)))

(defvar trail-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") #'trail-list-jump)
    (define-key map (kbd "k") #'trail-list-delete)
    (define-key map (kbd "e") #'trail-list-expand-trail-mark)
    (define-key map (kbd "j") #'trail-list-goto-trail-mark)
    (define-key map (kbd "r") #'trail-list-goto-neighbor)
    map)
  "The key map while in the \"*Trail List*\" buffer.")

(defun trail--format-full-contents (index)
  "Format the `trail-mark' given an INDEX into `trail-ring' and `trail--mark-ring'.

Notably, the names and positions here are not truncated."
  (let* ((trail-mark (ring-ref trail-ring index))
         (file-name (trail--trail-mark-file-name trail-mark))
         (buffer-name (trail--trail-mark-buffer-name trail-mark))
         (trail-mark-name (or file-name buffer-name))
         (marker (ring-ref trail--mark-ring index))
         (contents
          (format "%s:%s" trail-mark-name
                  (if-let ((marker-position (and marker (marker-position marker))))
                      marker-position
                    (trail--trail-mark-position trail-mark)))))
    (put-text-property 0 (length trail-mark-name) 'face 'font-lock-keyword-face contents)
    contents))

(defun trail--format-truncated-name (name len)
  "Formats a `trail-mark' NAME given the LEN it will be left-aligned against.

Notably, the NAME will be truncated in the middle given
`trail-list-ellipsis'."
  (let* ((format-string (format "%%-%ds" len))
         (formatted-string (format format-string (or name "")))
         (formatted-string-length (length formatted-string))
         (truncated-half-length (/ (- len (length trail-list-ellipsis)) 2)))
    (if (<= formatted-string-length len)
        formatted-string
      (concat
       (substring formatted-string 0 truncated-half-length)
       trail-list-ellipsis
       (substring formatted-string (- formatted-string-length
                                      (1+ truncated-half-length))
                  formatted-string-length)))))

(defun trail--format-marker-and-position (marker position)
  "Formats a `trail-mark' given a MARKER and its last-known absolute POSITION.

Namely, if MARKER exists and is valid, we use its position.
Otherwise, we fall back to POSITION."
  (format "%d"
          (if-let ((marker-position (and marker (marker-position marker))))
              marker-position
            position)))

(defun trail--format-list-entry (index)
  "Formats a `trail-mark' given an INDEX into `trail-ring' and `trail--mark-ring'.

The structure of this return type is to conform with members of
`tabulated-list-entries'."
  (let* ((trail-mark (ring-ref trail-ring index))
         (marker (ring-ref trail--mark-ring index))
         (file-name (trail--trail-mark-file-name trail-mark))
         (buffer-name (trail--trail-mark-buffer-name trail-mark))
         (trail-mark-name (or file-name buffer-name)))
    (vector
     (trail--format-truncated-name trail-mark-name 64)
     (trail--format-marker-and-position
      marker
      (trail--trail-mark-position trail-mark)))))

(defun trail-list--entries ()
  "Return `tabulated-list-entries' corresponding to all trail-marks."
  (cl-loop for i to (1- (trail--ring-length))
           collect (list i (trail--format-list-entry i))))

(defun trail-list--revert ()
  "Reverts `trail-list-buffer'."
  (when (buffer-live-p trail-list-buffer)
    (with-current-buffer trail-list-buffer
      (tabulated-list-revert))))

(define-derived-mode trail-list-mode tabulated-list-mode
  "Tracked buffer positions"
  (setq-local tabulated-list-entries #'trail-list--entries
              ;; numerical WIDTH of "Position" is ignored
	      tabulated-list-format `[("Name" 64) ("Position" -1)])
  (tabulated-list-init-header)
  (trail-list--revert))

(defun trail-list-jump ()
  "Jump to buffer position from \"*Trail List*\"."
  (interactive)
  (if-let ((index (tabulated-list-get-id)))
      (trail--jump index)
    (error "Not at a trail-mark")))

(defun trail-list-delete ()
  "Delete a buffer position from \"*Trail List*\"."
  (interactive)
  (when-let ((index (tabulated-list-get-id)))
    (trail--remove-at-index index)
    (when (or (ring-empty-p trail-ring) (equal index trail--neighbor))
      (setq trail--neighbor nil))
    (trail-list--revert)))

(defun trail-list-expand-trail-mark ()
  "Expand the `trail-mark' at `point' in \"*Trail List*\" to its full name and position."
  (interactive)
  (when-let ((index (tabulated-list-get-id)))
    (message "Expanded trail-mark: %s" (trail--format-full-contents index)))
  ;; markers may update while the buffer exists, so just do it here too
  (trail-list--revert))

(defun trail-list-goto-trail-mark (line)
  "Go to the LINE describing the `trail-mark' in \"*Trail List*\" using its full name and position."
  (interactive
   (list
    (let* ((trail-alist
            (cl-loop for i to (1- (trail--ring-length))
                     collect (cons (trail--format-full-contents i) (1+ i))))
           (choice
            (completing-read "Go to trail-mark: " trail-alist)))
      (cdr (assoc choice trail-alist)))))
  (when line
    (with-current-buffer trail-list-buffer
      (goto-line line)
      (pulse-momentary-highlight-one-line))))

(defun trail-list-goto-neighbor ()
  "Go to the line in \"*Trail List*\" that holds the most recently marked or jumped-to position."
  (interactive)
  (trail-list--revert)
  (unless trail--neighbor
    (error "No recently marked or jumped-to trail-mark"))
  (with-current-buffer trail-list-buffer
    (goto-line (1+ trail--neighbor))
    (pulse-momentary-highlight-one-line)))

;;;###autoload
(defun trail-mark ()
  "Track the position at point."
  (interactive)
  (trail--mark))

;;;###autoload
(defun trail-list ()
  "Show the list of tracked positions."
  (interactive)
  (with-current-buffer (get-buffer-create "*Trail List*")
    (setf trail-list-buffer (current-buffer))
    (trail-list-mode)
    (pop-to-buffer trail-list-buffer)))

;;;###autoload
(defun trail-find-and-jump-next ()
  "Jump to the next, tracked position as of the current position.

If `point' is not at a tracked position, do nothing."
  (interactive)
  (trail--find-and-jump :type 'next))

;;;###autoload
(defun trail-find-and-jump-previous ()
  "Jump to the previous, tracked position as of the current position.

If `point' is not at a tracked position, jump to the most
recently marked or jumped-to buffer position as defined by
`trail--neighbor'."
  (interactive)
  (trail--find-and-jump :type 'previous))

(provide 'trail)

;;; trail.el ends here
