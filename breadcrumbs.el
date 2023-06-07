;;; breadcrumbs.el --- track buffer positions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ramakrishnan Jayaraman

;; Version: 0.1.0
;; Author: Ramakrishnan Jayaraman <chat@rj95.be>
;; Maintainer: Ramakrishnan Jayaraman <chat@rj95.be>
;; URL: https://github.com/gitrj95/breadcrumbs.el
;; Keywords: convenience
;; Package-Requires ((emacs "27.1"))

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

;; Breadcrumbs is a simple Emacs package that facilitates tracking of
;; positions in buffers and allows navigation among them.
;;
;; Breadcrumbs (ie the positions in buffers), can be dropped (ie
;; stored in a ring buffer) manually or automatically.  They can also
;; be navigated.  The interface is described below.  Motivation,
;; general usage patterns and other qualities are listed in the README
;; in the repository link:
;; https://github.com/gitrj95/breadcrumbs.el
;;
;; In order to use breadcrumbs, `breadcrumbs-mode' must be enabled.

;;;; Interface:

;; `breadcrumbs-drop-breadcrumb' adds the current position in the
;; buffer to a ring.  If point is at a known breadcrumb, the existing
;; breadcrumb will be moved to the head of the ring.  Adding
;; breadcrumbs automatically can be achieved by adding entries to
;; `breadcrumbs-drop-around-fn-list'.  The functions in this list,
;; when invoked, will drop breadcrumbs before and after the operation.
;;
;; The list of breadcrumbs can be inspected with a call to
;; `breadcrumbs-list'.  From here, one can jump to or delete a
;; breadcrumb from the ring.
;;
;; `breadcrumbs-find-and-jump-next' and
;; `breadcrumbs-find-and-jump-previous' will jump to the next and
;; previous breadcrumbs if point is a known breadcrumb.  Otherwise,
;; `breadcrumbs-find-and-jump-previous' will jump to the most recently
;; dropped or jumped to breadcrumb in the Emacs session, and
;; `breadcrumbs-find-and-jump-next' will do nothing.

;;; Code:

(require 'ring)
(require 'cl-lib)
(require 'pulse)

(defgroup breadcrumbs ()
  "Track and navigate among buffer positions."
  :group 'convenience
  :prefix "breadcrumbs-")

(defvar breadcrumbs-ring nil
  "The collection of all tracked buffer positions up to `breadcrumb-ring-max'.")

(defcustom breadcrumb-ring-max 20
  "The maximum number of buffer positions to track."
  :type 'natnum
  :group 'breadcrumbs)

(defcustom breadcrumbs-drop-around-fn-list nil
  "The list of functions for which buffer positions are tracked, before and after invocation."
  :type 'list
  :group 'breadcrumbs)

(defvar breadcrumbs--neighbor nil
  "The most recently dropped or jumped to buffer position via the breadcrumbs interface.")

(defvar breadcrumbs-list-buffer nil
  "The \"*Breadcrumbs List*\" buffer.")

(cl-defstruct breadcrumbs--breadcrumb
  "The breadcrumb definition.

Namely, an aggregate of the file backing the buffer, the position
in the buffer, and the buffer's name.  The last member is
exercised only if the buffer has no associated file, relying on
`find-file' semantics otherwise."
  (buffer-file-name
   (buffer-file-name)
   :documentation "The file backing the breadcrumb.")
  (buffer-position
   (point)
   :documentation "Buffer position of the breadcrumb.")
  (fileless-buffer-name
   (buffer-name)
   :documentation "Buffer name, exercised only when buffer is fileless."))

(defun breadcrumbs--setup ()
  "Set up the state required to start using breadcrumbs.

To accommodate `savehist' or any other package that can set
`breadcrumbs-ring', it's initialized to an empty ring conditioned
on its value at the time of call.

Advice is added \"around\" the specified functions in
`breadcrumbs-drop-around-fn-list' as well."
  (unless breadcrumbs-ring
    (setq breadcrumbs-ring (make-ring breadcrumb-ring-max)))
  (dolist (fn breadcrumbs-drop-around-fn-list)
    (advice-add fn :around #'breadcrumbs--drop-around)))

(defun breadcrumbs--teardown ()
  "Tear down the state required for breadcrumbs.

Namely, we reset all state breadcrumbs uses and remove the advice
associated with `breadcrumbs-drop-around-fn-list'."
  (setq breadcrumbs-ring nil
        breadcrumbs--neighbor nil)
  (dolist (fn breadcrumbs-drop-around-fn-list)
    (advice-remove fn #'breadcrumbs--drop-around)))

(defun breadcrumbs--switch-to-fileless-buffer (breadcrumb)
  "Attempt to switch to the buffer of a fileless BREADCRUMB, if it exists.

If not, optionally remove it from `breadcrumbs-ring'."
  (if-let* ((buffer-name (breadcrumbs--breadcrumb-fileless-buffer-name breadcrumb))
            (buffer (get-buffer buffer-name)))
      (switch-to-buffer buffer)
    (when (yes-or-no-p (format "%s has been killed.  Remove this breadcrumb from `breadcrumbs-ring'? " buffer-name))
      (ring-remove breadcrumbs-ring (ring-member breadcrumbs-ring breadcrumb))
      (breadcrumbs-list--revert)
      nil)))

(defun breadcrumbs--jump (breadcrumb)
  "Jump to the specified buffer and position as specified by BREADCRUMB."
  (setq breadcrumbs--neighbor breadcrumb)
  (when
      (cond
       ((breadcrumbs--breadcrumb-buffer-file-name breadcrumb)
        (find-file (breadcrumbs--breadcrumb-buffer-file-name breadcrumb)))
       ((breadcrumbs--breadcrumb-fileless-buffer-name breadcrumb)
        (breadcrumbs--switch-to-fileless-buffer breadcrumb)))
    (goto-char (breadcrumbs--breadcrumb-buffer-position breadcrumb))
    (pulse-momentary-highlight-one-line)))

(defun breadcrumbs--drop ()
  "Track the buffer position as a `breadcrumbs--breadcrumb'.

If this has already been tracked, move an existing one in `breadcrumbs-ring' to head."
  (let* ((breadcrumb (make-breadcrumbs--breadcrumb))
         (index (ring-member breadcrumbs-ring breadcrumb)))
    (setq breadcrumbs--neighbor breadcrumb)
    (when index (ring-remove breadcrumbs-ring index))
    (ring-insert breadcrumbs-ring breadcrumb))
  (breadcrumbs-list--revert)
  (pulse-momentary-highlight-one-line))

(defun breadcrumbs--drop-around (fn &rest args)
  "Track the buffer position before and after FN is invoked with ARGS and return its last form."
  (breadcrumbs--drop)
  (let ((result (apply fn args)))
    (breadcrumbs--drop)
    result))

(defun breadcrumbs--find-and-jump (&rest args)
  "Jump to the next or previous buffer position based on a direction gleaned from ARGS."
  (let ((jump-target
         (let ((candidate (make-breadcrumbs--breadcrumb)))
           (and (ring-member breadcrumbs-ring candidate) candidate)))
	(direction (plist-get args :type)))
    (cond (jump-target
           (breadcrumbs--jump
            ;; our next/previous semantics are reversed from ring's
            (cond ((eq direction 'next) (ring-previous breadcrumbs-ring jump-target))
                  ((eq direction 'previous) (ring-next breadcrumbs-ring jump-target)))))
          (breadcrumbs--neighbor
           (when (eq direction 'previous)
             (breadcrumbs--jump breadcrumbs--neighbor))))))

;;;###autoload
(define-minor-mode breadcrumbs-mode
  "Track positions in buffers using breadcrumbs."
  :global t
  (if breadcrumbs-mode
      (breadcrumbs--setup)
    (breadcrumbs--teardown)))

(defvar breadcrumbs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") #'breadcrumbs-list-jump)
    (define-key map (kbd "k") #'breadcrumbs-list-delete)
    map)
  "The key map while in the \"*Breadcrumbs List*\" buffer.")

(defun breadcrumbs--format-slot (slot len)
  "Formats a `breadcrumbs--breadcrumb' SLOT given the LEN it will be left-aligned against."
  (let ((format-string (format "%%-%ds" len)))
    (format format-string (or slot ""))))

(defun breadcrumbs--format-breadcrumb (breadcrumb)
  "Return BREADCRUMB's formatted slots as a vector.

The structure of this return type is to conform with members of
`tabulated-list-entries'."
  (let* ((buffer-file-name (breadcrumbs--breadcrumb-buffer-file-name breadcrumb))
	 (buffer-position (breadcrumbs--breadcrumb-buffer-position breadcrumb))
         (buffer-name (breadcrumbs--breadcrumb-fileless-buffer-name breadcrumb))
         (breadcrumb-name (or buffer-file-name buffer-name)))
    (vector
     (breadcrumbs--format-slot breadcrumb-name 64)
     (breadcrumbs--format-slot buffer-position 16))))

(defun breadcrumbs-list--entries ()
  "Return `tabulated-list-entries' as a collection of formatted `breadcrumbs--breadcrumb' instances keyed on the instance."
  (mapcar (lambda (breadcrumb)
            (list
             breadcrumb
             (breadcrumbs--format-breadcrumb breadcrumb)))
          (ring-elements breadcrumbs-ring)))

(defun breadcrumbs-list--revert ()
  "Reverts `breadcrumbs-list-buffer'."
  (when (buffer-live-p breadcrumbs-list-buffer)
    (with-current-buffer breadcrumbs-list-buffer
      (tabulated-list-revert))))

(define-derived-mode breadcrumbs-list-mode tabulated-list-mode
  "Tabular list mode displaying tracked buffer positions."
  (setq-local tabulated-list-entries #'breadcrumbs-list--entries
	      tabulated-list-format `[("Name" 64) ("Position" 16)])
  (tabulated-list-init-header))

(defun breadcrumbs-list-jump ()
  "Jump to buffer position from \"*Breadcrumbs List*\"."
  (interactive)
  (breadcrumbs--jump (tabulated-list-get-id)))

(defun breadcrumbs-list-delete ()
  "Delete a buffer position from \"*Breadcrumbs List*\"."
  (interactive)
  (ring-remove breadcrumbs-ring
               (ring-member breadcrumbs-ring (tabulated-list-get-id)))
  (when (ring-empty-p breadcrumbs-ring)
    (setq breadcrumbs--neighbor nil))
  (breadcrumbs-list--revert))

;;;###autoload
(defun breadcrumbs-drop-breadcrumb ()
  "Track the buffer position at point."
  (interactive)
  (breadcrumbs--drop))

;;;###autoload
(defun breadcrumbs-list ()
  "Show the list of tracked buffer positions."
  (interactive)
  (with-current-buffer (get-buffer-create "*Breadcrumbs List*")
    (setf breadcrumbs-list-buffer (current-buffer))
    (breadcrumbs-list-mode)
    (pop-to-buffer breadcrumbs-list-buffer)))

;;;###autoload
(defun breadcrumbs-find-and-jump-next ()
  "Jump to the next, tracked buffer position as of the current buffer position.

If `point' is not at a tracked buffer position, do nothing."
  (interactive)
  (breadcrumbs--find-and-jump :type 'next))

;;;###autoload
(defun breadcrumbs-find-and-jump-previous ()
  "Jump to the previous, tracked buffer position as of the current buffer position.

If `point' is not at a tracked buffer position, jump to the most
recently dropped or jumped to buffer position."
  (interactive)
  (breadcrumbs--find-and-jump :type 'previous))

(provide 'breadcrumbs)

;;; breadcrumbs.el ends here
