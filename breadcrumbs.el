;;; breadcrumbs.el --- track buffer positions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ramakrishnan Jayaraman

;; Version: 0.0.1
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
;;
;; Interface:
;;
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
(require 'eieio)
(require 'pulse)

(defvar breadcrumbs-ring nil
  "All dropped breadcrumbs, up to `breadcrumb-ring-max' breadcrumbs.")

(defcustom breadcrumb-ring-max 20
  "The maximum number of breadcrumbs to track."
  :type 'natnum
  :group 'breadcrumbs)

(defcustom breadcrumbs-drop-around-fn-list nil
  "The list of functions for which a breadcrumb is dropped before invocation."
  :type 'list
  :group 'breadcrumbs)

(defvar breadcrumbs--neighbor nil
  "The most recently dropped or jumped to breadcrumb.")

(defvar breadcrumbs-list-buffer nil
  "The \"*Breadcrumbs List*\" buffer.")

(defclass breadcrumbs--breadcrumb ()
  ((buffer-file-name
    :initform (buffer-file-name)
    :documentation "The full file path of this breadcrumb.")
   (buffer-position
    :initform (point)
    :documentation "Position in buffer."))
  "The breadcrumb definition.")

(defun breadcrumbs--setup ()
  "Set up the state required to start using breadcrumbs."
  (if (not breadcrumbs-ring)
      (setq breadcrumbs-ring (make-ring breadcrumb-ring-max)))
  (mapcar (lambda (fn)
            (advice-add fn :around #'breadcrumbs--drop-around)) breadcrumbs-drop-around-fn-list))

(defun breadcrumbs--teardown ()
  "Tear down the state required for breadcrumbs."
  (setq breadcrumbs-ring nil)
  (setq breadcrumbs--neighbor nil)
  (mapcar (lambda (fn)
            (advice-remove fn #'breadcrumbs--drop-around)) breadcrumbs-drop-around-fn-list))

(defun breadcrumbs--jump (breadcrumb)
  "Jump to the specified breadcrumb."
  (setq breadcrumbs--neighbor breadcrumb)
  (with-slots (buffer-file-name buffer-position) breadcrumb
      (find-file-existing buffer-file-name)
      (goto-char buffer-position)
    (pulse-momentary-highlight-one-line)))

(defun breadcrumbs--drop ()
  "Drop a breadcrumb at point, moving an existing one if it already exists."
  (let* ((breadcrumb (make-instance 'breadcrumbs--breadcrumb))
         (index (ring-member breadcrumbs-ring breadcrumb)))
    (setq breadcrumbs--neighbor breadcrumb)
    (if index (ring-remove breadcrumbs-ring index))
    (ring-insert breadcrumbs-ring breadcrumb))
  (breadcrumbs-list--revert)
  (pulse-momentary-highlight-one-line))

(defun breadcrumbs--drop-around (fn &rest args)
  "Drop breadcrumbs before and after the function call."
  (breadcrumbs--drop)
  (let ((result (apply fn args)))
    (breadcrumbs--drop)
    result))

(defun breadcrumbs--find-and-jump (&key direction)
  "Find some candidate breadcrumb and jump to the next or previous, based on `direction'."
  (let ((jump-target
         (let ((candidate (make-instance 'breadcrumbs--breadcrumb)))
           (if (ring-member breadcrumbs-ring candidate)
               candidate))))
    (cond (jump-target
           (breadcrumbs--jump
            ;; our next/previous semantics are reversed from ring's
            (cond ((eq direction 'next) (ring-previous breadcrumbs-ring jump-target))
                  ((eq direction 'previous) (ring-next breadcrumbs-ring jump-target)))))
          (breadcrumbs--neighbor
           (if (eq direction 'previous)
               (breadcrumbs--jump breadcrumbs--neighbor))))))

;;;###autoload
(define-minor-mode breadcrumbs-mode
  "Track positions in buffers and files using breadcrumbs."
  :global t
  :init-value t
  (if breadcrumbs-mode
      (breadcrumbs--setup)
    (breadcrumbs--teardown)))

(defvar breadcrumbs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "j") #'breadcrumbs-list-jump)
    (define-key map (kbd "<RET>") #'breadcrumbs-list-jump)
    (define-key map (kbd "k") #'breadcrumbs-list-delete)
    map)
  "The key map while in the \"*Breadcrumbs List*\" buffer.")

(defun breadcrumbs--format-slot (slot len)
  "Formats a breadcrumbs slot."
  (let ((format-string (format "%%-%ss" len)))
    (format format-string
            (if slot slot ""))))

(defun breadcrumbs--format-breadcrumb (breadcrumb)
  "Return a formatted breadcrumb as a vector of formatted slots."
  (with-slots (buffer-file-name buffer-position) breadcrumb
    (vector
     (breadcrumbs--format-slot buffer-file-name 64)
     (breadcrumbs--format-slot buffer-position 16))))

(defun breadcrumbs-list--entries ()
  "Return `tabulated-list-entries' as a collection of breadcrumbs."
  (mapcar (lambda (breadcrumb)
            (list
             breadcrumb
             (breadcrumbs--format-breadcrumb breadcrumb))) (ring-elements breadcrumbs-ring)))

(defun breadcrumbs-list--revert ()
  "Reverts `breadcrumbs-list-buffer'."
  (when (buffer-live-p breadcrumbs-list-buffer)
    (with-current-buffer breadcrumbs-list-buffer
      (tabulated-list-revert))))

(define-derived-mode breadcrumbs-list-mode tabulated-list-mode
  "Tabular list mode displaying tracked breadcrumbs."
  (setq-local tabulated-list-format
              `[("File" 64) ("Position" 16)])
  (add-hook 'tabulated-list-revert-hook
            (lambda ()
              (setf tabulated-list-entries (breadcrumbs-list--entries))))
  (tabulated-list-init-header)
  (breadcrumbs-list--revert))

(defun breadcrumbs-list-jump ()
  "Jump to breadcrumb from \"*Breadcrumbs List*\"."
  (interactive)
  (breadcrumbs--jump (tabulated-list-get-id)))

(defun breadcrumbs-list-delete ()
  "Delete a breadcrumb from \"*Breadcrumbs List*\"."
  (interactive)
  (ring-remove breadcrumbs-ring
               (ring-member breadcrumbs-ring (tabulated-list-get-id)))
  (if (ring-empty-p breadcrumbs-ring)
      (setq breadcrumbs--neighbor nil))
  (breadcrumbs-list--revert))

;;;###autoload
(defun breadcrumbs-drop-breadcrumb ()
  "Drop a breadcrumb at point."
  (interactive)
  (breadcrumbs--drop))

;;;###autoload
(defun breadcrumbs-list ()
  "Show the list of breadcrumbs."
  (interactive)
  (with-current-buffer (get-buffer-create "*Breadcrumbs List*")
    (setf breadcrumbs-list-buffer (current-buffer))
    (breadcrumbs-list-mode)
    (pop-to-buffer breadcrumbs-list-buffer)))

;;;###autoload
(defun breadcrumbs-find-and-jump-next ()
  "Jump to the next breadcrumb from the breadcrumb at point."
  (interactive)
  (breadcrumbs--find-and-jump :type 'next))

;;;###autoload
(defun breadcrumbs-find-and-jump-previous ()
  "Jump to the previous breadcrumb from the breadcrumb at point."
  (interactive)
  (breadcrumbs--find-and-jump :type 'previous))

(provide 'breadcrumbs)

;;; breadcrumbs.el ends here
