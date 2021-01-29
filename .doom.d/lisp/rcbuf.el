;;; rcbuf.el --- A collection of buffers -*- lexical-binding: t; -*-

;;; TODO
;;; - Redo the hash-tables interface to separate an owners list from a tag list
;;; - Make all commands interactive
;;; - Write a test suite that spawns a new frame and runs all the tests inside

;;; Code:

;;;; Utils
(defun rcbuf--make-tag-set ()
  "Create a tag set."
  (make-hash-table :test 'equal :size 10 :weakness 'value))

(defun rcbuf--make-buffer-set ()
  "Create a buffer set."
  (make-hash-table :test 'equal :size 10 :weakness 'value))

(defun rcbuf--put-ownertag (tag tagset)
  "Add TAG in the tag set of owning tags TAGSET. Return a tagset with the addition."
  (puthash tag 'owner tagset)
  tagset)

(defun rcbuf--put-buffer-reference (tag buffer tagset)
  "Add BUFFER as part of TAG in the tag set TAGSET. Return a tagset with the addition."
  (let ((buffer-set (gethash tag tagset (rcbuf--make-buffer-set))))
    (puthash buffer 'owned buffer-set)
    tagset))

(defun rcbuf--remtag (tag tagset)
  "Remove TAG from the tag set TAGSET. Return a tagset with the deletion."
  (remhash tag tagset)
  tagset)

(defun rcbuf--tag-set-emptyp (tagset)
  "Return non-nil if TAGSET is empty."
  (= 0 (hash-table-count tagset)))

;;;; Variables
(defvar-local rcbuf--owned-by (rcbuf--make-tag-set)
  "A buffer-local mag whose keys are the current tags the buffer has.")

;;;; Functions
(defun rcbuf--init ()
  "Initializes the local variables needed to start using rcbuf."
  (modify-all-frames-parameters `((rcbuf--tags-list . ,(rcbuf--make-tag-set)))))

(defun rcbuf-tag-buffer (tag-name &optional buffer)
  "Tag BUFFER as owned by TAG-NAME. If BUFFER is nil, tag current buffer"
  (let* ((buffer (or buffer (current-buffer)))
         (owners (buffer-local-value 'rcbuf--owned-by buffer))
         (frame-tags (frame-parameter nil 'rcbuf--tags-list)))
    (with-current-buffer buffer
      (setq-local rcbuf--owned-by (rcbuf--put-ownertag tag-name owners))
      (set-frame-parameter nil 'rcbuf--tags-list (rcbuf--puttag tag-name frame-tags)))))


(defun rcbuf-untag-buffer (tag-name &optional buffer keep-dangling)
  "Mark BUFFER as not owned by TAG-NAME anymore.

If BUFFER is nil, untag current buffer
If KEEP-DANGLING is not nil, BUFFER will not be killed in no tag owns it afterwards."
  (let* ((buffer (or buffer (current-buffer)))
         (owners (buffer-local-value 'rcbuf--owned-by buffer))
         (new-owners (rcbuf--remtag tag-name owners)))
    (with-current-buffer buffer
      (setq-local rcbuf--owned-by new-owners)
      (unless keep-dangling
        (when (rcbuf--tag-set-emptyp new-owners)
          (kill-buffer buffer))))))


(provide 'rcbuf)
