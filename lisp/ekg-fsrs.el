;; --- DEPRECATED ---
(straight-use-package 'ekg)
(straight-use-package 'fsrs)

(require 'ekg)
(require 'triples)

(require 'fsrs)

(require 'seq)
(require 'subr-x)

;;; Tag
(defvar ekg-fsrs-tag-prefix
  "fsrs/")

(defun ekg-fsrs-tag-with-prefix-p (tag)
  (string-prefix-p ekg-fsrs-tag-prefix tag))

(defun ekg-fsrs-make-tag (date)
  (concat ekg-fsrs-tag-prefix date))

;;; FSRS
(defvar ekg-fsrs-weights
  fsrs-weights-default)

(defvar ekg-fsrs-scheduler
  (fsrs-make-scheduler :weights ekg-fsrs-weights))

;;; Schema
(defun ekg-fsrs-add-schema ()
  "Add the fsrs schema to ekg database."

  (triples-add-schema ekg-db
		      'fsrs
		      
		      '(card
			:base/type fsrs-card
			:base/unique t)

		      '(review-log
			:base/type fsrs-review-log)))

;;; Accessors
(defun ekg-fsrs-get-note-card (note)
  (thread-first note
		(ekg-note-properties)
		(plist-get :fsrs/card)))

(defun ekg-fsrs-set-note-card (note card)
  (setf (ekg-note-properties note)
	(plist-put (ekg-note-properties note)
		   :fsrs/card card)))

(defun ekg-fsrs-get-note-review-log (note)
  (thread-first note
		(ekg-note-properties)
		(plist-get :fsrs/review-log)))

(defun ekg-fsrs-set-note-review-log (note review-log)
  (setf (ekg-note-properties note)
	(plist-put (ekg-note-properties note)
		   :fsrs/review-log review-log)))

(defun ekg-fsrs-prepend-note-review-log (note a-review-log)
  (thread-last note
	       (ekg-fsrs-get-note-review-log)
	       (cons a-review-log)
	       (ekg-fsrs-set-note-review-log note)))

;;; Utils
(defun ekg-fsrs-calculate-due-date (note)
  "Calculate the due date of the note from :fsrs/card."

  (thread-last note
	       (ekg-fsrs-get-note-card)
	       (fsrs-card-due)
	       (parse-iso8601-time-string)
	       (format-time-string "%F")))

(defun ekg-fsrs-update-tag (note)
  "Update the tag of the note by its due date."
  
  (let* ((tag (thread-last note
			   (ekg-fsrs-calculate-due-date)
			   (ekg-fsrs-make-tag)))
	 (tags (list tag)))

    (setf (ekg-note-tags note)
	  (thread-last (ekg-note-tags note)
		       (seq-filter (lambda (tag) (not (ekg-fsrs-tag-with-prefix-p tag))))
		       (seq-concatenate 'list tags)))))

;;; User Level
(defun ekg-fsrs-setup-note (note)
  "Setup a note for ekg-fsrs."

  (ekg-fsrs-set-note-card note (fsrs-make-card))
  (ekg-fsrs-set-note-review-log note nil)
  
  (ekg-fsrs-update-tag note)
  (ekg-save-note note))

(defun ekg-fsrs-review-note (note rating)
  (let* ((card (ekg-fsrs-get-note-card note))
	 (reviewed (fsrs-scheduler-review-card ekg-fsrs-scheduler
						 card
						 rating))
	 
	 (next-card (nth 0 reviewed))	 
	 (this-review-log (nth 1 reviewed)))

    (ekg-fsrs-set-note-card note next-card)
    (ekg-fsrs-prepend-note-review-log note this-review-log)
    (ekg-fsrs-update-tag note)
    (ekg-save-note note)))

(defun ekg-fsrs-show-notes (date)

  (cl-letf (((symbol-function #'ekg-display-note-insert) (lambda (note)
							  (insert (ekg-display-note note ekg-oneliner-note-template)))))

    (ekg-show-notes-with-tag (ekg-fsrs-make-tag date))))

(provide 'ekg-fsrs)
