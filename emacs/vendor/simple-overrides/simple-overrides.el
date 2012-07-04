(defun my-previous-matching-history-element (regexp n)
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Previous element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history
					(car minibuffer-history-search-history))))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (if minibuffer-history-search-history
		   (car minibuffer-history-search-history)
		 (error "No previous history search regexp"))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (catch 'no-history-element
    (unless (zerop n)
      (if (and (zerop minibuffer-history-position)
	       (null minibuffer-text-before-history))
	  (setq minibuffer-text-before-history
		(minibuffer-contents-no-properties)))
      (let ((history (symbol-value minibuffer-history-variable))
	    (case-fold-search
	     (if (isearch-no-upper-case-p regexp t) ; assume isearch.el is dumped
		 ;; On some systems, ignore case for file names.
		 (if (memq minibuffer-history-variable
			   minibuffer-history-case-insensitive-variables)
		     t
		   ;; Respect the user's setting for case-fold-search:
		   case-fold-search)
	       nil))
	    prevpos
	    match-string
	    match-offset
	    (pos minibuffer-history-position))
	(while (/= n 0)
	  (setq prevpos pos)
	  (setq pos (min (max 1 (+ pos (if (< n 0) -1 1))) (length history)))
	  (when (= pos prevpos)
	    (throw 'no-history-element nil))
;	    (error (if (= pos 1)
;		       "No later matching history item"
;		     "No earlier matching history item")))
	  (setq match-string
		(if (eq minibuffer-history-sexp-flag (minibuffer-depth))
		    (let ((print-level nil))
		      (prin1-to-string (nth (1- pos) history)))
		  (nth (1- pos) history)))
	  (setq match-offset
		(if (< n 0)
		    (and (string-match regexp match-string)
			 (match-end 0))
		  (and (string-match (concat ".*\\(" regexp "\\)") match-string)
		       (match-beginning 1))))
	  (when match-offset
	    (setq n (+ n (if (< n 0) 1 -1)))))
	(setq minibuffer-history-position pos)
	(goto-char (point-max))
	(delete-minibuffer-contents)
	(insert match-string)
	(goto-char (+ (minibuffer-prompt-end) match-offset))))
    (if (memq (car (car command-history)) '(my-previous-matching-history-element
					    my-next-matching-history-element))
	(setq command-history (cdr command-history)))))

(defun my-next-matching-history-element (regexp n)
  (interactive
   (let* ((enable-recursive-minibuffers t)
	  (regexp (read-from-minibuffer "Next element matching (regexp): "
					nil
					minibuffer-local-map
					nil
					'minibuffer-history-search-history
 					(car minibuffer-history-search-history))))
     ;; Use the last regexp specified, by default, if input is empty.
     (list (if (string= regexp "")
	       (if minibuffer-history-search-history
		   (car minibuffer-history-search-history)
		 (error "No previous history search regexp"))
	     regexp)
	   (prefix-numeric-value current-prefix-arg))))
  (my-previous-matching-history-element regexp (- n)))

(defun my-next-complete-history-element (n)
  (interactive "p")
  (let ((point-at-start (point)))
    (my-next-matching-history-element
     (concat
      "^" (regexp-quote (buffer-substring (minibuffer-prompt-end) (point))))
     n)
    ;; next-matching-history-element always puts us at (point-min).
    ;; Move to the position we were at before changing the buffer contents.
    ;; This is still sensical, because the text before point has not changed.
    (goto-char point-at-start)))

(defun my-previous-complete-history-element (n)
  (interactive "p")
  (my-next-complete-history-element (- n)))
