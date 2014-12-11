;;; clj-fly.el --- Flycheck: Clojure support    -*- lexical-binding: t; -*-
;;; Commentary:
;;; So far, this only invokes eastwood.  It works best if flycheck-pos-tip is used, so as not to clash with Cider doc messages.

;;; Code:

(require 'cider-client)
(require 'flycheck)


(setq cmdf-ew "(do (require 'eastwood.lint)
    (eastwood.lint/eastwood {:source-paths [\"src\"] :namespaces ['%s] } ))")
(defun parse-ew (out)
  "Parse an output chunk from eastwood: OUT."
  (delq nil
	(mapcar (lambda (s)
	       (let ((r "^\\([^[:space:]]+\\)\\:\\([[:digit:]]+\\)\\:\\([[:digit:]]+\\)\\:[[:space:]]*\\(.*\\)"))
		 (if (string-match r s)
		     (list
		      (match-string 1 s)                     ;; file
		      (string-to-number (match-string 2 s))  ;; line
		      (string-to-number (match-string 3 s))  ;; col
		      (match-string 4 s)                     ;; msg
		      ))))
		(split-string out "\n"))))

(setq cmdf-tc "(do (require 'clojure.core.typed)
    (clojure.string/join \"__EOE__\"(map (fn [e]
      (let [env (:env (ex-data e))]
         (str (:file env) \":\" (:line env) \":\" (:column env) \":\"
                   (.getMessage e))))  (:delayed-errors (t/check-ns-info '%s)))))")

(defun parse-tc (out)
  "Parse an output chunk from typed clojure: OUT."
  (delq nil
	(mapcar (lambda (s)
	       (let ((r "^\\([^[:space:]]+\\)\\:\\([[:digit:]]+\\)\\:\\([[:digit:]]+\\)\\:[[:space:]]*\\(.*\\)"))
		 (if (string-match r s)
		     (list
		      (match-string 1 s)                     ;; file
		      (string-to-number (match-string 2 s))  ;; line
		      (string-to-number (match-string 3 s))  ;; col
		      (match-string 4 s)                     ;; msg
		      ))))
	     (split-string out "__EOE__"))))



(defun flycheck-clj-cider-start (checker callback)
  "Begin flychecking, with CHECKER and CALLBACK."
  (let* ((buffer (current-buffer))
	 (fname  (buffer-file-name buffer))
	 (ns     (cider-current-ns))
	 (cmd-ew (format cmdf-ew ns))
	 (cmd-tc (format cmdf-tc ns))
	 (errors ()))

    (cider-eval cmd-ew
     (nrepl-make-response-handler
      buffer
      (lambda (_buffer _value)
	  (message "Finished eastwood check."))
      (lambda (_buffer out)
	(mapc (lambda (w) (pcase-let* ((`(,file ,line ,column ,msg) w))
			      (push
			       (flycheck-error-new-at line column 'error msg
						      :checker checker
						      :buffer buffer
						      :filename fname)
			       errors)))
		(parse-ew out))
	)
      (lambda (_buffer err))
      '())
     )

    (cider-eval cmd-tc
     (nrepl-make-response-handler
      buffer
      (lambda (_buffer value)
	(let ((parsed-tc ))
	  (mapc (lambda (w) (pcase-let* ((`(,file ,line ,column ,msg) w))
			      (push
			       (flycheck-error-new-at line column 'error msg
						      :checker checker
						      :buffer buffer
						      :filename fname)
			       errors)))
		(parse-tc value))
	  (message "Finished core.typed check.")
	  ))
      (lambda (_buffer out))
      (lambda (_buffer err))
      '())
     )

    (cider-eval "true"
		(nrepl-make-response-handler
		 buffer
		 (lambda (_buffer _value)
		   (message "Finished all clj checks.")
		   (funcall callback 'finished errors))
		 (lambda (_buffer out))
		 (lambda (_buffer err))
		 '()))
    ))



(flycheck-define-generic-checker 'clojure-cider-checker
  "A syntax checker for Clojure using Cider"
  :start #'flycheck-clj-cider-start
  :modes '(clojure-mode)
  )

(add-to-list 'flycheck-checkers 'clojure-cider-checker)

(provide 'clj-fly)
;;; clj-fly.el ends here

