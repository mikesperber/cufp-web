; ,open exceptions extended-ports tables define-record-types posix-files filenames posix-regexps posix-time

(define time-offset (* 60 60 6)) ; MET is 6 hours off east-coast time

(define (make-cufp-time t) ; kludge
  (make-time (- t time-offset))) 

(define (pad-number d n)
  (let* ((str (number->string n))
	 (sz (string-length str)))
    (if (< sz d)
	(string-append (make-string (- d sz) #\0) str)
	str)))

(define (make-directory-p path)
  (for-each (lambda (progress)
	      (if (not (directory-exists? progress))
		  (make-directory progress (integer->file-mode #o777))))
	    (path-progression path)))

(define (path-components path)
  (let loop ((chars (string->list path)) (components '()))
    (let inner ((chars chars) (rev '()))
      (cond
       ((null? chars)
	(if (null? rev)
	    (reverse components)
	    (reverse (cons (list->string (reverse rev)) components))))
       ((char=? #\/ (car chars))
	(loop (cdr chars) (cons (list->string (reverse rev)) components)))
       (else
	(inner (cdr chars) (cons (car chars) rev)))))))

(define (path-progression path)
  (let loop ((components (path-components path))
	     (acc #f)
	     (rev '()))
    (if (null? components)
	(reverse rev)
	(let ((progress 
		(if acc
		    (string-append acc "/" (car components))
		    (car components))))
	  (loop (cdr components)
		progress
		(cons progress rev))))))

(define (directory-exists? filename)
  (and (accessible? filename (access-mode execute))
       (eq? (file-info-type (get-file-info filename))
	    (file-type directory))))

(define (read-line port)
  (let loop ((l '()))
    (let ((c (read-char port)))
      
      (cond
       ((eof-object? c)
	c)
       ((char=? c #\newline)
	(list->string (reverse l)))
       (else
	(loop (cons c l)))))))

(define (regexp-replace rx str start starts-line? ends-line?
			repl-proc)
  (cond
   ((regexp-match rx str start #t starts-line? ends-line?)
    => (lambda (lis)
	 (let ((all (car lis))
	       (submatches
		(map (lambda (m)
		       (substring str (match-start m) (match-end m)))
		     (cdr lis))))
	   (string-append (substring str 0 (match-start all))
			  (apply repl-proc submatches)
			  (substring str (match-end all) (string-length str))))))
   (else
    #f)))

(define (regexp-replace-exhaustively rx str start starts-line? ends-line?
				     repl-proc)
  (let loop ((str str))
    (cond
     ((regexp-replace rx str start starts-line? ends-line? repl-proc) => loop)
     (else str))))

(define (regexps-replace-exhaustively rx-alist str)
  (let loop ((rx-alist rx-alist) (str str))
    (if (null? rx-alist)
	str
	(loop (cdr rx-alist)
	      (regexp-replace-exhaustively (caar rx-alist) str 0 #f #f (cdar rx-alist))))))

(define (read-string-literal port)
    
    (define (read-regular rev)
      (let ((c (read-char port)))
	(cond
	 ((not (char? c))
	  (error 'read-regular "premature eof"))
	 ((char=? c #\')
	  (read-quote rev))
	 ((char=? c #\\)
	  (read-escape rev))
	 (else
	  (read-regular (cons c rev))))))

    (define (read-quote rev)
      (let ((c (peek-char port)))
	(cond
	 ((eof-object? c)
	  (list->string (reverse rev)))
	 ((char=? c #\')
	  (read-char port)
	  (read-regular (cons c rev)))
	 (else
	  (list->string (reverse rev))))))

    
    (define (read-escape rev)
      (let ((c (read-char port)))
	(cond
	 ((not (char? c)) 
	  (error 'read-escape "premature eof"))
	 ((char=? c #\r)
	  (read-regular rev))
	 ((char=? c #\n)
	  (read-regular (cons #\newline rev)))
	 ((char=? c #\\)
	  (read-regular (cons #\\ rev)))
	 (else
	  (error 'read-escape "premature eof" c)))))

    (let ((c (read-char port)))
      (if (not (char=? c #\'))
	  (error 'read-string-literal "not a quote" c))
      (read-regular '())))

(define (skip-space port)
  (let loop ()
    (let ((c (peek-char port)))
      (if (char-whitespace? c)
	  (begin
	    (read-char port)
	    (loop))))))

(define (read-number-literal port)
  (let loop ((rev '()))
    (let ((c (peek-char port)))
      (cond
       ((eof-object? c)
	(string->number (list->string (reverse rev))))
       ((char-numeric? c)
	(read-char port)
	(loop (cons c rev)))
       ((char=? #\. c)
	(read-char port)
	(loop (cons c rev)))
       (else
	(string->number (list->string (reverse rev))))))))
	

; returns #t if there are more records, #f if not
(define (read-record port)

  (define (expect e)
    (let ((c (read-char port)))
      (cond
       ((eof-object? c)
	(error 'read-record "premature eof"))
       ((not (char=? c e))
	(error 'read-record "unexpected character" c)))))
  
  ;; skip until (
  (skip-space port)

  (let ((c (read-char port)))
    (cond
     ((eof-object? c)
      (error 'read-record "premature eof"))
     ((char=? c #\()
      (values))
     (else
      (error 'read-record "invalid record start" c))))

  (skip-space port)

  ;; read field
  (let loop ((rev '()))
    (let* ((c (peek-char port))
	   (f
	    (cond
	     ((eof-object? c)
	      (error 'read-record "premature eof"))
	     ((char=? c #\')
	      (read-string-literal port))
	     ((char=? c #\N)
	      (expect #\N)
	      (expect #\U)
	      (expect #\L)
	      (expect #\L)
	      #f)
	     ((char-numeric? c)
	      (read-number-literal port)))))

      (skip-space port)

      (let ((c (read-char port)))
	(cond
	 ((eof-object? c)
	  (error 'read-record "premature eof"))
	 ((char=? c #\,)
	  (skip-space port)
	  (loop (cons f rev)))
	 ((char=? c #\))
	  (skip-space port)
	  (let ((c (read-char port)))
	    (cond
	     ((eof-object? c)
	      (error 'read-record "premature eof"))
	     ((char=? c #\,) 
	      (values (reverse (cons f rev)) #t))
	     ((char=? c #\;) 
	      (values (reverse (cons f rev)) #f))
	     (else
	      (error 'read-record "invalid record end" c)))))
	 (else
	  (error 'read-record "invalid separator" c)))))))

(define (string-prefix? p s)
  (let ((lp (string-length p))
	(ls (string-length s)))
    (and (>= ls lp)
	 (string=? p (substring s 0 lp)))))

(define (read-until port t)
  (let loop ((rev '()))
    (let ((c (read-char port)))
      (cond
       ((eof-object? port)
	(error 'read-until "premature eof"))
       ((char=? c t)
	(list->string (reverse rev)))
       (else
	(loop (cons c rev)))))))

(define (substring-until s start t)
  (let ((l (string-length s)))
    (let loop ((i start))
      (cond
       ((>= i l)
	(error 'substring-until "premature eos"))
       ((char=? (string-ref s i) t)
	(substring s start i))
       (else
	(loop (+ 1 i)))))))

(define (for-each-table-record proc port table-name)
  (let loop ((ok? #f))
    (let ((l (read-line port)))
      (cond
       ((eof-object? l)
	(if (not ok?)
	    (error 'for-each-table-record "table not found")))
       ((or (not (string-prefix? "INSERT INTO `" l))
	    (not (string=? table-name (substring-until l 13 #\`))))
	(loop ok?))
       (else
	(let inner ()
	  (call-with-values
	      (lambda () (read-record port))
	    (lambda (r go-on?)
	      (proc r)
	      (if go-on?
		  (inner)
		  (loop #t)))))))))) ; there may be more
		    
(define (for-each-table-record-in-file proc file table-name)
  (call-with-input-file file
    (lambda (port)
      (for-each-table-record proc port table-name))))

(define-record-type :node
  (make-node nid vid type title body created status url)
  node?
  (nid node-nid)
  (vid node-vid)
  (type node-type)
  (title node-title)
  (body node-body set-node-body!)
  (created node-created)
  (status node-status)
  (url node-url set-node-url!))

(define-record-type :user
  (make-user uid name mail picture)
  user?
  (uid user-uid)
  (name user-name)
  (mail user-mail)
  (picture user-picture)
  (node user-node set-user-node!))

(define (user-full-name user)
  (cond
   ((user-node user) =>
    (lambda (node)
      (or
       (regexp-replace user-title-regexp
		       (node-title node)
		       0 #f #f
		       values)
       (node-title node))))
   (else (user-name user))))

(define-record-type :file
  (make-file fid path mime)
  file?
  (fid file-fid)
  (path file-path)
  (mime file-mime))

(define-record-type :session
  (make-session vid node speaker file time time-2 details format)
  session?
  (vid session-vid)
  (node session-node)
  (speaker session-speaker)
  (file session-file)
  (time session-time)
  (time-2 session-time-2)
  (details session-details)
  (format session-format))

(define (read-user-table file)
  (let ((user-table (make-integer-table)))
    (for-each-table-record-in-file
     (lambda (rec)
       (apply 
	(lambda (uid name pass mail mode sort threshold theme signature signature_format created access login status timezone language picture init data timezone_name)
	  (display "User: ") (display uid) (newline)
	  (table-set! user-table
		      uid
		      (make-user uid name mail picture)))
	rec))
     file
     "users")
    user-table))

(define (read-file-table file)
  (let ((file-table (make-integer-table)))
    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (fid uid filename filepath filemime filesize status timestamp)
	  (table-set! file-table
		      fid
		      (make-file fid filepath filemime)))
	rec))
     file
     "files")
    file-table))
  

(define (read-node-table user-table file)
  (let ((node-table (make-integer-table)))
    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (nid vid type language title uid status created changed comment promote moderate sticky tnid translate)
	  (display "Node: ") (display nid) (newline)
	  (let* ((type (string->symbol type))
		 (node (make-node nid vid type title #f created status #f)))
	    (if (eq? type 'user_profile)
		(let ((user (table-ref user-table uid)))
		  (set-user-node! user node)))
	    (table-set! node-table vid node)))
	rec))
     file
     "node")

    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (nid vid uid title body teaser log timestamp format)
	  (cond
	   ((table-ref node-table nid)
	    => (lambda (node)
		 (display "Body for: ") (display nid) (newline)
		 (set-node-body! node body)))))
	rec))
     file
     "node_revisions")

    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (pid src dst language)
	  (if (string-prefix? "node/" src)
	      (let ((nid (string->number (substring src 5 (string-length src)))))
		(cond
		 ((table-ref node-table nid)
		  => (lambda (node)
		       (display "URL for: ") (display nid) (display " is ") (display dst) (newline)
		       (set-node-url! node dst)))))))
	rec))
     file
     "url_alias")

    node-table))

(define (read-session-table user-table file-table node-table file)
  (let ((session-speaker-table (make-integer-table))
	(session-table (make-integer-table))
	(session-file-table (make-integer-table)))

    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (vid nid delta uid)
	  (cond
	   ((not uid) (values))
	   ((table-ref user-table uid)
	    => (lambda (user)
		 (table-set! session-speaker-table vid user)))))
	rec))
     file
     "content_field_session_speaker")

    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (vid nid delta fid file-list data)
	  (cond
	   ((not fid) (values))
	   ((table-ref file-table fid)
	    => (lambda (file)
		 (table-set! session-file-table vid file)))))
	rec))
     file
     "content_field_session_file")

    (for-each-table-record-in-file
     (lambda (rec)
       (apply
	(lambda (nid vid time time-2 details format)
	  (table-set! session-table vid
		      (make-session vid
				    (table-ref node-table nid)
				    (table-ref session-speaker-table vid)
				    (table-ref session-file-table vid)
				    (make-cufp-time time) (make-cufp-time time-2) details format)))
	rec))
     file
     "content_type_session")

    session-table))

(define-record-type :page
  (make-page type url headers body)
  page?
  (type page-type)
  (url page-url)
  (headers page-headers)
  (body page-body))

(define (apply-page-replacements page-replacements page)
  (make-page (page-type page)
	     (page-url page)
	     (page-headers page)
	     (regexps-replace-exhaustively page-replacements (page-body page))))

(define (write-to-url output-dir url proc)
  (let ((dir (file-name-directory url))
	(name (file-name-nondirectory url)))
    (let* ((full-dir (string-append output-dir "/" dir))
	   (full-name (string-append full-dir "/" name ".md")))
      (display "Writing ") (display (string-append full-dir "/" full-name)) (newline)
      (make-directory-p full-dir)
      (call-with-output-file full-name proc))))

(define (write-front-matter port alist)
  (display "---" port)
  (newline port)
  (for-each (lambda (p)
	      (display (car p) port)
	      (display ": \"" port)
	      (display (cdr p) port)
	      (display "\"" port)
	      (newline port))
	    alist)
  (display "---" port)
  (newline port))

(define (write-page-to-jekyll output-dir page-replacements page)
  (write-to-url output-dir
		(page-url page)
		(lambda (port)
		  (write-front-matter port (page-headers page))
		  (display (page-body page) port)
		  (newline port))))

(define (write-page-to-json port page)
  (write-json-object port
		     (append (list (cons "type" (symbol->string (page-type page)))
				   (cons "url" (page-url page))
				   (cons "body" (page-body page)))
			     (page-headers page))))

(define (write-json-object port alist)
  (display "{" port) (newline port)
  (let loop ((alist alist))
    (if (pair? alist)
	(let ((p (car alist)))
	  (display-json-string port (car p))
	  (display " : " port)
	  (display-json-string port (cdr p))
	  (if (pair? (cdr alist))
	      (begin (display "," port) (newline port)))
	  (loop (cdr alist)))))
  (newline port)
  (display "}" port) (newline port))

(define (display-json-string port s)
  (display "\"" port)
  (for-each (lambda (c)
	      (case c
		((#\\) (display "\\\\" port))
		((#\") (display "\\\"" port))
		((#\tab) (display "\\t" port))
		((#\page) (display "\\f" port))
		((#\newline) (display "\\\n" port))
		((#\return) (display "\\\r" port))
		(else (display c port))))
	    (string->list s))
  (display "\"" port))

(define (news-page node)
  (let ((d (time->utc-date (make-cufp-time (node-created node))))
	(name (file-name-nondirectory (node-url node)))
	(title (node-title node)))
      (make-page 'news
		 (string-append "_posts/"
				(number->string (+ 1900 (date-year d)))
				"-"
				(pad-number 2 (+ 1 (date-month d)))
				"-"
				(pad-number 2 (date-month-day d))
				"-"
				name)
		 (list (cons "layout" "news")
		       (cons "title" title))
		 (node-body node))))

(define (format-time-date t)
  (format-date "%A, %B %d, %Y" (time->local-date t)))

(define (format-time/clock t)
  (format-date "%H:%M" (time->local-date t)))

(define (format-time-range t1 t2)
  (string-append (format-time-date t1)
		 " - "
		 (format-time/clock t1)
		 " - "
		 (format-time/clock t2)))

(define user-title-regexp (make-regexp "(.*) - Profile [0-9]+"
				       (regexp-option submatches)
				       (regexp-option extended)))

(define (extract-cufp-website file)
  (let* ((user-table (read-user-table file))
	 (file-table (read-file-table file))
	 (node-table (read-node-table user-table file))
	 (session-table (read-session-table user-table file-table node-table file))
	 (pages '()))

    (define page-replacements
      (list
       (cons (make-regexp "href='<\\?php print url\\('user/([0-9]+)'\\);\\?>'"
			  (regexp-option submatches)
			  (regexp-option extended))
	     (lambda (uid)
	       (let ((user (table-ref user-table (string->number uid))))
		 (string-append "href=\"http://cufp.org/users/"
				(user-name user)
				".html\""))))
       (cons (make-regexp "\\[([^]]*)\\]\\(http://cufp\\.org/([^.)]*)\\)"
			  (regexp-option submatches)
			  (regexp-option extended))
	     (lambda (text dir)
	       (string-append "[" text "](http://cufp.org/" dir ".html)")))))

    (define (add-page! type url headers body)
      (set! pages 
	    (cons (make-page type url headers body)
		  pages)))

    (display "Processing nodes ...") (newline)
    (table-walk 
     (lambda (vid node)
       (case (node-type node)
	 ((job page conference_data conference_info videos)
	  (cond
	   ((node-url node)
	    => (lambda (url)
		 (add-page! 'node
			    url
			    (list (cons "layout" "node")
				  (cons "title" (node-title node)))
			    (node-body node))))))
	 ((news)
	  (set! pages (cons (news-page node) pages)))))
     node-table)

    (display "Processing sessions ...") (newline)
    (table-walk
     (lambda (vid session)
       (cond
	((session-node session)
	 => (lambda (node)
	      (cond
	       ((node-url node)
		=> (lambda (url)
		     (let* ((front-matter
			     (list 
			      (cons "layout" "session")
			      (cons "title" (node-title node))
			      (cons "time" (format-time-range (session-time session)
							      (session-time-2 session)))))
			    (front-matter
			     (cond
			      ((session-speaker session)
			       => (lambda (speaker)
				    (cons (cons "speaker" (user-full-name speaker)) front-matter)))
			      (else
			       front-matter)))
			    (front-matter
			     (cond
			      ((session-file session)
			       => (lambda (file)
				    (cons (cons "file" (file-path file))
					  front-matter)))
			      (else
			       front-matter))))
		       (add-page! 'session
				  url
				  front-matter
				  (cond
				   ((session-details session)
				    => (lambda (details)
					 (string-append (node-body node) "\n" details)))
				   (else (node-body node))))))))))))
     session-table)
    
    (display "Processing users ...") (newline)
    (table-walk
     (lambda (uid user)
       (let ((node (user-node user)))
	 (if (node? node)
	     (add-page! 'user
			(string-append "users/" (user-name user))
			(list
			 (cons "layout" "user")
			 (cons "title" (user-full-name user)))
			(node-body node)))))
     user-table)

    (display "Applying page replacements ...") (newline)
    (map (lambda (page)
	   (apply-page-replacements page-replacements page))
	 pages)))

(define (write-pages-to-jekyll pages output-dir)
  (display "Writing pages ") (newline)
  (for-each (lambda (page)
	      (write-page-to-jekyll output-dir page))
	    pages))

(define (write-pages-to-json pages output-file)
  (call-with-output-file output-file
    (lambda (port)
      (for-each (lambda (page)
		  (write-page-to-json port page))
		pages))))
