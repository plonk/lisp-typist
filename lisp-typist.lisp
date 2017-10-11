(ql:quickload :cl-charms)

(defpackage lisp-typist
  (:use charms/ll common-lisp)
  (:export main)
  (:shadow log))

(in-package :lisp-typist)

(defconstant +current-word-untyped+ 1)
(defconstant +current-word-typed+ 2)
(defconstant +current-word-missed+ 3)
(defconstant +word-untyped+ 4)
(defconstant +word-typed+ 5)
(defconstant +word-missed+ 6)
(defconstant +getch-timeout-ms+ 500)

(defparameter *source-words* nil)
(defparameter *problem* nil)
(defparameter *answer* nil)
(defparameter *current-row-index* nil)
(defparameter *current-word-index* nil)
(defparameter *buffer*
  (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
(defparameter *test-start-time* nil)

(defmacro with-attr (attr &rest body)
  `(progn
     (attron ,attr)
     ,@body
     (attroff ,attr)))

(defun init-charms ()
  (initscr)
  (noecho)
  (timeout +getch-timeout-ms+)
  (keypad *stdscr* 1)
  (raw)
  ;;(curs-set 0)
  (unless (= 1 (has-colors))
    (print "Error: this terminal does not support colors")
    (sb-ext:exit))
  (start-color)
  (init-pair +current-word-untyped+ COLOR_BLACK COLOR_GREEN)
  (init-pair +current-word-typed+   COLOR_WHITE COLOR_GREEN)
  (init-pair +current-word-missed+  COLOR_RED   COLOR_GREEN)
  (init-pair +word-untyped+ COLOR_WHITE COLOR_BLACK)
  (init-pair +word-typed+   COLOR_BLUE  COLOR_BLACK)
  (init-pair +word-missed+  COLOR_RED   COLOR_BLACK))

(defun format-time (&optional (universal-time (get-universal-time)))
  (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month date
            hour minute second)))

(defun log (fmt &rest args)
  (when (not *log-file*)
    (return))
  (format *log-file* "~A " (format-time))
  (apply #'format (append (list *log-file* fmt) args))
  (format *log-file* "~%")
  (finish-output *log-file*))

(defun initialize ()
  (setf *random-state* (make-random-state t))
  (setf *log-file* (open "log"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create))
  (init-charms)
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (declare (ignore condition hook))
          (finalize)
          ;;(sb-ext:quit :recklessly-p t)
          ))
  (load-source-words)
  (let ((scene #'typing-scene))
    (loop
     while scene
     do
     (setf scene (funcall scene)))))

;; データ構造がよくないから、コードが無用に複雑に…。
(defun advance-word ()
  "単語位置を進めて t を返す。もう進められなかったら nil を返す。"
  (let ((row (nth *current-row-index* *problem*)))
    (if (= (length row) (1+ *current-word-index*)) ; row maxed
        (if (= (length *problem*) (1+ *current-row-index*))
            nil ; no more words to type
          (progn
            (incf *current-row-index*)
            (setf *current-word-index* 0)
            t))
      (progn
        (incf *current-word-index*)
        t))))

(defun show-message (str)
  (clear)
  (addstr str)
  (getch))

(defun flatten1 (lst)
  (reduce #'append lst))

(defstruct result
  (raw-wpm 0)
  (corrected-wpm 0)
  (raw-cpm 0)
  (corrected-cpm 0)
  (mistakes ()))

(defun get-done-words (llist)
  (append (flatten1 (subseq llist 0 *current-row-index*))
          (subseq (nth *current-row-index* llist)
                  0 (1+ *current-word-index*))))

(defun calculate-result (problem answer)
  (let ((res (make-result))
        (words1 (get-done-words problem))
        (words2 (get-done-words answer)))
    (let ((correctly-typed (loop for w1 in words1
                                 for w2 in words2
                                 if (string= w1 w2)
                                 collect w1
                                 else
                                 do (push (list w1 w2) (result-mistakes res))
                                 end)))
      (setf (result-raw-wpm res)       (length words2)
            (result-corrected-wpm res) (length correctly-typed)
            (result-raw-cpm res)       (reduce #'+ (mapcar #'length words2))
            (result-corrected-cpm res) (reduce #'+ (mapcar #'length correctly-typed))
            (result-mistakes res)      (reverse (result-mistakes res)))
      res)))

(defun movex (newx)
  (let ((y nil) (x nil))
    (getyx *stdscr* y x)
    (move y newx)))

(defun addstr-centered (&rest lines)
  (dolist (str lines)
    (let  ((left-margin (truncate (max 0 (- *cols* (length str))) 2)))
      (movex left-margin)
      (addstr str))))

(defun result-scene ()
  (clear)
  (move 5 0)
  (let* ((res (calculate-result *problem* *answer*))
         (nmistakes (length (result-mistakes res))))
    (with-attr
     A_BOLD
     (addstr-centered
      (format nil "Your score: ~A CPM (that is ~A WPM)~%~%"
              (result-corrected-cpm res)
              (result-corrected-wpm res))))
    (if (= 0 (length (result-mistakes res)))
        (addstr-centered
         (format nil "Congratulations! You typed all 103 words correctly!~%~%"))
      (progn
        (addstr-centered
         (format nil "In reality, you typed ~A CPM,~%" (result-raw-cpm res))
         (format nil "but you made ~A mistake~:p (out of ~A word~:p),~%"
                 nmistakes (length (get-done-words *problem*)))
         (format nil "which was not counted in the corrected scores.~%~%"))
        (addstr-centered
         (format nil "Your mistake~p ~a:~%" nmistakes (if (= 1 nmistakes) "was" "were")))
        (dolist (pair (result-mistakes res))
          (destructuring-bind
              (correct typed)
              pair
            (addstr-centered
             (format nil "Instead of \"~a\", you typed \"~a\".~%" correct typed))))
        (addstr-centered (format nil "~%"))))
    (addstr-centered
     (format nil "I advise you to take a 2 minute break now.~%~%")
     (format nil "Type Ctrl+C to quit, Ctrl+R to restart.~%")
    ))
  (timeout -1)
  (curs-set 0)
  (flet ((clean-up ()
                   (timeout +getch-timeout-ms+)
                   (curs-set 1)))
    (loop for keycode = (getch)
          do
          (cond
           ((= keycode #x12) ; c-r
            (clean-up)
            (return-from result-scene #'typing-scene))
           ((= keycode #x03) ; c-c
            (clean-up)
            (return-from result-scene nil))))))

(defun timeup-p ()
  (and *test-start-time*
       (<= 60 (truncate (- (get-internal-real-time) *test-start-time*)
                        internal-time-units-per-second))))

(defun make-adjustable-string ()
  (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))

(defmacro current-word-in (llist)
  `(nth *current-word-index* (nth *current-row-index* ,llist)))

(defun typing-scene ()
  (create-problem)
  ;; *problem* にパラレルな空文字列のリストのリスト。
  (setf *answer*
        (mapcar (lambda (row)
                  (mapcar (lambda (word) "") row))
                *problem*))
  (setf *current-row-index* 0
        *current-word-index* 0
        *test-start-time* nil
        *buffer* (make-adjustable-string))

  (loop do
        (show-screen)
        (let ((keycode (getch)))
          (cond
           ((= ERR keycode) ; タイムアウト。何もせず画面更新処理に進む。

            nil)

           ((= #x20 keycode)

            (if (string= *buffer* "")
                nil ; 単語の先頭でのスペースは無視する。
              (progn
                (setf (current-word-in *answer*) *buffer*
                      *buffer* (make-adjustable-string))

                (when (timeup-p)
                  (return-from typing-scene #'result-scene))

                (unless (advance-word)
                  (show-message "もう単語無いです…。")
                  (return-from typing-scene nil)))))

           ((<= #x21 keycode #x7e)

            (unless *test-start-time*
              (setf *test-start-time* (get-internal-real-time)))
            (vector-push-extend (code-char keycode) *buffer*))

           ((= #x08 keycode)         ; ^H, assume to be Ctrl+Backspace

            (setf *buffer* (make-adjustable-string)))

           ((or (= KEY_BACKSPACE keycode) ; this comes when in keypad mode
                (= #x7f keycode))         ; ^?

            (if (string= "" *buffer*)
                (beep)
              (vector-pop *buffer*)))

           ((= #x12 keycode) ; Ctrl+R
            (return-from typing-scene #'typing-scene))

           ((= #x03 keycode) ; Ctrl+C
            (return-from typing-scene nil))

           (t
            (log "keycode ~A ignored" keycode))))))

(defun add-current-word (word buffer)
  (loop for i below (length word)
        do
        (let ((char (aref word i)))
          (if (< i (length buffer))
              (with-attr (color-pair (if (equal char (aref buffer i))
                                         +current-word-typed+
                                       +current-word-missed+))
                         (addch (char-code char)))
            (with-attr (color-pair +current-word-untyped+)
                       (addch (char-code char)))))))

(defun format-time-left ()
  (if *test-start-time*
      (let ((seconds-left
             (- 60 (truncate (- (get-internal-real-time) *test-start-time*)
                             internal-time-units-per-second))))
        (if (< 0 seconds-left)
            (format nil "~D" seconds-left)
          "finish word"))
    "60"))

(defun show-screen ()
  (clear)
  (let ((top-margin (truncate (- *lines* 5) 2))
        (left-margin (truncate (- *cols* 35) 2)))

    (move (- top-margin 2) left-margin)
    (if *test-start-time*
        (let ((res (calculate-result *problem* *answer*))
              (time-elapsed (truncate (- (get-internal-real-time) *test-start-time*)
                                      internal-time-units-per-second)))
          (if (< 0 time-elapsed)
              (addstr (format nil "CPM: ~A WPM: ~A Time left: ~A"
                              (truncate (* (result-corrected-cpm res) (/ 60 time-elapsed)))
                              (truncate (* (result-corrected-wpm res) (/ 60 time-elapsed)))
                              (format-time-left)))
            (addstr (format nil "CPM: ? WPM: ? Time left: ~A"
                            (format-time-left)))))
      (addstr (format nil "CPM: ? WPM: ? Time left: ~A"
                      (format-time-left))))

    (dotimes (h 3)
      (let ((i (if (= 0 *current-row-index*)
                   h
                 (- h 1))))
        (move (+ h top-margin) left-margin)
        (let ((row (nth (+ i *current-row-index*) *problem*)))
          (dotimes (j (length row))
            (let ((word (nth j row)))
              (if (and (= 0 i) (= j *current-word-index*))
                  (add-current-word word *buffer*)
                (cond
                 ((string= "" (nth j (nth (+ i *current-row-index*) *answer*)))
                  (with-attr (color-pair +word-untyped+)
                             (addstr word)))
                 ((string= word (nth j (nth (+ i *current-row-index*) *answer*)))
                  (with-attr (color-pair +word-typed+)
                             (addstr word)))
                 (t (with-attr (color-pair +word-missed+)
                               (addstr word))))))
            (addstr " ")))))

    (move (+ 4 top-margin) (- (truncate *cols* 2) (length *buffer*)))
    (addstr *buffer*)
  ))

(defun join (lst)
  "lst を空白繋ぎの文字列にする。"
    (format nil "~{~A~^ ~}" lst))

(defun create-problem ()
  "*source-words* から単語をランダムの500個選択し、リストとして *problem* に設定する。"
  (assert *source-words*)

  (setf *problem* nil)
  (let ((bag nil)
        (nwords (length *source-words*)))
    (dotimes (i 500)
      (let ((word (nth (random nwords) *source-words*)))
        (when (< 35 (+ (length (join bag)) (length word)))
          (push bag *problem*)
          (setf bag nil))
        (push word bag)))
    (push bag *problem*))
  (setf *problem* (reverse *problem*)))

(defun load-source-words ()
  "words.txt から単語を読み込み *source-words* にリストとして設定する。"
  (setf *source-words*
        (with-open-file (in "words.txt")
                        (loop for line = (read-line in nil)
                              while line
                              collect line))))

(defun finalize ()
  (endwin))

(defun main ()
  (initialize)
  (finalize)
  (sb-ext:exit))
