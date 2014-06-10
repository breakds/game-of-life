;;;; game.lisp

(defpackage #:breakds.game-of-life
  (:nicknames #:game-of-life)
  (:use #:cl
        #:parenscript
        #:realispic))

(in-package #:breakds.game-of-life)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-jsx-reader))

(defun make-board ()
  (make-hash-table :test #'equal))

(defun inc-cell (key board)
  (incf (gethash key board 0)))

(defun initialize (rows cols)
  (let ((board (make-board)))
    (loop for i below rows
       do (loop for j below cols
             do (let ((k (random 1.0)))
                  (when (< k 0.3)
                    (inc-cell (list i j) board)))))
    board))

(defun initialize-block (rows cols)
  (let ((board (make-board)))
    (inc-cell (list 1 1) board)
    (inc-cell (list 1 2) board)
    (inc-cell (list 2 1) board)
    (inc-cell (list 2 2) board)
    board))

(defun initialize-glider (rows cols)
  (let ((board (make-board)))
    (inc-cell (list 6 6) board)
    (inc-cell (list 7 7) board)
    (inc-cell (list 7 8) board)
    (inc-cell (list 6 8) board)
    (inc-cell (list 5 8) board)
    board))

(defun initialize-blinker (rows cols)
  (let ((board (make-board)))
    (inc-cell (list 5 5) board)
    (inc-cell (list 6 5) board)
    (inc-cell (list 7 5) board)
    board))

(defun board-response (board rows cols)
  (loop for i below rows
     collect (loop for j below cols
                collect (gethash (list i j) board 0))))

(defun in-range (y x rows cols)
  (and (>= y 0) (>= x 0) (< y rows) (< x cols)))

(defun alive-next-turn (num previous)
  (case num
    (3 t)
    (2 (= previous 1))
    (t nil)))

(defun advance-board (board rows cols)
  (let ((counter (make-board)))
    (loop 
       for key being the hash-keys of board
       for i = (first key)
       for j = (second key)
       do (loop for y from (1- i) to (1+ i)
             do (loop for x from (1- j) to (1+ j)
                   when (and (not (and (= y i) (= x j)))
                             (in-range y x rows cols))
                   do (inc-cell (list y x) counter))))
    (let ((result (make-board)))
      (loop 
         for key being the hash-keys of counter
         for num being the hash-values of counter
         when (alive-next-turn num (gethash key board 0))
         do (inc-cell key result))
      result)))

(defun print-board (board rows cols)
  (loop for i below rows
     do (format t "~{~a ~}~%" 
                (loop for j below cols
                   collect (gethash (list i j) board 0))))
  (format t "------------------------------~%"))

(def-rpc refresh (rows cols init)
  (if init
      (setf (hunchentoot:session-value 'board) (initialize rows cols))
      (let ((original (hunchentoot:session-value 'board)))
        (setf (hunchentoot:session-value 'board)
              (advance-board original rows cols))))
  (board-response (hunchentoot:session-value 'board)
                  rows cols))

(def-widget board-cell (size status)
    ()
  #jsx(:td ((style (create "width" (+ size "px")
                           "height" (+ size "px")
                           "border" "1px solid black"
                           "background-color" (if (= 0 status)
                                                  "white"
                                                  "black"))))))

            
(def-widget board (rows cols)
    ((state (matrix (array (array 0 1) (array 1 0))))
     (refresh-matrix ()
                     (with-rpc (refresh rows cols false)
                       (chain this (set-state (create matrix rpc-result)))))
     (component-will-mount ()
                           (with-rpc (refresh rows cols t)
                             (chain this (set-state (create matrix rpc-result))))
                           (set-interval (@ this refresh-matrix) 1000)))
  #jsx(:div 
       ()
       (:a ((href "http://en.wikipedia.org/wiki/Conway's_Game_of_Life"))
           "Wikipedia for Conway's Game of Life")
       (:table ((style (create "border-collapse" "collapse")))
               (chain (local-state matrix)
                      (map 
                       (lambda (row)
                         (:tr () 
                              (chain row 
                                     (map 
                                      (lambda (cell)
                                        (:board-cell ((size 10) (status cell)))))))))))))


(def-realispic-app (game-of-life :title "Conway's Game of Life"
                                 :libs ("http://fb.me/react-0.10.0.js"
				  	 "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
                                 :port 14387)
  #jsx(:board ((rows 20) (cols 20))))
                                        

(eval-when (:compile-toplevel :load-toplevel :execute)
  (disable-jsx-reader))

