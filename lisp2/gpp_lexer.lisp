(defvar count 0)
(defun checkKeywords (currentList)
  (cond
    ((string-equal currentList "AND") "KW_AND")
    ((string-equal currentList "OR") "KW_OR")
    ((string-equal currentList "NOT") "KW_NOT")
    ((string-equal currentList "EQUAL") "KW_EQUAL")
    ((string-equal currentList "LESS") "KW_LESS")
    ((string-equal currentList "NIL") "KW_NIL")
    ((string-equal currentList "LIST") "KW_LIST")
    ((string-equal currentList "APPEND") "KW_APPEND")
    ((string-equal currentList "CONCAT") "KW_CONCAT") 
    ((string-equal currentList "SET") "KW_SET") 
    ((string-equal currentList "DEFFUN") "KW_DEFFUN") 
    ((string-equal currentList "FOR") "KW_FOR")
    ((string-equal currentList "IF") "KW_IF") 
    ((string-equal currentList "EXIT") "KW_EXIT") 
    ((string-equal currentList "LOAD") "KW_LOAD")
    ((string-equal currentList "DISP") "KW_DISP")
    ((string-equal currentList "TRUE") "KW_TRUE")
    ((string-equal currentList "FALSE") "KW_FALSE") 
    (t "nothing")))

  (defun checkOperators (currentList)
  (cond
    ((string-equal currentList "+") "OP_PLUS")
    ((string-equal currentList "-") "OP_MINUS")
    ((string-equal currentList "/") "OP_DIV")
    ((string-equal currentList "(") "OP_OP")
    ((string-equal currentList ")") "OP_CP")
    ((string-equal currentList ",") "OP_COMMA")
    ((string-equal currentList "*") "OP_MULT")
    (t "nothing")))

(defun split-input-into-words (sentence)
 ;"Separates words in parentheses by spaces and inserts parentheses as separate characters."
  (let* ((inside-parentheses nil);Are we in parentheses
         (words '()) ;will store words and parenthesis characters
         (current-word "")) ;empty series to hide the current word

   ; We start a loop to process each character of the sentence.
    (loop for char across sentence
          do (cond
               ((char= char #\() ;eğer ( ise
                (if (plusp (length current-word)) 
                    (push current-word words))
                (setf current-word "")
                (push (string #\() words)
                (setf inside-parentheses nil))
               ((char= char #\))
                (if (plusp (length current-word))
                    (push current-word words))
                (setf current-word "")
                (push (string #\)) words)
                (setf inside-parentheses nil))
               ((and (char= char #\Space) (not inside-parentheses))
                (if (plusp (length current-word))
                    (push current-word words))
                (setf current-word ""))
               (t
                (setf current-word (concatenate 'string current-word (string char))))))
    (if (plusp (length current-word))
        (push current-word words))
    (reverse words)))

(defun process-word (word)
  (let ((result (checkKeywords word)))
    (if (string= result "nothing")
        (setq count (1+ count)) ;count is incremented by 1
        (format t "~a~% ~a~%" word result))))

(defun process-operator(word)
  (let ((result (checkOperators word)))
    (if (string= result "nothing")
         (setq count (1+ count)) ; count is incremented by 1
        (format t "~a~% ~a~%" word result))))

(defun is-float-number (word)
 (let* ((len (length word));character count detection
        (has-b-between-numbers nil);Is there a b between 2 numbers or not? 
        (b-count 0)) ;Number of letters 'b' in the word

        ;b check if between numbers
        (loop for i from 1 to (1- len)
        until has-b-between-numbers
        do (if (char= (char word i) #\b)
        (setq has-b-between-numbers t)))

       ;Number of letters 'b' in the wor
        (loop for char across word
        when (char= char #\b)
        do (incf b-count))

        ;this code checks for letters other than b              
        (let ((notb t))
        (loop for char across word
        do (if (not (or (char= char #\b)
        (digit-char-p char)))
        (setq notb nil)))

 (if (and (< len 3))
    (setq count (1+ count)); (format t "word count less than 3")
        (if(char= (char word 0) #\b)
          (setq count (1+ count))
                (if (char= (char word (1- len)) #\b)
                   (setq count (1+ count))
                     (if (not has-b-between-numbers)
                     (setq count (1+ count))
                           (if (not notb)
                             (setq count (1+ count))
                               (if (not (= b-count 1) )
                                (setq count (1+ count))
                               (format t "~a, VALUEF ~%" word))))))))))
        

(defun valid-word-p (word)
  (let* ((len (length word)) ; stores length
         (char1 (char word 0)) ; keeps the first character
         (valid nil)) ; suppose the word is valid

    (if (< len 1)
        (setq valid nil) ; Invalid if the word is shorter than 1 character
        (if (not (and (char>= char1 #\a) (char<= char1 #\z))); If the first character is not between a-z
            (setq valid nil)
            (loop for i from 1 to (1- len)
                until (not (or (and (char>= (char word i) #\a) (char<= (char word i) #\z))
                                 (and (char>= (char word i) #\0) (char<= (char word i) #\9))))
              
                do (if (= i (1- len))
                         (setq valid t)
                         ))))

        (if valid
        (format t "~a,DENTIFIER ~%"word)
        (setq count (1+ count)))))

(defun gppinterpreter(&optional input-file)
  (format t "Değer girin: ")
  (let ((input (read-line))
        (count 0)) ; Reset count on every gppinterpreter call
    (let ((words (split-input-into-words input)))
      (dolist (word words)
        (setq count 0) ; Reset count at the start of processing of each word
        (process-word word)

        (if (= count 1)
        (valid-word-p word))

        (if (= count 2)
        (process-operator word)) 

        (if (= count 3)
        (is-float-number word))
        )
            
        (if (not(or (= count 0) (= count 1)(= count 2)(= count 3)))
        (format t "LEXİCAL_LSYNTAX_ERROR~%" )))))

(gppinterpreter)

(defun gppinterpreter (&optional input-file)
 ; "Verilen dosyadaki her satırı okur ve parçalar."
  (with-open-file (stream file-path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do
          (format t "SATIR: ~a~%" line) ; You can print the read line on the screen.
          (let ((words (split-input-into-words line)))
          (dolist (word words)
          (setq count 0) ;Reset count at the start of processing of each word
          (process-word word)

        (if (= count 1)
        (valid-word-p word))

        (if (= count 2)
        (process-operator word)) 

        (if (= count 3)
        (is-float-number word))
        )
            
        (if (not(or (= count 0) (= count 1)(= count 2)(= count 3)))
        (format t "LEXİCAL_LSYNTAX_ERROR~%" )))
        ))) 

(setq file-path "test.txt")

(gppinterpreter)