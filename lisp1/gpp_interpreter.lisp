(defvar count 0)
(defvar token-type '()) ; Define tokens as a global variable
(defvar token-value '())
(defvar function-name-list '())
(defvar temp-token-value '())
(defvar temp-token-type '())
(defvar copy_temp_type '())
(defvar count_op 0
  "Global variable to store the count of OP_OP occurrences.")

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
    ((string-equal currentList "DEF") "KW_DEF") 
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
       
          (setq count (1+ count))
        (progn
        (push  word token-value)
        (push  result token-type)))))

(defun process-operator (word)
  (let ((result (checkOperators word)))
    (if (string= result "nothing")
          (setq count (1+ count))
          (progn
        (push word token-value)
        (push result token-type)))))

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
                            ;;  (push (list word "VALUEF") tokens)
                              (progn (push  word token-value)
                                (push  "VALUEF" token-type))
                                )))))))))
        

(defun valid-word-p (word)
  (let* ((len (length word)) ; stores length
         (char1 (char word 0)) ; keeps the first character
         (valid nil)) ; suppose the word is valid

    (if (< len 0)
        (setq valid nil) ; Invalid if the word is shorter than 1 character
        (if (not (and (char>= char1 #\a) (char<= char1 #\z))); If the first character is not between a-z
            (setq valid nil)
            (loop for i from 0 to (1- len)
                until (not (or (and (char>= (char word i) #\a) (char<= (char word i) #\z))
                                 (and (char>= (char word i) #\0) (char<= (char word i) #\9))))
              
                do (if (= i (1- len))
                         (setq valid t)
                         ))))

        (if valid
        (progn
        (push  word token-value)
        (push "IDENTIFIER" token-type))
        ;;(push (list word "IDENTIFIER ") tokens)
        (setq count (1+ count)))))



;;burada ben fonksiyonum tanımlı mı değilmi ona bakıyorum tanımlıysa true dondecerek değilse fil dondurecek
(defun check-in-list (element function-name-list)
  (loop for item in function-name-list
        until (equal element item)
        finally (return (equal element item))))




;;;op_op sonuncunun indexsini donderir.
(defun enumerate (lst)
  "Enumerate the list, returning a list of cons cells (index . element)."
  (loop for element in lst
        for index from 0
        collect (cons index element)))
;;;op_op sonuncunun indexsini donderir.
(defun find-max-open-parenthesis (token-type)
  "Finds the index of the largest number of '(' characters in the token-type-list."
  (let ((max-count 0)
        (current-count 0)
        (max-index nil))

    (loop for (index . token-type-list) in (enumerate token-type)
          do
          (if (equal token-type-list "OP_OP")
              (progn
                (setq current-count (1+ current-count))
                (if (> current-count max-count)
                    (progn
                      (setq max-count current-count)
                      (setq max-index index))))))
                      ;;(format t "Index temp: ~a~%" max-index )

   max-index))


(defun evaluate-expression (token-type token-value index)
(if (> (length token-type) 4)
(progn
  (let* ((operator (nth (1+ index) token-type))
         (operand1 (nth (+ 2 index) token-value))
         (operand2 (nth (+ 3 index) token-value))
         (result (evaluate-math-expression operator operand1 operand2)))

    (setf (nth index token-type) "VALUEF")
    (setf (nth index token-value) result)
    ;;(format t "aToken type: ~a~%" token-value)
    ;;(format t "bToken type: ~a~%" token-type)
    (setf indexx (+ index 1))
    (setf token-value (append (subseq token-value 0 indexx) (subseq token-value (+ indexx 4))))
    (setf token-type (append (subseq token-type 0 indexx) (subseq token-type (+ indexx 4))))
    ;;(format t "cToken type: ~a~%" token-value)
    ;;(format t "Token type: ~a~%" token-type)
    (setq temp_index (find-max-open-parenthesis token-type))
    ;;(format t "Index temp: ~a~%" temp_index )
     (evaluate-expression token-type token-value temp_index )
))
    (format t "Result: ~a~%" (first token-value)))  
)

(defun error_function (copy_token index)
  "Evaluate the expression based on CFG rule and perform the math operation."

              (let* ((operator_op (nth index copy_token))
                 (operator (nth (1+ index) copy_token))
                 (operand1 (nth (+ 2 index) copy_token))
                 (operand2 (nth (+ 3 index) copy_token))
                 (operator_cp (nth (+ 4 index) copy_token)))
            (if (and (string= operator_op "OP_OP")
                     (string= operator_cp "OP_CP"))
                (if (or (string= operator "OP_PLUS")
                        (string= operator "OP_MINUS")
                        (string= operator "OP_MULT")
                        (string= operator "OP_DIV"))
                    (if (and (string= operand1 "VALUEF")
                             (string= operand2 "VALUEF"))
                        (progn
                          (setf (nth index copy_token) "VALUEF")
                          (setf indexx (+ index 1))
                          (setf copy_token (append (subseq copy_token 0 indexx) (subseq copy_token (+ indexx 4))))
                          (setq index_temp (find-max-open-parenthesis copy_token))
                          (if (> (length copy_token) 5)
                          (error_function copy_token index_temp)
                          ;;(format t "All checks passed successfully.~%")
                          )
                          t)
              (error "Syntax error. Stop Program" )
              )
              (error "Syntax error Stop Program " )
              )
              (error "Syntax error Stop Program")
              )
)
)
;;matematiksel
(defun evaluate-math-expression (operator operand1 operand2)
  (cond
    ((string= operator "OP_PLUS")
     (perform-addition operand1 operand2))

    ((string= operator "OP_MINUS")
     (perform-subtraction operand1 operand2))

    ((string= operator "OP_MULT")
     (perform-multiplication operand1 operand2))

    ((string= operator "OP_DIV")
     (perform-division operand1 operand2))

    (t (error "Unknown operator: ~A" operator))))

(defun parse-fraction-string (operand)
  (if (position #\b operand)
      (let* ((b-index (position #\b operand))
             (term1 (parse-integer (subseq operand 0 b-index)))
             (term2 (parse-integer (subseq operand (1+ b-index)))))
        (list term1 term2)) ; Burada paydanın 1 olduğunu varsayıyorum
      (if (stringp operand)
          (list (parse-integer operand) 0 1)
          (list operand 0 1)))) ; Eğer operand zaten bir sayı i

(defun perform-addition (operand1 operand2)
  (let* ((values1 (parse-fraction-string operand1)) ; Operand 1'i parçala
         (values2 (parse-fraction-string operand2)) ; Operand 2'yi parçala
         (numerator1 (nth 0 values1))  ; Operand 1'in payı
         (denominator1 (nth 1 values1)) ; Operand 1'in paydası
         (numerator2 (nth 0 values2))  ; Operand 2'nin payı
         (denominator2 (nth 1 values2)) ; Operand 2'nin paydası
         (result-numerator (+ (* numerator1 denominator2) (* numerator2 denominator1))) ; Payın toplamı
         (result-denominator (* denominator1 denominator2))) ; Paydanın çarpımı
    (format nil "~Ab~A" result-numerator result-denominator denominator2)))

(defun perform-subtraction (operand1 operand2)
  (let* ((values1 (parse-fraction-string operand1)) ; Operand 1'i parçala
         (values2 (parse-fraction-string operand2)) ; Operand 2'yi parçala
         (numerator1 (nth 0 values1))  ; Operand 1'in payı
         (denominator1 (nth 1 values1)) ; Operand 1'in paydası
         (numerator2 (nth 0 values2))  ; Operand 2'nin payı
         (denominator2 (nth 1 values2)) ; Operand 2'nin paydası
         (result-numerator (- (* numerator1 denominator2) (* numerator2 denominator1))) ; Payın farkı
         (result-denominator (* denominator1 denominator2))) ; Paydanın çarpımı
    (format nil "~Ab~A" result-numerator result-denominator denominator2)))


(defun perform-multiplication (operand1 operand2)
  (let* ((values1 (parse-fraction-string operand1)) ; Operand 1'i parçala
         (values2 (parse-fraction-string operand2)) ; Operand 2'yi parçala
         (numerator1 (nth 0 values1))  ; Operand 1'in payı
         (denominator1 (nth 1 values1)) ; Operand 1'in paydası
         (numerator2 (nth 0 values2))  ; Operand 2'nin payı
         (denominator2 (nth 1 values2)) ; Operand 2'nin paydası
         (result-numerator (* numerator1 numerator2)) ; Payın çarpımı
         (result-denominator (* denominator1 denominator2))) ; Paydanın çarpımı
    (format nil "~Ab~A" result-numerator result-denominator)))

(defun perform-division (operand1 operand2)
  (let* ((values1 (parse-fraction-string operand1)) ; Operand 1'i parçala
         (values2 (parse-fraction-string operand2)) ; Operand 2'yi parçala
         (numerator1 (nth 0 values1))  ; Operand 1'in payı
         (denominator1 (nth 1 values1)) ; Operand 1'in paydası
         (numerator2 (nth 0 values2))  ; Operand 2'nin payı
         (denominator2 (nth 1 values2)) ; Operand 2'nin paydası
         (result-numerator (* numerator1 denominator2)) ; Payın çarpımı
         (result-denominator (* denominator1 numerator2))) ; Paydanın çarpımı
    (format nil "~Ab~A" result-numerator result-denominator)))





;; copy_value listemin typlerını bulmak için bir fonksiyon
(defun find_copy_temp_type (copy_value)
;(format t "Copy Temptype List: ~a~%" copy_value)
(loop for element in copy_value
 do (let ((result (checkOperators element)))
    (if (string= result "nothing")
        (push "VALUEF" copy_temp_type)
        (progn
        (push result  copy_temp_type))))
)
(setq copy_temp_type (reverse copy_temp_type))
;(format t "Copy Temptype List: ~a~%" copy_temp_type)
(setq index (find-max-open-parenthesis copy_temp_type))
(evaluate-expression copy_temp_type copy_value index))

;;token value ve token type kopyalıyorum  bide  funciton adı neyse onu function name list içine atıyorum. bu Bir function tanımlandığında geliyor
(defun def-function (token-value token-type)
  (setq temp-token-value (append temp-token-value (copy-list token-value)))
  (setq temp-token-type (append temp-token-type  (copy-list token-type)))
  (let ((function-name (nth 2 token-value)))
    ;(format t "Function Name List: ~a~%" function-name)
    ;(format t "Temp Token Value List: ~a~%" temp-token-value)
    ;(format t "Temp Token Type List: ~a~%" temp-token-type)
    ;; Fonksiyonun geri kalan işlemleri buraya eklenebilir
    ;; Oluşturulan function-name listesini function-name-list'e ekle
    (setq function-name-list (append function-name-list (list function-name))))
    (format t "#function~%")
)


(defun function-operation-function (token-type token-value)
;(format t "Copy Temp List: ~a~%" temp-token-value)
  ;;(format t "Token valuesss: ~a~%" temp-token-value)
  (if (= (length token-value) 5)
      (progn
        (setq num1 (nth 2 token-value)
              num2 (nth 3 token-value)
              tempx (nth 3 temp-token-value)
              tempy (nth 4 temp-token-value))
              (setq copy_value (copy-list temp-token-value ))
             ; (format t "Copy Temp List temp-token-value : ~a~%" temp-token-value)
              ;(format t "Copy Temp List: copy_value ~a~%" copy_value)
              (Setq copy_value (append (subseq copy_value 5 (1- (length copy_value)))));;burada işlem olan kısım kalıyor sadece 
              ;(format t "Copy Temp List copy_value: ~a~%" copy_value)
              ;(format t "Copy Temp List: ~a~%" num1)
              
             (loop for i below (length copy_value)
              do (when (equal (elt copy_value i) tempx)
             (setf (elt copy_value i) num1))
             (when (equal (elt copy_value i) tempy)
             (setf (elt copy_value i) num2))) 
              ;(format t "Copy Temp yrni List: ~a~%" copy_value)
              )
    (if (= (length token-value) 6)
        (progn
          (setq num1 (nth 2 token-value)
                num2 (nth 3 token-value)
                num3 (nth 4 token-value)
                tempx (nth 3 temp-token-value)
                tempy (nth 4 temp-token-value)
                tempa (nth 5 temp-token-value))
                (setq copy_value (copy-list temp-token-value ))
                ;(format t "ACopy Temp List: ~a~%" temp-token-value)
                ;(format t "Copy Temp List: ~a~%" copy_value)
                (Setq copy_value (append (subseq copy_value 6 (1- (length copy_value)))));;burada işlem olan kısım kalıyor sadece 
                ;(format t "Copy Temp List: ~a~%" copy_value)
                ;(format t "Copy Temp List: ~a~%" temp-token-value)
                
                
                (loop for i below (length copy_value)
                do (when (equal (elt copy_value i) tempx)
                (setf (elt copy_value i) num1))
                (when (equal (elt copy_value i) tempy)
                (setf (elt copy_value i) num2))
                (when (equal (elt copy_value i) tempa)
                (setf (elt copy_value i) num3))) 
               ; (format t "Copy Temp yrni List: ~a~%" copy_value)
                
             
                )
      (if (= (length token-value) 6)
          (progn
            (setq num1 (nth 2 token-value)
                  num2 (nth 3 token-value)
                  num3 (nth 4 token-value)
                  num4 (nth 5 token-value)
                  tempx (nth 3 temp-token-value)
                  tempy (nth 4 temp-token-value)
                  tempa (nth 5 temp-token-value)
                  tempb (nth 6 temp-token-value))
                  (setq copy_value (copy-list temp-token-value ))
                  ;(format t "Copy Temp List: ~a~%" temp-token-value)
                  ;(format t "Copy Temp List: ~a~%" copy_value)
                  (Setq copy_value (append (subseq copy_value 7 (1- (length copy_value)))));;burada işlem olan kısım kalıyor sadece 
                  ;;(format t "Copy Temp List: ~a~%" copy_value)
                  ;;(format t "Copy Temp List: ~a~%" temp-token-value)
                  

                  (loop for i below (length copy_value)
                  do (when (equal (elt copy_value i) tempx)
                  (setf (elt copy_value i) num1))
                  (when (equal (elt copy_value i) tempy)
                  (setf (elt copy_value i) num2))
                  (when (equal (elt copy_value i) tempa)
                  (setf (elt copy_value i) num3))
                  (when (equal (elt copy_value i) tempb)
                  (setf (elt copy_value i) num4))) 
                  ;;(format t "Copy Temp yrni List: ~a~%" copy_value) 
                  ))))
 ;(format t "Copy Temp List: ~a~%" copy_value)
  (find_copy_temp_type copy_value)
  )


(defun gpplexer (&optional input-file)
  "Verilen dosyadaki her satırı okur ve parçalar."
  (if input-file
      (with-open-file (stream file-path :direction :input)
        (loop
         :for line = (read-line stream nil nil)
         :while line
         :do
         (format t "SATIR: ~a~%" line)
         (setq token-type nil) ; token-type'ı sıfırla
         (setq token-value nil) ; token-value'yu sıfırla
         (setq f-temp nil) ; You can print the read line on the screen.
         (let ((words (split-input-into-words line)))
           (dolist (word words)
             (setq count 0) ;Reset count at the start of processing of each word
             (process-word word)
             (if (= count 1)
                 (valid-word-p word))
             (if (= count 2)
                 (process-operator word))
             (if (= count 3)
                 (is-float-number word)))
           (setq token-type (reverse token-type)) ; tokens listesini tersine çevir
           ;(format t "Token type: ~a~%" token-type) ; Use nreverse instead of reverse
           (setq token-value (reverse token-value))
           ;(format t "Token values: ~a~%" token-value)
           (setq index (find-max-open-parenthesis token-type))
           (setq f-temp (nth 1 token-value));girilen input un 2. elemanı functionın
           ;(format t "Token Value'nun 2. elemani: ~a~%" f-temp)
           ;; "KW_EXIT" durumunu kontrol et
           (if (string= (cadr token-type) "KW_EXIT")
               (return-from gpplexer 0))
           (if (string= (cadr token-type) "KW_DEF")
               (progn
                 (def-function token-value token-type))
             (if (check-in-list f-temp function-name-list)
                 (function-operation-function token-type token-value)
                 (progn
                   (setq copy_token (copy-list token-type))
                   (error_function copy_token index)
                   (evaluate-expression token-type token-value index))))))
         (format t "---- Bitiş ----~%"))
      (loop
       (setq token-type nil) ; token-type'ı sıfırla
       (setq token-value nil) ; token-value'yu sıfırla
       (format t "Değer girin: ")
       (let* ((input (read-line))
              (count 0)
              (f-temp nil)) ; f-temp'i tanımla
         (let ((words (split-input-into-words input)))
           (dolist (word words)
             (setq count 0)
             (process-word word)
             (if (= count 1)
                 (valid-word-p word))
             (if (= count 2)
                 (process-operator word))
             (if (= count 3)
                 (is-float-number word)))
           (setq token-type (reverse token-type)) ; tokens listesini tersine çevir
           ;(format t "Token type: ~a~%" token-type) ; Use nreverse instead of reverse
           (setq token-value (reverse token-value))
           ;(format t "Token values: ~a~%" token-value)
           (setq index (find-max-open-parenthesis token-type))
           (setq f-temp (nth 1 token-value));girilen input un 2. elemanı functionın
           ;(format t "Token Value'nun 2. elemani: ~a~%" f-temp)
           ;; "OP_OP KW_EXIT OP_CP" durumunu kontrol et
           ;(format t "funciton time list: ~a~%" function-name-list)
           (if (string= (format nil "~a" token-type) "(OP_OP KW_EXIT OP_CP)")
               (return-from gpplexer 0))
           (if (string= (cadr token-type) "KW_DEF")
               (progn
              (def-function token-value token-type))
           (if (check-in-list f-temp function-name-list)
               (function-operation-function token-type token-value)
               (progn
                 (setq copy_token (copy-list token-type))
                 (error_function copy_token index)
                 (evaluate-expression token-type token-value index))))))
       (format t "---- Bitiş ----~%"))))

;; Dosya adı verilirse
(setq file-path "test.txt")
(gpplexer file-path)

;; Dosya adı verilmezse (kullanıcıdan giriş alınır)
(gpplexer)
