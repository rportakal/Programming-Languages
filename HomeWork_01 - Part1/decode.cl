; *********************************************
; *  341  Programming Languages               *
; *  Fall 2016                                *
; *  Author: Rıdvan Portakal , clisp          *
; *           121044052                       *
; *********************************************

;; ENVIRONMENT
;; "c2i", "i2c",and "apply-list"
(load "include.cl")

;; test document
(load "document.cl")

;; test-dictionary
;; this is needed for spell checking
(load "test-dictionary.cl")

;(load "dictionary.cl") ;;  real dictionary (45K words)

;; -----------------------------------------------------
;;                  HELPERS
;; -----------------------------------------------------

(defun getIndex (ch liste)

    (dotimes (i (list-length liste))
        (if(equal ch (nth i liste))
            (return-from getIndex i)
        )
    )
    (return-from getIndex -1)
)


(defun helperr (nameEncoded nameDict)

    (dotimes (i (list-length nameDict))
        (dotimes (j (- (list-length nameDict) 1))

            (if (equal (nth i nameEncoded) (nth j nameEncoded))
                (if (not (equal (nth i nameDict) (nth j nameDict)))
                    (return-from helperr -1)
                )
            )
        )
    )

    (dotimes (i (list-length nameDict))
        (dotimes (j (- (list-length nameDict) 1))

            (if (equal (nth i nameDict) (nth j nameDict))
                (if (not (equal (nth i nameEncoded) (nth j nameEncoded)))
                    (return-from helperr -1)
                )
            )
        )
    )
    (return-from helperr 0)
)


(defun CheckWords (nameEncoded nameDict myListe)

    (if (equal (helperr nameEncoded nameDict) -1)
        (return-from CheckWords -1)
    )

    (dotimes (j (list-length nameDict))
    
        (let ((indexWill (c2i (nth j nameEncoded))))
        (let ((indexDid (getIndex (nth j nameDict) myListe)))

        (if(not (equal indexDid -1))       
            (if(not (equal indexDid indexWill))
                (return-from CheckWords -1)
            )    
        )
        
        (if (not (equal (nth indexWill myListe) NIL))
            (if(not(equal (nth j nameDict) (nth indexWill myListe))) 
                (return-from CheckWords -1)
            )
        )

        (setf (nth indexWill myListe) (nth j nameDict))
        ));for lets
    )
)

;;-------------------------------------------------------------

(defun myDecoder (paraf myListe)

    (let ((wordList (make-list (list-length paraf))))

    (dotimes (i (list-length paraf))
        (let ((kelime (nth i paraf)))
        (let ((newWord (make-list (list-length kelime))))

        (dotimes (j (list-length kelime))
            (setf (nth j newWord) (nth (c2i (nth j kelime)) myListe))
        )

        (setf (nth i wordList) newWord)
        ));for lets
    )

    (return-from myDecoder wordList)
    );for let
)



(defun speacialFunction (paragraph myList)

    (if (equal (nth 0 paragraph) nil)
        (return-from speacialFunction (list myList 78)) 
    )

    (let ((flag 2))
    (let ((check 2))

    (let ((encodedWord (nth 0 paragraph)))
    (let ((charSize (list-length encodedWord)))                    

    (dotimes (i (list-length *dictionary*))

        (let ((copyList (copy-list myList)))
        (let ((backUpList (copy-list myList)))

        (let ((dictWord (nth i *dictionary*)))                
        (let ((charsizeDict (list-length dictWord)))          
    
        (if (= charsize charsizeDict)
            
            (if (not (equal (CheckWords encodedWord dictWord copyList) -1))
                (progn
                    (setf myList (copy-list copyList))
                    (setf flag 1)
                    (setf multipleList (copy-list (speacialFunction (cdr paragraph) myList)))
                    (setf myList (copy-list (nth 0 multipleList)))
                    (setf check (nth 1 multipleList))
                    (if(equal check 78)
                        (return-from speacialFunction (list myList 78)) 
                    )
                )
            )
        )

        (if (equal flag 1)
            (if(not(equal check 78))
                (progn
                (setf flag 2)
                (setf myList (copy-list backUpList)))
            )           
        )   
        ))));for lets
    )

    )))) ;for lets

    (return-from speacialFunction (list myList 2))    
)

;; -----------------------------------------------------
;;          DECODE FUNCTIONS
;; -----------------------------------------------------

(defun Gen-Decoder-A (paragraph)

    (print paragraph)

    (let ((myList (make-list 26)))

    (return-from Gen-Decoder-A (nth 0 (speacialFunction paragraph myList)))
    );for let

)

;; -----------------------------------------------------

(defun Gen-Decoder-B-0 (paragraph)
  ;you should implement this function
)
;; -----------------------------------------------------

(defun Gen-Decoder-B-1 (paragraph)
  ;you should implement this function
)


;; -----------------------------------------------------
;;                  CODE-BREAKER
;; -----------------------------------------------------

(defun Code-Breaker (document decoder-function)
  
    (if(equal decoder-function "Gen-Decoder-A")
        (dotimes (i (list-length document))
            (let ((paragraf (nth i document)))
            (print (myDecoder paragraf (Gen-Decoder-A (nth i document))))
            (print "--------")
            );for let
        )
    )
)

;; -----------------------------------------------------
;;                  MAIN
;; -----------------------------------------------------

(Code-Breaker *test-document* "Gen-Decoder-A")




