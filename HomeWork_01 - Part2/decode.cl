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

;***********************************************************

(defun findMaxIndex (countList)
    (let ((max 0))
    (let ((index -1))

    (dotimes (i (list-length countList))
        (if(> (nth i countList) max)
            (progn
            (setf max (nth i countList))  
            (setf index i))
        )
    )

    (return-from findMaxIndex index)

    ));for lets
)



;; -----------------------------------------------------
;;          DECODE FUNCTIONS
;; -----------------------------------------------------

(defun Gen-Decoder-A (paragraph myList)

    (return-from Gen-Decoder-A (speacialFunction paragraph myList))

)

;; -----------------------------------------------------

(defun Gen-Decoder-B-0 (paragraph myList)
    
    (print "-----------  Gen-Decoder-B-0  -----------")

    (let ((countList (make-list 26 :initial-element 0)))

    (dotimes (i (list-length paragraph))
        (dotimes (j (list-length (nth i paragraph)))       
            (setf (nth (c2i (nth j (nth i paragraph))) countList) (+ (nth (c2i (nth j (nth i paragraph))) countList) 1))
        )
    )

    (let ((copyList (copy-list myList)))

    (setf (nth (findMaxIndex countList) copyList) 'e)
    (setf (nth (findMaxIndex countList) countList) -1)

    (setf (nth (findMaxIndex countList) copyList) 't)
    (setf (nth (findMaxIndex countList) countList) -1)

    (setf (nth (findMaxIndex countList) copyList) 'a)
    (setf (nth (findMaxIndex countList) countList) -1)

    (setf (nth (findMaxIndex countList) copyList) 'o)
    (setf (nth (findMaxIndex countList) countList) -1)

    (setf (nth (findMaxIndex countList) copyList) 'i)
    (setf (nth (findMaxIndex countList) countList) -1)

    (setf (nth (findMaxIndex countList) copyList) 'n)
    (setf (nth (findMaxIndex countList) countList) -1)


    (let (( myListe (copy-list (Gen-Decoder-A paragraph copyList)) ))

    (if (equal (nth 1 myListe) 78)
        (return-from Gen-Decoder-B-0 myListe)
        (return-from Gen-Decoder-B-0 (Gen-Decoder-A paragraph myList))
    )
    ) ;for let

    ));for lets

)
;; -----------------------------------------------------

(defun Gen-Decoder-B-1 (paragraph myList)
  
    (print "-----------  Gen-Decoder-B-1  -----------")

    ; 45 bin kelimenin oldugu sozlukte inceleme yaptim.
    ; 22 harfli bir tane kelime var. (E L E C T R O E N C E P H A L O G R A P H Y) - 12 farklı harf
    ; Eger sifreli dosyada 22 harfli kelime varsa sifrenin cozumu cok hızlı olacaktır. 

    (let ((flag -1))
    (let ((index -1))
    (dotimes (i (list-length paragraph))
        (if(equal (list-length (nth i paragraph)) 21)
            (progn
            (setf index i)
            (setf flag 1))
        )
    )

    (if(equal flag 1)
        (progn
        (let (( encodedWord (nth index paragraph)))

        (dotimes (i (list-length *dictionary*))
            (if(equal (list-length (nth i *dictionary*)) 21)
                (CheckWords encodedWord (nth i *dictionary*) myList)
            )
        )
        ) ;for let

        (return-from Gen-Decoder-B-1 (Gen-Decoder-A paragraph myList)) )
    )
    )) ;for lets

;----------

    ; Eger sifreli dosyada 22 harfli kelime yoksa.
    ; Yaptıgım arastirmaya gore ingilizcede en cok kullanılan kelime -> 'the'
    ; Sifreli dosyada karsima çikan 3 harfli kelimeleri teker teker 'the' olarak kabul ettim.


    (dotimes (i (list-length paragraph))
        (if(equal (list-length (nth i paragraph)) 3)
            (progn
                (let ((copyList (copy-list myList)))
                (let (( encodedWord (nth i paragraph)))

                (CheckWords encodedWord '(t h e) copyList) 

                (let (( myListe (copy-list (Gen-Decoder-A paragraph copyList)) ))

                (if (equal (nth 1 myListe) 78)
                    (return-from Gen-Decoder-B-1 myListe)
                )

                ))) ;for lets
            )
        )
    )

    (return-from Gen-Decoder-B-1 (Gen-Decoder-A paragraph myList))

)


;; -----------------------------------------------------
;;                  CODE-BREAKER
;; -----------------------------------------------------

(defun Code-Breaker (document decoder-function)
  
    (let ((myList (make-list 26)))

    (if(equal decoder-function "Gen-Decoder-A")
        (dotimes (i (list-length document))
            (let ((paragraf (nth i document)))
            (print paragraf)
            (print "-----------  Gen-Decoder-A  -----------")
            (let ((myListe (copy-list (Gen-Decoder-A (nth i document) myList))))

            (if (equal (nth 1 myListe) 78)
                (print (myDecoder paragraf (nth 0 myListe)))
                (print "I did not solve")
            )
            (write-line "")
            (write-line "")

            )) ;for lets
        )
    )

    (if(equal decoder-function "Gen-Decoder-B-0")
        (dotimes (i (list-length document))
            (let ((paragraf (nth i document)))
            (print paragraf)
            (let ((myListe (copy-list (Gen-Decoder-B-0 (nth i document) myList))))

            (if (equal (nth 1 myListe) 78)
                (print (myDecoder paragraf (nth 0 myListe)))
                (print "I did not solve")
            )
            (write-line "")
            (write-line "")

            )) ;for lets
        )
    )

    (if(equal decoder-function "Gen-Decoder-B-1")
        (dotimes (i (list-length document))
            (let ((paragraf (nth i document)))
            (print paragraf)
            (let ((myListe (copy-list (Gen-Decoder-B-1 (nth i document) myList))))

            (if (equal (nth 1 myListe) 78)
                (print (myDecoder paragraf (nth 0 myListe)))
                (print "I did not solve")
            )
            (write-line "")
            (write-line "")

            )) ;for lets
        )
    )

    ) ;for let
)

;; -----------------------------------------------------
;;                  MAIN
;; -----------------------------------------------------

(Code-Breaker *test-document* "Gen-Decoder-A")
(Code-Breaker *test-document* "Gen-Decoder-B-0")
(Code-Breaker *test-document* "Gen-Decoder-B-1")




