
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

 ;;(load "dictionary.ss") ;; the real thing with 45,000 words



(define paragraph  '(
                     (t h i s)(c o u r s e)(c o v e r s)(t o p i c s)(i n)(p r o g r a m m i n g)(l a n g u a g e s)(a n d)(c o m p i l e r s)

                      (a t t r i b u t e)(g r a m m a r s)(a n d)(t h e i r)(u s e)(i n)(s y n t a x)(d i r e c t e d)(t r a n s l a t i o n)
                     (m o d e l s)(o f)(p r o g r a m m i n g)(l a n g u a g e)(s e m a n t i c s)
                     )

  )

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;;iterates through the whole dictionary to check if the given word exists in the dicionary or not
(define spell_checker_helper (lambda (w d)

                               (cond((null? d) (> 0 0))
                                    ((equal? w (car d)) (= 1 1))
                                    (else (spell_checker_helper w (cdr d)))

                               )))

;;encode an each word in the paragraph n times and check that each word is found through the spell checker
(define check_paragraph (lambda (p n occur)
                          (cond((null? p) (list n occur))
                               ((spell-checker ((encode-n n) (car p))) (check_paragraph (cdr p) n (+ occur 1)))
                               (else (check_paragraph (cdr p) n occur))
                          )))

                          
;;helper method, calls check paragraph with the given n. increases n from 0 to 25, when the helper method returns true, it returns the decoded word back
;;touple (number of shitfts, number of occurences)
(define get_decoded_word (lambda (p w touple n)
                           (cond((> n 25) touple)
                                ((> (car (cdr touple)) (car (cdr (check_paragraph p n 0)))) (get_decoded_word p w touple (+ n 1)) )
                                (else (get_decoded_word p w (check_paragraph p n 0) (+ n 1)))
                           )))
;;gets the touple back with wich most of matches occures
(define get_decoder_A (lambda (paragraph word)

                       (get_decoded_word paragraph word '(0 0) 0)
                        ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DECODER B DOWN!!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;gets the max touple from the list
(define get_max_touple (lambda (l max)
                         (cond ((null? l) max)
                               ((equal? (car(car l)) 'a) (get_max_touple (cdr l) (list 'a (car(cdr (car l))))))
                               ((< (car (cdr max)) (car (cdr (car l)))) (get_max_touple (cdr l) (list (car (car l)) (car (cdr (car l))))))
                               (else (get_max_touple (cdr l) max)))
                               ))

;;given the letter update the list
;;input: a letter
;;output: updated list to that letter
(define update_list (lambda (letter l)
                      (cond ((null? letter) '())
                            ((equal? (car (car l)) letter) (cons (list (car (car l)) (+ (car (cdr (car l))) 1)) (cdr l)))
                            (else (cons (car l) (update_list letter (cdr l)))))
                      ))

;;input = A word
;;output: updated list to that word
(define get_n_freq (lambda (p l)
                     (cond ((null? p) l)
                           (else (get_n_freq (cdr p) (update_list (car p) l)))
                     )))


;;calls get-n-freq for each word in paragraph
;;input: paragraph
;;output touples
(define get_touple_list (lambda (p l)

                          (cond ((null? p) (get_max_touple l '(a 0)))
                                (else (get_touple_list (cdr p) (get_n_freq (car p) l))))
                          
                          ))

(define find_gap (lambda (l)

                   (cond ((equal? l 'e) 0)
                         (else (+ 1 (find_gap (next_letter l)))))

                   ))


(define touples '((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0)(k 0) (l 0) (m 0) (n 0) (o 0) (p 0) (q 0) (r 0) (s 0) (t 0) (u 0) (v 0) (w 0) (x 0) (y 0) (z 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DECODER B FINISHED!!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE BREAKER DOWN!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;



;;Takes an encoded paragraph and return the decoded paragraph
(define get_decoded_paragraph (lambda (paragraph decoder)
                                (cond((null? paragraph) '())
                                     (else (cons (decoder (car paragraph)) (get_decoded_paragraph (cdr paragraph) decoder)))
                                     )


                                ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE BREAKER FINISHED!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DECODER-N-D DOWN!!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define next_letter (lambda (letter)
                    (cond ((equal? letter 'a) 'b)
                          ((equal? letter 'b) 'c)
                          ((equal? letter 'c) 'd)
                          ((equal? letter 'd) 'e)
                          ((equal? letter 'e) 'f)
                          ((equal? letter 'f) 'g)
                          ((equal? letter 'g) 'h)
                          ((equal? letter 'h) 'i)
                          ((equal? letter 'i) 'j)
                          ((equal? letter 'j) 'k)
                          ((equal? letter 'k) 'l)
                          ((equal? letter 'l) 'm)
                          ((equal? letter 'm) 'n)
                          ((equal? letter 'n) 'o)
                          ((equal? letter 'o) 'p)
                          ((equal? letter 'p) 'q)
                          ((equal? letter 'q) 'r)
                          ((equal? letter 'r) 's)
                          ((equal? letter 's) 't)
                          ((equal? letter 't) 'u)
                          ((equal? letter 'u) 'v)
                          ((equal? letter 'v) 'w)
                          ((equal? letter 'w) 'x)
                          ((equal? letter 'x) 'y)
                          ((equal? letter 'y) 'z)
                          ((equal? letter 'z) 'a)

                          )))




(define get_nth_letter(lambda (letter n)

                       (cond ((null? letter) '())
                             ((= n 0) letter)
                             (else (get_nth_letter(next_letter letter) (- n 1)))
                         )

                       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DECODER-N-D FINISHED!!!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEBUGGERS METHODS DOWN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define get_word (lambda (word)

               (cond ((null? word) '())
                     (else (cons (next_letter (car word)) (get_word (cdr word))))


               )))




;; NOT NEEDED METHOD, ENCODES THE PARAGRAPH
;;w NOT NECCESARY. GOOD FOR DEBUGGING
(define encode_paragraphs (lambda (paragraphs)

                            (cond((null? paragraphs) '())
                                 (else (cons ((encode-n 5) (car paragraphs))  (encode_paragraphs(cdr paragraphs)))) )
 )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEBUGGERS METHODS FINISHED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION



;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
;;calls the helper method which returns either true of false
(define spell-checker 
  (lambda (w)

    (spell_checker_helper w dictionary)
   ))






;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
     (cond((null? d) '())
          (else (cons(get_decoded_paragraph (car d) encoder) (encode-d (cdr d) encoder)))
     
     )
    
    ))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
;;encodes the word to the nth value
(define encode-n
  (lambda (n)
    (lambda (w)

      (cond ((null? w) '())
            (else (cons (get_nth_letter (car w) n) ((encode-n n) (cdr w))))))

    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
;;gets the touple back
(define Gen-Decoder-A
  (lambda (p)
    (lambda (word)

     ((encode-n (car (get_decoder_A p word))) word)
    
    )))



;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
   (lambda (word)
     ( (encode-n (find_gap (car (get_touple_list p touples)))) word)
     )))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
;;a codebreaker
(define Code-Breaker
  (lambda (d decoder)
    
     (cond((null? d) '())
          (else (cons(get_decoded_paragraph (car d) decoder) (Code-Breaker (cdr d) decoder)))
     
     )))




;;;;;;;;;;---------------------------------------TEST CASES----------------------------------------------------------------

;---------------------------------TEST CASE 1 -----------------------------------------------------
(define e_d_5 '(((y m n x)
   (h t z w x j)
   (h t a j w x)
   (y t u n h x)
   (n s)
   (u w t l w f r r n s l)
   (q f s l z f l j x)
   (f s i)
   (h t r u n q j w x))
  ((f y y w n g z y j)
   (l w f r r f w x)
   (f s i)
   (y m j n w)
   (z x j)
   (n s)
   (x d s y f c)
   (i n w j h y j i)
   (y w f s x q f y n t s))
  ((r t i j q x)
   (t k)
   (u w t l w f r r n s l)
   (q f s l z f l j)
   (x j r f s y n h x))
  ((n s y j w r j i n f y j)
   (w j u w j x j s y f y n t s x)
   (t k)
   (u w t l w f r x)
   (u f w f q q j q)
   (u w t l w f r r n s l)
   (r t i j q x))))

(define e_p_5 '((h t z w x j) (h t a j w x) (y t u n h x) (n s)))

;-----------------------------TEST CASE 2 ------------------------------
(define d_e '( 
((h e l l o)(m y) (n a m e) (i s) (i m r a n))
((a n d) (t h i s) (i s) (a) (t e s t) (c a s e) (f o r))
((p a r t) (t w o))
((e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e))

))
(define e_d_e '(((r o v v y) (w i) (x k w o) (s c) (s w b k x))
  ((k x n) (d r s c) (s c) (k) (d o c d) (m k c o) (p y b))
  ((z k b d) (d g y))
  ((o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o))))

(define p_e '((e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e)))
(define p_10 '((o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o o)))

;FOR PART A
(define d_a_10 (Gen-Decoder-A '((k x n) (d r s c) (s c) (k) (d o c d) (m k c o) (p y b))))

;type: (Code-Breaker e_d_e d_a_10)
;expected_output : (((h e l l o)(m y) (n a m e) (i s) (i m r a n))
;((a n d) (t h i s) (i s) (a) (t e s t) (c a s e) (f o r))
;((p a r t) (t w o))
;((e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e))

;))


;FOR PART B

(define decoder_10 (Gen-Decoder-B p_10))
;;type: (Code-Breaker e_d_e decoder_10)
;;expected output: ( 
;((h e l l o)(m y) (n a m e) (i s) (i m r a n))
;((a n d) (t h i s) (i s) (a) (t e s t) (c a s e) (f o r))
;((p a r t) (t w o))
;((e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e e))

;))




;------------------------------TEST CASE 3-----------------------------------
(define p_d '((d d)))
(define d_d '(
              ((z) (a) (b))
              ((c d e f g h i j k l m n o p q r s t u v w x y z))
              ))



;For Part B
(define d_a_a (Gen-Decoder-A '((c d e f g h i j k l m n o p q r s t u v w x y z))))
;;type: (Code-Breaker d_d d_a_a)
;;;expected-output: (((y) (z) (a)) ((b c d e f g h i j k l m n o p q r s t u v w x y)))





;For Part A
(define decoder_alpha (Gen-Decoder-B p_d))

;;type: (Code-Breaker d_d decoder_alpha)
;expected-output: (((a) (b) (c)) ((d e f g h i j k l m n o p q r s t u v w x y z a)))


;-------------------------------TEST CAST 4- ----------------------------

;;PART B
(define d_e_7'(

	((e l e p h e n t) (e l e g e n t) (e n g l i s h) (e m n e s i a) (p e e d) (b e e p) (b l e a c h) (e g e r) (g e e p) (g e e k) (c r e e d))
))


(define e_d_e_7 '(((l s l w o l u a)
   (l s l n l u a)
   (l u n s p z o)
   (l t u l z p h)
   (w l l k)
   (i l l w)
   (i s l h j o)
   (l n l y)
   (n l l w)
   (n l l r)
   (j y l l k)))
)

(define p_e_7 '((l u n s p z o) (l n l y) (n l l w) (n l l r)))

(define decoder_7_b (Gen-Decoder-B p_e_7))
;;type: (Code-Breaker e_d_e_7 decoder_7_b)
;expected-output: '(((e l e p h e n t) (e l e g e n t) (e n g l i s h) (e m n e s i a) (p e e d) (b e e p) (b l e a c h) (e g e r) (g e e p) (g e e k) (c r e e d))




;;PART A ---------------------------
;;;;; ADD (e n g l i s h) in you test-dictionary first
(define decoder_7_a (Gen-Decoder-A p_e_7))
;;type (Code-Breaker e_d_e_7 decoder_7_a)
;;expected output: '(((e l e p h e n t) (e l e g e n t) (e n g l i s h) (e m n e s i a) (p e e d) (b e e p) (b l e a c h) (e g e r) (g e e p) (g e e k) (c r e e d))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 25))
;;(encode-d document add5)
;;"-------------------------------------------------"
;;(define decoder_a (Gen-Decoder-A paragraph))
;;(define decoder_b (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)
