
;======================================================================================================
;
;                         LIBRAIRIE DE MANIPULATIONS COMBINATOIRES
;         INSPIREES(?) DANS ET PAR LA TECHNIQUE COMPOSITIONNELLE DE BRIAN FERNEYHOUGH
;
;======================================================================================================
;
;       Cette librairie a ete concue pendant le passage du compositeur Brian Ferneyhough par la pedagogie
;       de l'IRCAM.
;       Pendant les mois de mars/avril 1993 a janvier-avril 1996.
;       by Mikhail Malt     IRCAM 1993-1996                                                             
;
;
(in-package :om)


;pour l'index de positions
(defvar index 0)
;pour des posiions

;; 11/06/09 replaced position by lposition for avoiding CL symbol redefinition
(defvar lposition 0)

;pour des listes auxilliaires

(defvar aux1 nil)
(defvar aux2 nil)

(defmethod old-list-modulo ((list list) (ncol integer))
  (when (> ncol 0) (list-part list ncol)))

;=====================manipulations frequentielles================================


(defmethod! ratio-freq ((fund  integer) (ratio number))
  :icon 129
  :initvals '(6000 0)
  :doc "Computes a list of frequencies with <ratio> relations."
  (let ((note (mc->f fund))  (aux))
    (push note aux)
    (dolist (n ratio)
      (push (setf note (* note n)) aux))
    (f->mc (reverse aux))))


;;; !! Formerly called RATIO / conflicted with CL ratio
(defmethod! ratio-list  ((init integer) (ratio number)  (n integer)) 
  :icon 129
  :initvals '(0 0 1)
  :doc "Takes an interval <init> in midicents and a <ratio>
to build a list of <n> elements with decreasing intervals."
  (let ((aux (first init)) (val))
    (dotimes (x n)
      (push aux val)
      (setf aux (* aux ratio)))
    (reverse val)))

;==============================piqué (still) from Laurent Poittier=======================
(defmethod! geomt ((init number ) (n integer)  (pas number)) 
  :icon 129
  :initvals '(0 1 0)
  :doc "Builds a geometric series of <n> elements 
awith initial value <init> a a factor <pas>."
  (if (= n 0) ()
      (cons init (geomt (* pas init) (1- n) pas))))
;=================================================================================

(defun deep-mapcar/1 (fun list? &rest args)
  (labels ((map-structure (str accum)
             (cond ((null str) (reverse accum))
                   ((not (consp str))
                    (if accum (reverse (cons (apply fun str args) accum)) (apply fun str args)))
                   (t (map-structure (cdr str) (cons (map-structure (car str) ()) accum))))))
    (map-structure list? nil)))

(defmethod! g-subs (( sequence list) (old number) (new number))
  :icon 129
  :initvals '((1 2) 1 0)
  :doc "Substitutes <old> by <new> anywhere.
<old> and <new> can be numbers or symbols.
<new> can also be a list."
  (deep-mapcar/1  #'(lambda (x) (if (equalp x old) new x)) sequence))



(defun subs-prof-list (liste1 liste2)
  (cond ((null liste1) nil)
        ((atom liste1) (nth (incf index) liste2))
        (t (cons ( subs-prof-list (first liste1) liste2) 
                 ( subs-prof-list(rest liste1) liste2)))))


(defmethod! gl-subs ((list1 list) (list2 list))
  :icon 129
  :initvals '((1 2) (1 2))
  :doc "Substitution of elements in liste2 (flat) in liste1 (with several levels)"
  (let ((index -1) )
    (subs-prof-list list1 list2)))

 ;; ==================================================================================== 
;;                                PAQUET   combine
;; ==================================================================================== 

;;          V1.0
;;                                 functions by Mikhail Malt   08/10/1993 Paris IRCAM

(defmethod! ser-op ((serie list) 
                    (oper integer)
                    &optional (mode 1 ) (in/pi 0))
  :icon 129
  :initvals '((6000 6300 6600 6400 6500 7100) 1 1 0)
  :menuins '( (1 (("-O-"  1) ("-R-" 2) ("-I-" 3) ("-RI-" 4)))
               (2 (("interv"  1) ("note" 2) )))
  :doc "The four basic serial music operations.

The optional inputs allow to obtain the four forms with different controls.

Note: in the mode <interv> transposition is in en midicents!"

  (case oper 
    (1 (if   (or (= 1 mode) (= 2 mode))  (case mode 
                                           (1 (om+ in/pi serie))
                                           (2 (dx->x in/pi (x->dx serie))))
             serie))
    (2 (if   (or (= 1 mode) (= 2 mode)) (case mode 
                                          (1 (om+ in/pi (reverse serie)))
                                          (2 (dx->x in/pi (x->dx (reverse serie)))))
             (reverse serie)))
    (3 (if   (or (= 1 mode) (= 2 mode)) (case mode 
                                          (1 (om+ in/pi (dx->x (first serie) (om* -1 (x->dx serie)))))
                                          (2 (dx->x in/pi (om* -1 (x->dx serie)))))
             (dx->x (first serie) (om* -1 (x->dx serie)))))
    (4 (if   (or (= 1 mode) (= 2 mode)) (case mode 
                                          (1 (om+ in/pi (dx->x (first serie) (reverse (om* -1 (x->dx serie))))))
                                          (2 (dx->x in/pi (reverse (om* -1 (x->dx serie))))))
             (reverse (dx->x (first serie) (om* -1 (x->dx serie))))))))



(defmethod! rot90 ((serie list)  &optional (mod 12)) 
  :icon 129
  :initvals '((6000) 12)
  :doc "Rotation of 90� from Walter O'Connell. 
Inversion between dates and pitches."
  (m->mc 
   (second 
    (mat-trans 
     (sort-list (mat-trans (list (mc->m serie mod) (arithm-ser 0 (1- mod) 1 ))) '<  'first)))))



(defmethod! serie-space ((serie list) (gen number) &optional (mod 12)) 
  :icon 129
  :initvals '((6000) 1 12)
  :doc "Transformation of a series by switching space.
<gen> is the generator of the new space.
<mod> is the modulo on which we operate (default=12)."
  (let ((aux nil) )
    (dotimes (n mod aux)
      (push (list n (mod (* n gen) mod)) aux))
    (setf aux (reverse aux))
    (mapcar  #'(lambda (x) (om+ (om* 1200 (- (first (octave-c3 x)) 3)) 
                                (m->mc (second (assoc (mc->m x mod) aux)) mod))) serie)))




(defmethod! retire ((liste list) (place integer) (n-elem integer))
  :icon 129
  :initvals '((1 2 3 4) 1 2)
  :doc "Removes the <n-elem> in <liste> from position <place>. 

Note: place=0 is the first element in the list."

  (subseq  liste place n-elem))



(defmethod! segment ((liste list) (place integer) (n-elem integer) (lecture integer))
  :icon 129
  :initvals '((1 2 3 4) 0 1 1)
  :menuins '( (3 (("lin" 1) ("circ" 2) )) )
  
  :doc "Removes the <n-elem> in <liste> from position <place>. 

Note: place=0 is the first element in the list.

<lecture> allows to choose between linear or circular reading of the list."

  (case lecture
    (1 (if (> (length liste) (+ place n-elem)) (subseq  liste place  (+ place n-elem))
           (nthcdr place (butlast liste 0))))
    (2 (let ((aux))
         (dotimes (n n-elem (reverse aux))
           (push (nth (mod (+ n place ) (length liste)) liste) aux))))))



(defmethod! l-segment ((liste list) (place list) (n-elem list)
                       (lecture integer))
  :icon 129
  :initvals '((1 2 ) (1 2 ) (1 2 ) 1)
  :menuins '( (3 (("lin" 1) ("circ" 2) )) )
  
  :doc "Removes the <n-elem> in <liste> from position <place>. 

Note: place=0 is the first element in the list.

<lecture> allows to choose between linear or circular reading of the list.

<place> and <n-elem> can be lists: the shortest list will stop the process!"

  (mapcar #'(lambda (x y) (segment liste x y lecture)) place n-elem))



(defmethod! analyse ((accord list))
  :icon 129
  :initvals '((1 2 3 4))
  :doc "Analyzes a chord or list and returns a list with all intervals in each chord."
  (sort 
   (remove nil 
           (flat (maplist  #'(lambda (x) (om- (rest x) (first x))) accord))) #'<))

(defmethod! l-analyse ((accord list))
  :icon 129
  :initvals '((1 2 3 4))
  :doc "Analyzes a chord or list and returns a list with all intervals in each chord."
  (cond
   ((listp (first accord)) (mapcar  #'(lambda (x) (analyse (sort x '<))) accord))
   (t (analyse (sort accord '<)))))   
   
;------------mod-----------------
(defmethod* om-mod ((self number) (num number))  
  :initvals '(0 12) :indoc '("first input" "second input")
  :doc "Mod of two of numbers or trees." :icon 209 
  (mod self num))

(defmethod* om-mod ((self number) (num list))  
  (mapcar #'(lambda (input)
              (om-mod self input)) num))

(defmethod* om-mod ((self list) (num number))   
  (mapcar #'(lambda (input)
              (om-mod  input num)) self))

(defmethod* om-mod ((self list) (num list))
  (mapcar #'(lambda (input1 input2)
              (om-mod input1 input2)) self num))

;------------ceiling-----------------
(defmethod* om-ceiling ((self number))  
  :initvals '(0) :indoc '("first input" )
  :doc "Mod of two of numbers or trees." :icon 209 
  (ceiling self ))

(defmethod* om-ceiling ((self list) )   
  (mapcar #'(lambda (input)
              (om-ceiling  input )) self))


(defmethod! mc->M ((liste t) &optional (mod 12)) 
  :icon 129
  :initvals '(6000 12)
  :doc "Conversion of a list of midicents into residual classes modulo <mod>.
The modulo by default is 12.
C3 is the reference pitchs."
  (om-mod (om/ liste (om/ 100 (/ mod 12)))  mod))

(defmethod! M->mc ((liste list) &optional (mod 12) (ref 6000)) 
  :icon 129
  :initvals '((1 2) 12 6000)
  :doc "Conversion of a list of residual classes modulo <mod> into midicents. 
The modulo by default is 12.
C3 is the reference pitchs."
  (om+ ref (om* liste (om/ 1200 mod))))

(defmethod! octave-c3 ((midic integer)) 
  :icon 129
  :initvals '( 6000)
  :doc "Returns the octave from c3=octave 3"
  (let ((midic (list! midic)))
    (mapcar #'(lambda (x) (om- (om// x 1200) 2) ) midic)))

;=====================================================================================================

;============================================================================
;
;                PERMUTATIONS
;
;==================FONCTIONS AUXILIAIRES================================

(defun construct-memo (aux2 liste-base memo-pos)
  "Contruction of the list of paired positions"
  (dotimes (n (length aux2) memo-pos)
    (if (eql (nth n aux2) (nth n liste-base)) 
      (setf (nth n memo-pos) (nth n aux2))))
  memo-pos)
    
(defun construct-aux2 (aux2 aux3 memo-pos)
  "Contruction of the permuted list"
  (let ( (indice 0))
    (dotimes (m (length aux2))
      (if (eql 0 (nth m memo-pos)) (prog ()
                                     (setf (nth m aux2) 
                                           (nth indice aux3))
                                     (setf indice (1+ indice)))
          (setf (nth m aux2) (nth m memo-pos) )))
    aux2))
 
(defun construct-aux3 (aux2 memo-pos)
  "Construction of the list to permute"
  (let ((aux3))
    (dotimes (n (length aux2))
      (if (= 0 (nth n memo-pos))
        (push (nth n aux2) aux3)))
    (reverse aux3)))
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

#|
(defmethod! permutations ((bag list)) list
            "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag)
    '(())
    ;; Otherwise, take an element, e, out of the bag.
    ;; Generate all permutations of the remaining elements,
    ;; And add e to the front of each of these.
    ;; Do this for all possible e to generate all permutations.
    (mapcan #'(lambda (e)
                (mapcar #'(lambda (p) (cons e p))
                        (permutations
                         (remove e bag :count 1 :test #'eq))))
            bag)))
|#

;-----------------------------------------------------------------------------------

#|
;;;already defined in OMs Kernel Analproj;Canons;Noll
(defmethod! cartesian ((l1? t) (l2? t) fun) 
  :icon 129
  :initvals '((1 2 3 4) (5 6 7 8) +)
  :doc 
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (funcall fun x y)) (list! l2?))) (list! l1?)))
|#

(defun cartesian-op (l1 l2 fun) 
  "Applies the function fun to elements 
of l1? and l2? considered as matrices. 
Like  g-oper ;fun may be a Lisp function 
(list, +, *, cons, etc.)  or a function object 
created by  the  make-num-fun ;box .
The result is a cartesian product of l1? by l2?.
"
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (funcall fun x y)) (list! l2))) (list! l1)))


(defun combx (vals n)
  (cond
   ((<=  n 0) vals)
   (t (flat-once 
       (cartesian-op vals (combx vals (1- n)) 'x-append)))))


(defmethod! combinations ((vals list) (n integer))
  :icon 129
  :initvals '((1 2) 2)
  :doc "Combination of <vals> <n> to <n>"
  (let ((n (1- n)))
    (combx vals n)))


;------------------------------------------------------------------------

(defmethod! messiaen ((list list) &optional (max 2))
  :icon 129
  :initvals '((1 2) 2)
  :doc "Cyclic permutation - Messiaen-style"
  (let ((ref list)
        (permut nil)
        (n 1)
        (aux (list list)))
    (loop while (not (or (equal ref permut) (>= n max)))
          do (setf permut (first aux))
          (setf permut (posn-match permut list))
          (push permut aux)
          (incf n))
    (reverse aux)))

;(messiaen '(6 2 8 5 3 4 1 7 0) 30)

;----------------------------------------------------------------------------
(defmethod! prolifer ((serie1 list) (serie2 list) &optional (mod 12)) 
  :icon 129
  :initvals '((1 2) (1 2) 12)
  :doc "Proliferating series from Baraqu�"
  (let* ((factmod (/ 1200 mod))
         (modser2 (om-mod (om/ serie2 factmod) mod)))
    (posn-match serie1 modser2)))

;------------------------------------------------------------------------------

(defmethod! saw ((list list) (pas integer)) 
  :icon 129
  :initvals '((1 2) 12)
  :doc "Alternating permutation"
  (flat (mat-trans (reverse (old-list-modulo list pas)))))

(defmethod! rand-saw ((list list) (pas integer)) 
  :icon 129
  :initvals '((1 2) 12)
  :doc "Alternating permutation"
  (flat (mat-trans (permut-random (old-list-modulo list pas)))))


(defmethod! circ-saw ((list list) (pas integer) &optional (del 1))
  :icon 129
  :initvals '((1 2) 12 1)
  :doc "Alternating oscillation"
  (flat (mat-trans (rotate  (old-list-modulo list pas) del))))


(defmethod! oscil-permut ((list list))
  :icon 129
  :initvals '((1 2 3 4 5))
  :doc "Oscillation between extremes"
  (let ((explode (list-explode list 2)))
    (flat (mat-trans (list (first explode) (reverse (second explode)))))))


(defmethod! oscil-permutn ((list list) (deep integer)) 
  :icon 129
  :initvals '((1 2 3 4 5) 1)
  :doc "Oscillation between extremes with depth control"
  (let ((aux list))
    (dotimes (n deep aux)
      (setf aux (oscil-permut aux)))))


(defmethod! rev-saw ((list list) (pas integer)) 
  :icon 129
  :initvals '((1 2 3 4 5) 2)
  :doc "Reversed saw permutation."
  (flat  (reverse (list-explode  list pas))))

;----------------------------------------------------------------------

(defmethod! kreus0 ((list list)) 
  :icon 129
  :initvals '((1 2 3 4 5))
  :doc "Kreuspiel permutation"
  (let* ((longlist (length list))
         (longlist/2 (om// longlist 2))
         (half1 (rotate 
                 (posn-match list 
                                 (arithm-ser 0 (1- longlist/2) 1 )) 1))
         (half2 (rotate 
                 (posn-match list 
                                 (arithm-ser  longlist/2 (1- longlist) 1 )) -1)))
    (x-append (butlast half1)
              (first  half2)
              (last-elem half1)
              (rest half2))))


(defmethod! kreus ((list list) (pas integer))
  :icon 129
  :initvals '((1 2 3 4 5) 1)
  :doc "Kreuspiel permutation"
  (let ((aux list))
    (dotimes (n pas aux)
      (setf aux (kreus0 aux)))))


(defmethod! kreus0-1 ((list list))
  "Kreuspiel permutation"
  (let* ((longlist (length list))
         (longlist/2 (om// longlist 2))
         (half1 (rotate 
                 (posn-match list 
                             (arithm-ser 0 (1- longlist/2) 1 )) -1))
         (half2 (rotate 
                 (posn-match list 
                             (arithm-ser longlist/2 (1- longlist) 1 )) 1)))
    (x-append (last-elem half2)
              (rest half1)
              (butlast half2)
              (first  half1)
              )))


(defmethod! kreus-1 ((list list) (pas integer)) 
  :icon 129
  :initvals '((1 2 3 4 5) 1)
  :doc "Kreuspiel permutation"
  (let ((aux list))
    (dotimes (n pas aux)
      (setf aux (kreus0-1 aux)))))


;--------------------------------------------------------------------------
(defun spiral-out-left (list)
  (let* ((long (length list))
         (index (om-ceiling (om/ long 2))))
    (posn-match list
                    (x-append (reverse (arithm-ser index (1- long) 1 ))
                                  (rotate (reverse (arithm-ser 0 (1- index) 1 )) -1)))))


(defun spiral-out-rigth (list)
  (let* ((long (length list))
         (index (om// long 2)))
    (posn-match list
                    (x-append (rotate (reverse (arithm-ser index (1- long) 1 )) 1)
                                  (reverse (arithm-ser 0 (1- index) 1 ))
                                  ))))


(defun spiral-in-left (list)
  (let* ((long (length list))
         (index (om-ceiling (om/ long 2))))
    (posn-match list
                (x-append (reverse (arithm-ser (1- long) index 1 ))
                          (rotate (reverse (arithm-ser 0 (1- index) 1 )) 1)))))

(defun spiral-in-rigth (list)
  (let* ((long (length list))
         (index (om// long 2)))
    (posn-match list
                (x-append (rotate (reverse (arithm-ser index (1- long) 1 )) -1)
                          (reverse (arithm-ser 0 (1- index) 1 ))
                          ))))

(defmethod! spiral ((list list) (mode integer) (deep integer))
  :icon 129
  :initvals '((1 2) 1 1)
  :menuins '( (1 (("out-l" 1) ("out-r" 2) ("in-l" 3) ("in-r" 4))))
  :doc "Spiral permutation"
  (let ((aux (list list)))
    (dotimes (n deep (reverse aux))
      (push (funcall (case mode
                       (1 'spiral-out-left)
                       (2 'spiral-out-rigth)
                       (3 'spiral-in-left)
                       (4  'spiral-in-rigth))  (first aux)) aux))))



;--------------------------------------------------------------------------

;----------------------------------------------------------------------------------

(defmethod! permut-dyn ((liste list ))
  :icon 129
  :initvals '((1 2 3 4 5))
  :doc "Computes the dynamic permutation of a list until it's in order."
  (let ((aux1  )
        (aux2 (copy-list liste))
        (aux3 nil)
        (memo-pos (create-list (length liste) 0)) 
        (liste-base (sort-list  liste)))
    (loop  
      (setf aux1 (append aux2 aux1))
      (if (equal  liste-base aux2) 
        (return (reverse (list-explode aux1 (/ (length aux1) (length liste))))) )
      (setf memo-pos (construct-memo aux2 liste-base memo-pos)) ; memo-pos memorise les positions pas appariees
      (setf aux3 (construct-aux3 aux2 memo-pos))                   ; aux3 contient les elements pas matches!
      (setf aux3  (permut-random aux3))                    ; permutation de aux3
      (setf aux2 (construct-aux2 aux2 aux3  memo-pos))         ; reconstruction de la liste a partir d'elem. permut et elem. fixes! 
      )))

(defmethod! permut-dyn1 ((lis1 list ) (lis2 list ))
  :icon 129
  :initvals '((1 2 3 4 5) (1 2))
  :doc "Computes a dynamic permutation between <liste> and <liste2>.
The lists must contain the same elements!"
  (let ((aux1  nil)
        (aux2 (copy-list lis1))
        (aux3 nil)
        (memo-pos (create-list (length lis1) 0)) 
        (liste-base (copy-list lis2)))
    (loop  
      (setf aux1 (append aux2 aux1))  
      (if (equal  liste-base aux2) 
        (return (reverse (list-explode aux1 (/ (length aux1) (length lis1))))) )
      (setf memo-pos (construct-memo aux2 liste-base memo-pos))
      (setf aux3 (construct-aux3 aux2 memo-pos))
      (setf aux3  (permut-random aux3))
      (setf aux2 (construct-aux2 aux2 aux3  memo-pos))  
      )))

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

;Cheack if an element is smaller or equal to 1
; recursive version
(defun verifier1 (liste)
  (cond ((null liste) nil)
        ((atom liste) (if (< liste 1) 1 liste))
        (t (cons (verifier1 (car liste))
                 (verifier1 (cdr liste))))))


(defun verifierx (liste x)
  (cond ((null liste) nil)
        ((atom liste) (if (<= liste x) x liste))
        (t (cons (verifierx (car liste) x)
                 (verifierx (cdr liste) x)))))


(defmethod! single-to-1 ((liste list)) 
  :icon 129
  :initvals '((1 2 3 4 5))
  :doc "prend une liste simple et la réduit à une liste de 1"
  (let ((aux )  (len (length liste)))
    (dotimes (m len aux)
      (push  (verifier1 (om- liste m)) aux))
    (reverse aux)))

(defmethod! single-to-x ((liste list)  (x integer)) 
  :icon 129
  :initvals '((1 2 3 4 5) 1)
  :doc "prend une liste simple et la réduit à une liste de  x"
  (do ((aux ) 
       (aux1)
       (indice 0 (1+ indice))
       (liste-de-x (create-list (length liste) x)))
      ((equal aux1 liste-de-x )  (reverse aux))
    (setf aux1 (verifierx (om- liste indice) x))
    (push   aux1 aux)))

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

(defmethod! permut->to-1 ((liste list))
  :icon 129
  :initvals '(((1 2) (2 1)))
  :doc "Takess a list of permutations (from permut-dyn for instance) and brings it back to 1"
  (let ((aux )
        (aux1)
        (liste-de-1 (create-list (length (first liste)) 1)))
    (dotimes (n (length liste) aux)
      (push  (setf aux1 (verifier1 (om- (nth n liste) n) )) aux)
      (if (equal aux1 liste-de-1 ) (return aux)))
    (if (equal aux1 liste-de-1 )
      (reverse aux)
      (do ((indice 1 (+ indice 1)))
          ((equal aux1 liste-de-1 )  (reverse aux))
        (push (setf aux1 (verifier1 (permut-random (om- aux1 indice)) )) aux)))))

;Note: decrements are done independently, even if permutation is repeated!
(defmethod! permut->to-x ((liste list ) (x integer)) 
  :icon 129
  :initvals '(((1 2) (2 1)) 1)
  :doc "prend une liste de permutations ( de permut-dyn par exemple), ou n'importe  et la ramène à x"
  (let ((aux )
        (aux1)
        (liste-de-1 (create-list (length (first liste)) x)))
    (dotimes (n (length liste) aux)
      (push  (setf aux1 (verifierx (om- (nth n liste) n) x)) aux)
      (if (equal aux1 liste-de-1 ) (return aux)))
    (if (equal aux1 liste-de-1 )
      (reverse aux)
      (do ((indice 1 (+ indice 1)))
          ((equal aux1 liste-de-1 )  (reverse aux))
        (push (setf aux1 (verifierx (permut-random (om- aux1 indice)) x)) aux)))))
        
;========================================================================





;=====================teste de registre==================================== 

#|
(defmethod* teste1 (( variable t) (binf t) (bsup t) (mod t)) t
  "test la <variable>est entre binf et bsup, et la raméne à l'interieure par +/- mod " 
  (let ((variable (/ variable 100)) )
    (* 100 (if (> variable bsup) (- bsup (- mod  (pw::ll/mod (- variable bsup)  mod)))
               (if (< variable binf) (+ binf (- mod  (pw::ll/mod (- binf variable )  mod)))  variable) )
       )))


(defmethod* teste1a (( variable t) (binf t) (bsup t) (mod integer))
  "test la <variable>est entre binf et bsup, et la raméne à l'interieure par +/- mod, variable est une liste, 
et les bornes sont fixes. <Variable> est en midicents et <binf> et <bsup> sont en midi " 
  
  (mapcar #'(lambda (l) (teste1 l binf bsup mod))  variable)
  )
|#
;============================================================================
;=============================OPERATIONS SUR LES PULSES DES RTM!!!!!=================== 


(defmethod! puls/mes ((mesures list) (pulses list))
  :icon 129
  :initvals '((2 2) (3 3))
  :doc "Build a rhythmic structure with a sequence of <mesures> (subdivisions) and <pulses> pulsesper measure."
  (mapcar #'(lambda (m p) (list m  p))  
          mesures (mapcar #'(lambda (x) (create-list x 1)) pulses)))


(defun get-nth-measure (n meslist loop-p)
   (let ((len (length meslist)))
     (cond
      ((null loop-p) 
       (if (< n (1- len))
           (nth n meslist) 
         (car (last meslist))))
      (t (nth (mod n len)  meslist)))))

(defmethod! make-measures ((mesures t) &optional n loop)
  :icon 129
  :initvals '(((4 4)) nil nil)
  :doc "Build a list of measures from signatures.
If <n> is given create n masures.
<loop> flag make circular measures else the last is repeat."
  (let* ((mesures (if (listp (car mesures)) mesures (list mesures))))
    (when (and n (> n (length mesures)))
      (setf mesures
            (loop for i from 0 to (- n 1)
                  collect (get-nth-measure i mesures loop))))
    (loop for mes in mesures collect
          (list mes (create-list  (car mes) 1)))))

#|


;Substitutions de pulses
;<sub>, <sub> est une liste au format de rtm!!!!!!

(defun subs-pulse (liste  sub)
  "ce module substitue toutes les pulsations par la liste de subdivisions
<sub>, <sub> est une liste au format de rtm!!!!!!
ATTENTION:<liste> est une mesure!!!"
  (cond ((null liste) nil)
        ((atom liste ) sub)
        (t  (first liste)
            (dotimes (n (length (second liste)) liste)
              (if (atom (nth n (second liste))) 
                (setf (nth n (second liste)) (list (nth n (second liste)) sub ))    ;(setf (nth n (second liste)) sub)
                (subs-pulse (nth n (second liste)) sub)) )) ))



(defmethod! subst-pulses ((objet  list (:value '() :type-list (list  pw::measure-line)))
                          (sub list)) list
            "ce module substitue toutes les pulsations par la liste de subdivisions
<sub>, <sub> est une liste au format de rtm!!!!!!
ATTENTION:<objet> est la sortie d'une RTM avec une structure quelconque!!!"
  (let ((mesures (cond 
                  ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                  ((listp objet) objet)
                  (t (error "l'entrée <objet> n'a pas le bon type!!!")))))
    (mapcar #'(lambda (x) (subs-pulse x sub)) mesures)))

;++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++
;;;;version pour une liste de <sub>

(defun g-subs-pulse (liste)
  "ce module substitue toutes les pulsations par la liste de subdivisions
!!!!!!
ATTENTION:<liste> est une mesure!!!"
  (dotimes (n (length (second liste)) liste)
              (if (atom (nth n (second liste)))
                (progn ()
                       (setf (nth n (second liste)) 
                             (list (nth n (second liste)) (nth (mod index (length aux1)) aux1))) ;(setf (nth n (second liste)) (nth (mod index (length aux1)) aux1))
                       (incf index))
                (g-subs-pulse (nth n (second liste)))) )) 


(defun double-list (chose)
  (cond 
   ((null chose) '(()))
   ((atom chose) (list (list chose)))
   ((listp chose) (if (listp (car chose)) chose (list chose)))
   (t (list! chose))))

;(double-list 4)
;(double-list '(4))
;(double-list '((4 5)))



(defmethod! l-subst-pulses ((objet  list (:value '() :type-list (list  pw::measure-line)))
                          (sub list (:value '((1 2)))) )list
"ce module substitue toutes les pulsations par la liste de subdivisions
<sub>, <sub> est une liste simple avec des proportions ou 
une liste de listes avec des subdivisions (ou des proportions)!!!!!!
ATTENTION:<objet> est la sortie d'une RTM avec une structure quelconque!!!"
  (let ((mesures (cond 
                  ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                  ((listp objet) objet)
                  (t (error "l'entrée <objet> n'a pas le bon type!!!"))))
      (index 0) (aux1 (double-list sub)) )
  (mapcar #'(lambda (x) (g-subs-pulse x )) mesures)))


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;..........................................................................
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun subs-propo (liste1 liste2   place)
  "liste1, est la liste de la mesure
   liste2, est une liste pour les subdivisions. l'élém. 'n' indique qu'on va subdiviser
           la n'éme pulsation en 'n' parties
   sub est un pointeur qui indique quelle est la prochaine subdivision
   index, doit être toujours zéro
   place, est la liste des positions des pulses qui seront subdivisés"
  
  (dotimes (n (length (second liste1)) liste1 )
    (cond 
     ((and (atom (nth n (second liste1))) (plusp  (nth n (second liste1))))
      (progn ()
             (incf index)
             (if (appartient? index place)
               (progn ()
                      (setf (nth n (second liste1)) 
                            (list (nth n (second liste1)) 
                                  (nth lposition liste2)))
                      (incf lposition)))))
     ((listp (nth n (second liste1)))
      (subs-propo (nth n (second liste1)) liste2  place))
     (t ))) 
  liste1)
;==============================================================

;=====================================================================================================
(defmethod! subst-puls ((objet  list (:value '() :type-list (list  pw::measure-line))) 
                            (proportions  list) 
                            (lecture menu (:menu-box-list (("lin" . 1) ("circ". 2))
                                                          :type-list (no-connection)))
                            (places list)) list
            "substitution des pulses d'une liste de <mesures> par une de <proportions) et de <places>.
   L'argument <lecture> determine le mode de lecture de la liste de mesures soit linéairement,
   soit circulairement!!!!!
   <objet> est la sortie d'une boite <rtm>!!!!!
Cette fonction saute les pauses!!
<proportions> est une liste de listes"
  (let* ((mesures (cond 
                   ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                   ((listp objet) objet)
                   (t (error "l'entrée <objet> n'a pas le bon type!!!"))))
        (long-mesures (length mesures))
        (index 0) 
        (lposition 0)
        (n-mesure 0) 
        (aux-mesure nil)
        (aux-liste nil)
        (aux1 nil))
    (loop
      (setf aux1 
            (subs-propo  (nth (case lecture
                                  (1  n-mesure)
                                  (2  (mod n-mesure long-mesures))) mesures) 
                           proportions
                           places))
      (setf aux-mesure aux1) ;la nouvelle mesure
      (push aux-mesure aux-liste)
      (if (case lecture
            (1  (>= n-mesure (- long-mesures 1)))
            (2  (>= index (om-max places)))) 
        (return (reverse aux-liste))
        (incf n-mesure))
      )))

;=========================================================================== 
;partial reverse         ::::::::::::::::::::::::::::::::::::::::
;==============================================================

(defun my-listp (valeur  )
  (if (listp valeur) (list (first valeur ) (reverse ( second valeur )) ) valeur))

;(my-listp  '(7 (1 -1 1 -2)) )

(defun retro-mesure (mesure)
"construit le rétrograde d'une mesure"

(list (first mesure) (mapcar #'my-listp (reverse (second mesure)))))

;==============================================================
;total reverse         ::::::::::::::::::::::::::::::::::::::::
;==============================================================


(defun reverse-pulse-mes (liste)
  "liste= mesure
reverse d'une mesure-spéculaire"
  (cond ((null liste) nil)
        ((atom liste ) liste)
        (t (let ((aux))
             (list (first liste)             
                   (dolist (n  (second liste)  aux)       
                     (push (if (atom  n) n           
                               ( reverse-pulse-mes n))  
                           aux )))))))



(defmethod! reverse-mes ((objet  list (:value '() :type-list (list  pw::measure-line)))
                         (measu menu (:menu-box-list (("retro" . 1) ("norm". 2))
                                                     :type-list (no-connection)))
                         (pulse menu (:menu-box-list (("total" . 1) ("local". 2) ("norma". 3))
                                                     :type-list (no-connection)))) numbers?
            "inversion des pulses dans les mesure d'une RTM
<objet> est la sortie d'une RTM avec une structure quelconque
<measu> est le mode d'action sur les mesures
        -retro- les mesures sont retrogradées
        -norm- les mesures ne sont pas retrogradées
<pulse> est le mode d'action (retrogradation) sur les pulses de chaque mesure
        -total- les pulses d'une mesure sont totalement retrogradés
        -local- seulement le premier niveau des pulses est retrogradé
        -norma- aucune action sur les pulses"
  (let ((mesures (cond 
                  ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                  ((listp objet) objet)
                  (t (error "l'entrée <objet> n'a pas le bon type!!!")))))
    (setf mesures 
          (case pulse
            (1 (mapcar #'(lambda (x) (reverse-pulse-mes x))  mesures))
            (2 (mapcar #'(lambda (x) (retro-mesure x))  mesures))
            (3  mesures)))
    (case measu
      (1 (reverse mesures))
      (2 mesures))
    ))
;=======================================================================
;::::::::::::::::PERMUTATIONS:::::::::::::::::::::::::::::::::
;=======================================================================


(defmethod! permut-mes ((objet  list (:value '() :type-list (list  pw::measure-line)))
                         (circ numbers? (:value 1))) list
            "permutation circulaire du premier niveaux de pulses d'une mesure.
<objet> est la sortie d'un module `RTM
<circ> est un argument optionnel indiquant de combien de pas est la permutation circulaire
       il peut être soit un nombre soit une liste (une permutation différente pour chaque mesure"
  (let* ((mesures (cond 
                   ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                   ((listp objet) objet)
                   (t (error "l'entrée <objet> n'a pas le bon type!!!"))))
         (circ (create-list (length mesures) 1 (list! circ))))
    (mapcar #'(lambda (x c) (list (first x) (rotate (second x) c))) mesures circ)))


(defmethod! permut-struct ((objet  list (:value '() :type-list (list  pw::measure-line)))
                            (circ numbers? (:value 1))) list
            "permutation circulaire des mesures d'une séquence
<objet> est la sortie d'un module `RTM
<circ> est un argument optionnel indiquant de combien de pas est la permutation circulaire"
  (let* ((mesures (cond 
                   ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                   ((listp objet) objet)
                   (t (error "l'entrée <objet> n'a pas le bon type!!!")))))
    (rotate mesures circ)))

;=======================================================================
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;========================================================================

(defmethod! subs-pulse1 ((liste list) ) list
            "ce module subdivise toutes les pulsations d'une mesure par <sub>"
  (cond ((null liste) nil)
        (t  (first liste)
            (dotimes (n (length (second liste)) liste)              
              (cond 
               ((and (atom (nth n (second liste))) (plusp (nth n (second liste))))
                (setf (nth n (second liste)) 
                      (if (< (nth (mod index (length aux1)) aux1) 0)
                        (list (abs (nth n (second liste))) (list -1))
                        (list (nth n (second liste)) (make-list (abs (nth (mod index (length aux1)) aux1)) :initial-element 1))))
                (incf index))
               ((listp (nth n (second liste))) (subs-pulse1 (nth n (second liste)) ))
               (t )) )) ))



;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defmethod! div-pul-lis ((objet  list (:value '() :type-list (list  pw::measure-line)))
                         (sub numbers?))list
            "ce module subdivise toutes les pulsations d'une mesure par <sub>,
l'entrée mesures doit être la sortie d'une rtm!!!
<sub peut être une subdivision ou une liste de subdivisions"
  (let ((mesures (cond 
                  ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                  ((listp objet) objet)
                  (t (error "l'entrée <objet> n'a pas le bon type!!!"))))
        (index 0) (aux1 (list! sub)))
    (mapcar #'(lambda (l) (subs-pulse1 l) )  mesures)))



;==============================================================================
(defmethod! mesures1 ((liste list)) list
"calcul de mesures dans une liste, (1 3 6 8) -> ((1 3) (3 6) (6 8))"
(let ((aux))
(dotimes (n (length liste))
  (push (list (nth n liste) 
              (if (= n (- (length liste) 1) )  (nth 0 liste) (nth (1+ n) liste))) aux))
(reverse aux)))


;==============================================================================
(defmethod! make-mesure-c ((liste list) (princip list) (secondaires list) (pat list))list
  "liste= mesure la structure de la mesure devra avoir la syntaxe suivante:
(mesure (proportion1 proportion2 (proportion3 (sous-prop31 sous-prop32 (sous-prop31 (pattern))))))
Les indices comme <mesure>, <proportion3> et <sous-prop31> qu'indiquent en combien de pulsations
se subdivisera une liste de proportions, ou un pattern, seront pris de la librairie PRINCIP.
Les proportions secondaires de la librairie SECONDAIRES.
Pour indiquer qu'on veut un pattern il faut indiquer
-la proportion du pattern (librairie PRINCIP)
-et le pattern comme une liste d'un élément ->> (2 (0))
 (2 (0))  indique le pattern d'indice zéro dans l'espace de deux pulsations!!!"
  (cond ((null liste) nil)
        ((atom liste ) (nth (mod liste (length princip)) princip))
        (t (let ((aux))
             (list   (nth (mod (first liste) (length princip)) princip)           ;mesure  ou proportion principale d'une liste de subdivisions
                     (dolist (n  (second liste)  (reverse aux))        ;subdivisions
                       (push 
                        (cond ((atom  n) (nth (mod n (length secondaires)) secondaires))       ;verifie si il existe encore des subdivisions!!
                              ((and (atom  (first n)) (= 1 (length (second n)))) 
                               (list (nth (mod (first n) (length princip)) princip)
                                     (nth (mod (first (second n)) (length pat)) pat)))
                              (t (make-mesure-c n princip secondaires pat)))
                             aux )))))))

; ici la lecture des indices est faite de façon circulaire!!!!!!!

;================================================================================

(defmethod! l-make-mesure-c ((mesures list) (princip list) (secondaires list) (pat list))list
  "liste= mesure la structure de la mesure devra avoir la syntaxe suivante:
(mesure (proportion1 proportion2 (proportion3 (sous-prop31 sous-prop32 (sous-prop31 (pattern))))))
Les indices comme <mesure>, <proportion3> et <sous-prop31> qu'indiquent en combien de pulsations
se subdivisera une liste de proportions, ou un pattern, seront pris de la librairie PRINCIP.
Les proportions secondaires de la librairie SECONDAIRES.
Pour indiquer qu'on veut un pattern il faut indiquer
-la proportion du pattern (librairie PRINCIP)
-et le pattern comme une liste d'un élément ->> (2 (0))
 (2 (0))  indique le pattern d'indice zéro dans l'espace de deux pulsations!!!"
(mapcar #'(lambda (m) (make-mesure-c m princip secondaires pat)) mesures))


;//////////////////////////////////////////////////////////////////////////////////////////
;==========================================================================================
;///////////////////////////////////////////////////////////////////////////////////////////
(defun appartient? (element liste)
"cette fonction retourne 't si <element> appartient à <liste>
où nil au cas contraire"

 (not (null (member element liste)))) 


;===========================================================================================
;::::::::::::::::::::::fonction auxiliaire de subd-puls-mes3:::::::::::::::::::::::::::::::


(defun subs-pulsex3 (liste1 liste2   place)
  "liste1, est la liste de la mesure
   liste2, est une liste pour les subdivisions. l'élém. 'n' indique qu'on va subdiviser
           la n'éme pulsation en 'n' parties
   sub est un pointeur qui indique quelle est la prochaine subdivision
   index, doit être toujours zéro
   place, est la liste des positions des pulses qui seront subdivisés"
  
  (dotimes (n (length (second liste1)) liste1 )
    (cond 
     ((and (atom (nth n (second liste1))) (plusp  (nth n (second liste1))))
      (progn ()
             (incf index)
             (if (appartient? index place)
               (progn ()
                      (if (<=  (nth lposition liste2) 0)  
                        (setf (nth n (second liste1)) (om* (nth n (second liste1)) (- 1)))
                        (setf (nth n (second liste1)) 
                              (list (nth n (second liste1)) 
                                    (make-list (nth lposition liste2) :initial-element 1))))
                      (incf lposition)))))
     ((listp (nth n (second liste1)))
      (subs-pulsex3 (nth n (second liste1)) liste2  place))
     (t ))
    ) 
  liste1)
;==============================================================

;=====================================================================================================
(defmethod! subd-puls-mes3 ((objet  list (:value '() :type-list (list  pw::measure-line))) 
                            (subdivisions  list) 
                            (lecture menu (:menu-box-list (("lin" . 1) ("circ". 2))
                                                          :type-list (no-connection)))
                            (places list)) list
            "subdivision des pulses d'une liste de <mesures> par une de <subdivisions> et de <places>.
   L'argument <lecture> determine le mode de lecture de la liste de mesures soit linéairement,
   soit circulairement!!!!!
   <objet> est la sortie d'une boite <rtm>!!!!!
Cette fonction saute les pauses!!"
  (let* ((mesures (cond 
                   ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                   ((listp objet) objet)
                   (t (error "l'entrée <objet> n'a pas le bon type!!!"))))
         (long-mesures (length mesures))
         (index 0) 
         (lposition 0)
         (n-mesure 0) 
         (aux-mesure nil)
         (aux-liste nil)
         (aux1 nil))
    (loop
      (setf aux1 
            (subs-pulsex3  (nth (case lecture
                                  (1  n-mesure)
                                  (2  (mod n-mesure long-mesures))) mesures) 
                           subdivisions
                           places))
      (setf aux-mesure aux1) ;la nouvelle mesure
      (push aux-mesure aux-liste)
      (if (case lecture
            (1  (>= n-mesure (- long-mesures 1)))
            (2  (>= index (om-max places)))) 
        (return (reverse aux-liste))
        (incf n-mesure))
      )))
;..............................................................................
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun count-pulse (liste )
  "compte le nombre de pulses dans une mesure"
  (cond ((null liste) nil)
        (t  (dolist (n (second liste) index)
              (cond  
               ((and (atom n) (plusp n)) (incf index))
               ((listp n) (count-pulse n ))
               (t nil)) )) ))




(defmethod! npulses ((objet  list (:value '() :type-list (list  pw::measure-line)))) numbers?
            "retourne le nombre de pulses (pas les pauses) d'une RTM"
  (let ((mesures (cond 
                  ((typep objet 'pw::c-measure-line) (pw::rtm-dim objet 1))
                  ((listp objet) objet)
                  (t (error "l'entrée <objet> n'a pas le bon type!!!")))))
    (apply '+ (mapcar #'(lambda (x) (let ((index 0))
                                      (count-pulse x)))
                      mesures))))

;==============================================================
;============MUZAK- FUNCTIONS==================================
;==============================================================
;::::::::::::::::::::::::fonction auxiliaire:::::::::::::::::::
(defmethod! retirex ((liste list) (place fix>0) (n-elem fix>0))list
"retirex les <n-elem> éléments de la liste <liste> à partir de la place
<place>. OBS: place=1 c'est-à-dire premier élément de liste"
  (let ((aux))
    (dotimes (n n-elem (reverse aux))
      (push (nth (mod (+ n place (- 1)) (length liste)) liste) aux))))


#|
(defmethod! retire ((liste list) (place fix>0) (n-elem fix>0)) list
"retire les <n-elem> éléments de la liste <liste> à partir de la place
<place>. OBS: place=0 c'est-à-dire premier élément de liste"
(nthcdr place (butlast liste (- (length liste) (+ place n-elem)))))
|#

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defmethod! muzak2 ((liste list) (transp list) 
                    (n-pitch list) (mod fix>0 (:value  12))) list
"Cyclically read through it <liste> , transposing (according to a <transp> list) 
to a new level after a certain number of pithes are read (<n-pitch> list)
OBS:!:!:! Cette fonction permet la manipulation de séries numeriques modulo <mod>"
  (let ((aux) (aux1 liste) (pointeur (butlast (dx->x 1 n-pitch) 1)))

    (push (retirex aux1 (first pointeur) (first n-pitch)) aux)

    (dotimes (n (1- (length n-pitch)) (flat (reverse aux)))

      (setf aux1 (om-mod (om+ aux1 (nth (mod n (length transp)) transp)) mod))

      (push 
         (retirex    aux1  (nth (1+ n) pointeur)  (nth (1+ n) n-pitch)) aux))))

(defmethod! muzak3 ((liste list) (inlock list) (places list) (n-pitches list)) list
"interlock <inlock> sequence with <liste>.
<places> se référe à <liste> et <n-pitches> à <inlock>"
(let ((aux) 
      (plik (butlast (dx->x 1 n-pitches) 1))
      (pins (rest (dx->x 0 places)))
      (indice 0))

  (dotimes (n (length liste) (reverse aux))
    (push  (nth n liste) aux)
    (if (appartient? (1+ n) pins)
      (progn ()
           (setf aux  (append (reverse (retirex inlock (nth indice plik)  (nth indice n-pitches))) aux))
             (incf indice))))))




(defmethod! muzak4 ((serie list) (int  list)) list
"After each pitch, add another pitch in a specified intervallic relationship to it.
<int> is the intervallic list "
(let ((aux) (pointeur 0))
  (dotimes (n (length serie))
    (push (list (mod (+ (nth n serie)
                        (nth pointeur int) ) 12)
                (nth n serie))
          aux)
    (setf pointeur (mod (incf pointeur) (length int))))
  (reverse (flat aux))))


(defmethod! muzak5 ((serie list) (mod fix>0 (:value  12))) list
" Transpose all pitches so that their intervallic 
structure  is related to the first pitch
OBS:!:!:! Cette fonction permet la manipulation de séries numeriques modulo <mod>"
(om-mod (om+ (first serie) (cons 0  (x->dx serie))) mod))

(defmethod! muzak6 ((liste list) (n-pitch list) (mod fix>0 (:value  12))) list
"After a certain number of pitches (les places impaires de <liste>),
invert the others <n-pitches>
OBS:!:!:! Cette fonction permet la manipulation de séries numeriques modulo <mod>"
  (let ((aux)
        (places (butlast (edx->x 1 n-pitch) 1))
        (interv (cons 0 (x->dx liste))))
    (dotimes (n (/ (length n-pitch) 2) aux)    
      (push (reverse 
             (append (retirex interv (nth (* 2 n) places) (nth (* 2 n) n-pitch))
                     (om-mod (om- mod (retirex interv (nth (1+ (* 2 n)) places) (nth (1+ (* 2 n)) n-pitch))) mod)))
            aux))
    (om-mod (rest (edx->x  (first liste) (reverse (flat aux)))) mod)))



(defmethod! muzak7 ((serie list)  (ref-serie list)) list
"Replace every pitch, in <serie>, which is not contained in <ref-serie>
with the next available pitch from <ref-serie>"
(let ((indice 0) (aux) )
  (dotimes (n (length serie ))
    (if (appartient?  (nth n serie) ref-serie)     
      (push  (nth n serie) aux)
      (progn ()   
             (push (nth indice ref-serie) aux)
             (setf indice (mod (incf indice) (length ref-serie))))))
  (reverse aux)))

|#
