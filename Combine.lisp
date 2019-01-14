;;
;;
;;            Combine
;;
;;      by Mikhail Malt  © IRCAM 1996
           
(in-package :om)

(compile&load (om-relative-path '("sources") "ferneyhough"))


; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)

(om::fill-library '(("Basic" nil  nil (ser-op rot90 serie-space) nil)
                    ("Permut" nil nil (combinations messiaen prolifer saw rand-saw
                                                                      circ-saw oscil-permutn rev-saw spiral kreus kreus-1
                                                                      permut-dyn permut-dyn1 single-to-1 single-to-x
                                                                      permut->to-1 permut->to-x) nil)
                    ("Analyse" nil  nil (l-analyse) nil)
                    ("Conv" nil  nil (mc->m m->mc octave-c3) nil)
                    ("Tools" nil  nil (geomt ratio ratio-freq g-subs gl-subs segment l-segment) nil)
                    ))


(om::set-lib-release 1.1)

(om::add-lib-alias "OMCombine" "Combine")

(print "
;;            Combine
;;
;;      by Mikhail Malt  © IRCAM 1996
")