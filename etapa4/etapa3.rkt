#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let ([not-ideal? (lambda (engagement)
                         (or
                            (better-match-exists? (car engagement) (cdr engagement) (get-pref-list wpref (car engagement)) mpref engagements)
                            (better-match-exists? (cdr engagement) (car engagement) (get-pref-list mpref (cdr engagement)) wpref (map (lambda (pair) (cons (cdr pair) (car pair))) engagements))))])
    (let compute-engs ([engs engagements] [res '()])
      (if (null? engs)
          res
          (let ([curr-eng (car engs)]
                [rest-engs (cdr engs)])
            (if (not-ideal? curr-eng)
                (compute-engs rest-engs (cons curr-eng res))
                (compute-engs rest-engs res)))))))   


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let men-help ([men free-men] [engs engagements])        ;fctie recursiva pt iterare prin barbati
    (if (null? men)
          engs
          (let* ([curr-man (car men)]                             ;variabila pt barbatul curent
                 [mprefs (get-pref-list mpref curr-man)])         ;variabila care retine lista de preferinte a fiecarui barbat               
            (let women-help ([women mprefs])               ;fctie recursiva pt iterat prin femei
              (if (null? women)
                  engs
                  (let* ([curr-woman (car women)]                      ;variabila pt femeia curenta
                         [wprefs (get-pref-list wpref curr-woman)]     ;variabila care retine lista de preferinte a fiecarei femei
                         [her-partner (get-partner engs curr-woman)])  ;variabila pt partenerul femeii curente
                    (cond
                      [(not her-partner) (men-help (cdr men) (cons (cons curr-woman curr-man) engs))]
                      [(preferable? wprefs curr-man her-partner) (men-help (cons her-partner (cdr men)) (update-engagements engs curr-woman curr-man))]
                      [else (women-help (cdr women))]))))))))
      
; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (curry (lambda (pair list) (cons (car pair) (cons (cdr pair) list)))) '() pair-list))

