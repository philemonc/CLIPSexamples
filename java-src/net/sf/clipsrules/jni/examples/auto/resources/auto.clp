
;;;======================================================
;;;   Automotive Expert System
;;;
;;;     This expert system diagnoses some simple
;;;     problems with a car.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     For use with the Auto Demo Example
;;;======================================================

;;;*****************
;;;* Configuration *
;;;*****************
   
(defglobal ?*target* = gui) ; console, cgi, or gui

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (slot state (default middle)))
   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

(deffunction MAIN::find-text-for-id (?id)
   ;; Search for the text-for-id fact
   ;; with the same id as ?id
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

;;;*****************
;;;* STATE METHODS *
;;;*****************
      
;;; Console target
   
(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t ?display crlf)
   (str-assert (str-cat "(" ?relation-asserted " " yes ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target console))
                         (?question LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE) ; default
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (bind ?display-answers (sym-cat-multifield ?display-answers))
   (format t "%s " ?question)
   (printout t ?display-answers " ")
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?display-answers)) do
      (format t "%s " ?question)
      (printout t ?display-answers " ")
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   (bind ?pos (member$ ?answer ?display-answers))
   (bind ?answer (nth$ ?pos ?valid-answers))
   (str-assert (str-cat "(" ?relation-asserted " " ?answer ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME))
   (assert (conclusion))
   (printout t ?display crlf)
   (halt))

;;; CGI target

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t "state=greeting" crlf)
   (printout t "display=" ?display crlf)
   (printout t "variable=greeting" crlf)
   (printout t "validAnswers=yes" crlf)
   (printout t "displayAnswers=yes" crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target cgi))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (printout t "state=interview" crlf)
   (printout t "display=" ?message crlf)  
   (printout t "variable=" ?relation-asserted crlf)
   (printout t "validAnswers=" (multifield-to-delimited-string ?valid-answers ":") crlf)
   (printout t "displayAnswers=" (multifield-to-delimited-string ?display-answers ":") crlf) 
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME))
   (printout t "state=conclusion" crlf)
   (printout t "display=" ?display crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers yes)
                     (display-answers yes)))
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
   (halt))
 
(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME))
   (assert (UI-state (display ?display)
                     (state ?state)
                     (valid-answers)
                     (display-answers)))
   (halt))

;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""
  (not (greeting yes))
  =>
  (handle-state greeting
                ?*target*
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))
  
;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-starter-state ""

   (greeting yes)
   (not (starter-turns ?))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id StartQuestion)
                 starter-turns
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule determine-lights-working ""

   (starter-turns no)
   (not (lights-working ?))
   
   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id LightsQuestion)
                 lights-working
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-petrol-state ""

   (starter-turns yes)
   (not (got-petrol ?))

   =>

   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id PetrolQuestion)
                 got-petrol
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule solenoid-clicking-test ""

   (lights-working yes)
   (not (solenoid-clicking ?))

   =>
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id SolenoidQuestion)
                 solenoid-clicking
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))
   
(defrule determine-terminals-clean ""

   (solenoid-clicking yes)
   (not (terminals-clean ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id TerminalsQuestion)
                 terminals-clean
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule determine-solenoid-fuse ""

   (solenoid-clicking no)
   (not (solenoid-fuse ?))

   =>
   
   (bind ?answers (create$ no yes))
   (handle-state interview
                 ?*target*
                 (find-text-for-id SolenoidFuseQuestion)
                 solenoid-fuse
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

;;;****************
;;;* RECOMMENDATIONS *
;;;****************

(defrule got-petrol-yes ""
   (declare (salience 10))
   (got-petrol yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id CallAA)))
 
(defrule got-petrol-no ""
   (declare (salience 10))
   (got-petrol no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id BuyPetrol)))

(defrule lights-working-no ""
   (declare (salience 10))
   (lights-working no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ChangeBattery)))

(defrule replace-fuse ""
   (declare (salience 10))
   (solenoid-fuse no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ReplaceFuse)))

(defrule replace-solenoid ""
   (declare (salience 10))
   (solenoid-fuse yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ReplaceSolenoid)))
   
(defrule clean-terminals ""
   (declare (salience 10))
   (terminals-clean no)
   =>
   (handle-state conclusion ?*target* (find-text-for-id CleanTerminals)))

(defrule replace-starter ""
   (declare (salience 10))
   (terminals-clean yes)
   =>
   (handle-state conclusion ?*target* (find-text-for-id ReplaceStarter)))



                     

                     

  
