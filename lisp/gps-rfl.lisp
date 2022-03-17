(load "auxfns.lisp")
(requires "gps")

;; Simple reflective process:
;; remember "shortcut" operators, each learnt from an example,
;; reminiscent of explanation-based generalization/learning.

(defun achieve-goals (state goals &optional (*ops* *ops*))
  (achieve-all (cons '(start) state) goals nil))

(defun minimize-preconds (state goals)
  "Removes each condition from state that is not necessary to achieve the goals."
  (remove-if
   (lambda (p) (achieve-goals (remove p state :test #'equal) goals))
   state))

(defun extract-op (state goals &optional (*ops* *ops*))
  "Extract a reflective operator that consolidates a path from state to goals. Requires that state achieves goals."
  (let* ((preconds (minimize-preconds state goals))
         (action (list 'reflect preconds goals))
         (res (achieve-goals preconds goals)))
    (op action
        :preconds preconds
        :add-list (cons (list 'executing action) (cdr res))
        :del-list (set-difference state (cdr res) :test #'equal)
        )))

(defun reflected? (c)
  (and (consp c) (consp (cdr c)) (consp (cadr c)) (eql 'reflect (caadr c))))

(reflected? '(executing (reflect '(a b c) '(d))))

(defun already-reflected? (cs)
  (some #'reflected? cs))

(defun gps-rfl (state goals)
  (let ((r (achieve-goals state goals)))
    (when r
      (remove-if
       #'atom
       (if (already-reflected? r)
           r
           (let ((rfl-op (extract-op state goals)))
             (use (cons rfl-op *ops*))
             r))))))

(use *banana-ops*)
(gps-rfl '(foo at-door on-floor empty-handed hungry chair-at-door) '(not-hungry))
(gps-rfl '(at-door on-floor empty-handed hungry chair-at-door) '(not-hungry))
(gps-rfl '(at-door on-floor has-ball hungry chair-at-door) '(not-hungry))
(length *ops*)
