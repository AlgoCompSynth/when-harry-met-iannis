;;; When Harry Met Iannis 2004
;;; M. Edward Borasky
;;; znmeb@aracnet.com
;;; http://www.algocompsynth.com

#|
The chord being played at any one time is defined by the value of a state vector,
which we store as a list. The "first" of the list is the Otonality/Utonality switch,
which is +1 for Otonality and -1 for Utonality. The "second" of the list is the Octave,
where 1 is the Octave beginning with G 392 and going up to almost 2. The "third" of 
the list is the Numerary Nexus, and the rest of the list is the identities in the
chord.

The transition from one state to the next is done in two stages. In the first stage,
we pick with a 25% probability from the following alternatives:

1. Flip the Otonality/Utonality switch,
2. Go up or down an Octave,
3. Pick a new Numerary Nexus, and
4. Add or delete an Identity.

Each of these cases decides exactly what to do on its own.
|#

;;; low-level utilities
(defun choose-one (list)
	(nth (random (length list)) list))

(defun Select-OUSwitch (State-Vector)
	(first State-Vector))

(defun Select-Octave (State-Vector)
	(second State-Vector))

(defun Select-Nexus (State-Vector)
	(third State-Vector))

(defun Select-identities (State-Vector)
	(rest (rest (rest State-Vector))))

(defun Construct-State-Vector (OUSwitch Octave Nexus identities)
(append (list OUSwitch Octave Nexus) identities))

;;; Construct a random transformation of a state vector
(defun Construct-Random-Transformation (State-Vector)
	(let ((case-number (random 4))); this is the major case definition
		(cond ((equal case-number 0) (Change-OUSwitch State-Vector)); Otonality/Utonality switch
			((equal case-number 1) (Change-Octave State-Vector)); go up or down an Octave
			((equal case-number 2) (Change-Nexus State-Vector)); pick a new Numerary Nexus
			((equal case-number 3) (Change-idents State-Vector)); add or drop an identity
			(t State-Vector))))

(defun Change-OUSwitch (State-Vector)
"piece of cake -- just Change the sign of the first element"``
	(Construct-State-Vector 
		(* -1 (Select-OUSwitch State-Vector))
		(Select-Octave State-Vector)
		(Select-Nexus State-Vector)
		(Select-identities State-Vector)))

(Change-OUSwitch '(1 2 3 4 5))
(Change-OUSwitch '(-1 2 3 4 5))

;;; Octave up or down: pick a direction at random and go that way unless it's out
;;; of bounds. If it's out of bounds, go the other way.
(defConstant *Low-Octave* 1/2); lowest possible Octave
(defConstant *High-Octave* 4); highest possible Octave
(defun Change-Octave (State-Vector)
	(let* ((old (Select-Octave State-Vector)); input Octave number
	(new (* old (expt 2 (- (* (random 2) 2) 1))))); tentative new Octave value
	(cond ((> new *High-Octave*) (setf new (/ new 4))); go the other way
		((< new *Low-Octave*) (setf new (* new 4))); go the other way
		(t new)); success
	(Construct-State-Vector 
		(Select-OUSwitch State-Vector)
		new
		(Select-Nexus State-Vector)
		(Select-identities State-Vector))))

(Change-Octave '(1 1/2 1 2 3))
(Change-Octave '(1 1 1 2 3))
(Change-Octave '(1 2 1 2 3))
(Change-Octave '(1 4 1 2 3))
(Change-Octave '(1 1/2 1 2 3))
(Change-Octave '(1 1 1 2 3))
(Change-Octave '(1 2 1 2 3))
(Change-Octave '(1 4 1 2 3))
(Change-Octave '(1 1/2 1 2 3))
(Change-Octave '(1 1 1 2 3))
(Change-Octave '(1 2 1 2 3))
(Change-Octave '(1 4 1 2 3))
(Change-Octave '(1 1/2 1 2 3))
(Change-Octave '(1 1 1 2 3))
(Change-Octave '(1 2 1 2 3))
(Change-Octave '(1 4 1 2 3))

;;; In the following, to make things simple, we use the list of identities starting with 4
(defConstant *Identity-List* '(4 5 6 7 9 11))

;; Change the Numerary Nexus
(defun Change-Nexus (State-Vector)
	(let* ((old-Nexus (Select-Nexus State-Vector))
		(new-Nexus old-Nexus))
		(loop while (equal new-Nexus old-Nexus)
			do (setf new-Nexus (choose-one *Identity-List*)))
		(Construct-State-Vector 
			(Select-OUSwitch State-Vector)
			(Select-Octave State-Vector)
			new-Nexus
			(Select-identities State-Vector))))

(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))
(Change-Nexus '(1 1/2 4 4 5 6 7 9 11))

;;; Now the hairy one -- Change the list of identities. First, we chose whether to drop or add one. If the
;;; list is too short, we have to add one. If it's too long, we have to drop one. Otherwise, we pick "add"
;;; or "drop" at random and do it.
(defConstant *Low-Chord-Length* 3); never less than three notes in a chord
(defConstant *High-Chord-Length* (length *Identity-List*)); never more than number of identities in a chord

(defun drop-ident (State-Vector)
"Drop an identity from the State Vector"
	(let* ((old-idents (Select-identities State-Vector))
		(which-to-drop (choose-one old-idents))
		(new-idents (delete which-to-drop (copy-list old-idents))))
		(if (<= *Low-Chord-Length* (length new-idents)) (Construct-State-Vector 
			(Select-OUSwitch State-Vector)
			(Select-Octave State-Vector)
			(Select-Nexus State-Vector)
			new-idents)
		(Construct-State-Vector 
			(Select-OUSwitch State-Vector)
			(Select-Octave State-Vector)
			(Select-Nexus State-Vector)
			old-idents))))

(defun in-listp (element list)
	(cond 
		((null list) nil)
		((equal element (first list)) t)
		(t (in-listp element (rest list)))))

(defun all-but (list1 list2 &optional (result nil))
"all the elements in list2 that are **not* in list1"
	(loop for element in list2 do
		(if (not (in-listp element list1))
			(setf result (cons element result))))
	result)	

(defun possible-new-idents (State-Vector)
"returns identities not in the state vector"
	(all-but (Select-identities State-Vector) *Identity-List*))

(defun pick-new-ident (State-Vector)
	(let* ((could-be (possible-new-idents State-Vector)))
		(cond 
			((null could-be) nil)
			(t (choose-one could-be)))))

(defun add-ident (State-Vector)
	(let* ((the-new-ident (pick-new-ident State-Vector)))
		(cond 
			((null the-new-ident) State-Vector)
			(t 
				(Construct-State-Vector
					(Select-OUSwitch State-Vector)
					(Select-Octave State-Vector)
					(Select-Nexus State-Vector)
					(sort (cons the-new-ident (Select-identities State-Vector)) #'<))))))

;;; finally -- the real function
(defun Change-idents (State-Vector)
	(let* ((ident-count (length (Select-identities State-Vector))))
		(cond 
			((<= ident-count *Low-Chord-Length*) (add-ident State-Vector))
			((>= ident-count *High-Chord-Length*) (drop-ident State-Vector))
			((zerop (random 1)) (add-ident State-Vector))
			(t (drop-ident State-Vector)))))

;;; convert state vector to list of ratios
(defun make-ratios (State-Vector)
	(let* (
		(OUSwitch (Select-ouswitch State-Vector))
		(Octave (Select-Octave State-Vector))
		(Nexus (Select-Nexus State-Vector))
		(idents (sort (Select-identities State-Vector) #'<)))
			(cond 
				((> OUSwitch 0)
					(loop for ident in idents 
						collect (/ (* Octave (* ident)) Nexus)))
				(t
					(loop for ident in idents 
						collect (/ (* Octave (* Nexus)) ident))))))

(defVar *diagnostic-log* nil); trace of the state vector history
;;; Main Compose Function!!
(defun Compose (minutes &optional (clock 0) (duration 1000) (State-Vector '(1 1 4 4 5 6 7 9 11)))
	(setf *diagnostic-log* nil)
	(loop while (<= clock (* 60000 minutes))
		do (setf *diagnostic-log* (append *diagnostic-log* (list State-Vector)))
		do (setf duration (+ 250 (random 1750)))
		collect (make-cope-chord
			clock
			duration
			(make-ratios State-Vector)
			127)
		do (setf clock (+ clock duration))
		do (setf State-Vector (Construct-Random-Transformation State-Vector))))
