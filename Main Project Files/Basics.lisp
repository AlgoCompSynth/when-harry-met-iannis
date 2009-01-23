;;; When Harry Met Iannis 2004
;;; M. Edward Borasky
;;; znmeb@aracnet.com
;;; http://www.algocompsynth.com


;;; constants
#|
The Partch system, as described in "Genesis of a Music", is a system of just intonation.
As such, it is based on ratios of frequencies. So, we define, as did Partch, unity -- known
in the Just Intonation circles as 1/1 -- as the G just below A 440. Partch lists it as G 392
but it's really 391.99 or thereabouts. Any other pitch will be computed as a ratio relative
to this value. For example, G 784 (MIDI 79) will be 2.
|#

(defConstant *MIDI-Note-For-Unity* 67) ; G (392) above middle C

; This is the Partch 43-note scale, mapped onto the octave between G 392 and G 784
(defConstant *Partch-Scale* '(1 81/80 33/32 21/20 16/15 12/11
                        11/10 10/9 9/8 8/7 7/6 32/27
                        6/5 11/9 5/4 14/11 9/7 21/16
                        4/3 27/20 11/8 7/5 10/7 16/11
                        40/27 3/2 32/21 14/9 11/7 8/5
                        18/11 5/3 27/16 12/7 7/4 16/9
                        9/5 20/11 11/6 15/8 40/21 64/33 160/81 2))

; we transpose a list of pitches by multiplying each element of the list by a ratio
(defun Transpose-By-Ratio (List-Of-Pitches Ratio)
  (if (null List-Of-Pitches) nil
      (cons (* (car List-Of-Pitches) Ratio)
            (Transpose-By-Ratio (cdr List-Of-Pitches) Ratio))))

#|
This is how we get a MIDI note number (floating!!) from a ratio. Soren Goodman's code
translates floating MIDI numbers to an integer note number and a pitch bend control message.
|#
(defun Ratio-To-MIDI-Note-Number (Ratio)
  (+ (* 12 (log Ratio 2))
        *MIDI-Note-For-Unity*))

; test functions
;(ratio-to-midi-note-number 1)
;(ratio-to-midi-note-number 2)

; And this is how we get a frequency from a MIDI note number
(defun MIDI-Note-Number-To-Frequency (Note-Number)
; A 440 equals 69
; A 880 equals 69 + 12, etc.
(* 440 (expt 2 (/ (- Note-Number 69) 12))))

; test functions
;(MIDI-Note-Number-To-Frequency 69)
;(MIDI-Note-Number-To-Frequency 81)
;(MIDI-Note-Number-To-Frequency 67)
;(MIDI-Note-Number-To-Frequency 60)

; sometimes, we need to force a ratio to lie between two numbers.
(defun Force-Between (Ratio Low High)
  (cond ((< Ratio Low)
         (Force-Between (* Ratio 2) Low High))
        ((>= Ratio High)
         (Force-Between (/ Ratio 2) Low High))
        (T Ratio)))

; test functions
;(force-between 1 4 8)
;(force-between 3/2 2 4)

; create a single Cope Event
; on-time and duration are in milliseconds
; channel is an integer
; pitch is a MIDI note number
; velocity ranges from 0 to 127
(defun Make-Cope-Event (on-time channel duration pitch velocity)
  (list on-time pitch duration channel velocity))

; test functions
;(Make-Cope-Event 1000 1 1000 67 127)

;;; and now, a real piece -- well, a scale :)
(defun Make-Cope-Scale (on-time duration velocity)
  (loop for ratio in *Partch-Scale* for counter upfrom 0
        collect (Make-Cope-Event 
                 (+ (* duration counter) on-time)
                 1; Soren's code ignores channels
                 duration 
                 (Ratio-To-MIDI-Note-Number ratio)
                 velocity)))

; test functions
;(Make-Cope-Scale 0 1000)
;(length (Make-Cope-Scale 0 1000))

;; let's actually listen to the scale
;(set-microtone-instrument)
(setf *dur* 250)
;play-events (make-cope-scale 0 *dur* 127))

;; define "chords" and "arpeggios"
(defun Make-Cope-Chord (on-time duration list-of-ratios velocity)
  (loop for ratio in list-of-ratios
        collect (Make-Cope-Event 
                 on-time
                 1; Soren's code ignores channels
                 duration 
                 (Ratio-To-MIDI-Note-Number ratio)
                 velocity)))

(defun Make-Cope-Arpeggio (on-time duration list-of-ratios velocity)
  (loop for ratio in list-of-ratios
        for counter upfrom 0
        collect (Make-Cope-Event 
                 (+ on-time (* duration counter))
                 1; Soren's code ignores channels
                 (* duration (- (length list-of-ratios) counter))
                 (Ratio-To-MIDI-Note-Number ratio)
                 velocity)))

#| Some definitions:

1. In the Partch system, there are two types of tonality, called "Otonalities" and
"Utonalities". An Otonality is defined as an ascending list of ratios with a common
*denominator*. The numerators are called "Odentities", a contraction of "Over Number"
and "Identity", and the denominator is called the Numerary Nexus. The Odentities and 
Numerary Nexus are chosen from the set {1 5 3 7 9 11}. To get the pitches to show up
in the right octave, one multiplies or divides by two as shown above in function 
"Force-Between". Otonalities correspond to major keys in "conventional" music theory.

2. By symmetry, we define a Utonality as a *descending* list of ratios with a common
*numerator*. The denominators are called "Udentities", a contraction of "Under Number"
and "Identity", and the numerator is called the Numerary Nexus. As before, the Udentities
and Numerary Nexus are chosen from the set {1 5 3 7 9 11}, and we use multiplication and 
division by two to get our pitches in the right octave. Utonalities correspond (somewhat)
to minor keys in "conventional" music theory.

3. Given six possible numerators and six possible divisors, we can make a (square) table 
of all the pitches in the six basic Otonalities and Utonalities. Partch rotates this
square 45 degrees and calls it the "Tonality Diamond", but we'll keep it as a square here.
|#

(defConstant *Identities* '(1 5 3 7 9 11))

(defun Construct-Otonality (Under)
  (loop for Over in *Identities*
               collect (force-between (/ Over Under) 1 2)))

; test function
(construct-otonality 1)
(construct-otonality 5)
(construct-otonality 3)
(construct-otonality 7)
(construct-otonality 9)
(construct-otonality 11)

(defun Construct-Utonality (Over)
  (loop for Under in *Identities*
               collect (force-between (/ Over Under) 1 2)))

; test function
(construct-utonality 1)
(construct-utonality 5)
(construct-utonality 3)
(construct-utonality 7)
(construct-utonality 9)
(construct-utonality 11)

#|
Now we have some raw materials. Let's see if we can put them together.  We'll cycle through
the tonalities as chords and as arpeggios. We want to do this so an Otonality corresponds to
a full chord up from a "root" note. That is, the lowest note will be the root, the next a major
third up from the root, the next a fifth up, the next a seventh up, the next a ninth up and the
last an eleventh up. Given that we're using "small number ratios", these chords are supposed to
sound "better" than their tempered equivalents.

We will do the same for the Utonalities, preserving the symmetry by starting at the *highest*
note of the chord and working our way *down*.  
|#

; sequence of intervals relative to the "root".
(defConstant *Intervals* '(4/4 5/4 6/4 7/4 9/4 11/4))

(defun Construct-Otonality-Hexad (Starting-Ratio)
  (loop for Interval in *Intervals* 
        collect (* Starting-Ratio Interval)))

(defun Construct-Utonality-Hexad (Starting-Ratio)
  (loop for Interval in *Intervals* 
        collect (/ Starting-Ratio Interval)))

; set up to play notes
;(set-microtone-instrument)
(setf *dur* 1500)

#|
Play the otonality chords -- these sound good with Synth Lead Calliope or Synth Pad Fantasy or Drawbar Organ.
See the Diamond Marimba on page 261 of "Genesis of a Music" for the basic layout. We'll start at the 1/1 on 
the far left and go down and to the right for the root. The chord goes up and to the right.
|#

;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/5) 127))
;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/6) 127))
;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/7) 127))
;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/9) 127))
;play-events (make-cope-chord 0 *dur* (construct-otonality-hexad 4/11) 127))

#|
Play the utonality chords. We'll use the same Diamond Marimba, and we're starting at the 1/1 on the far left
again. These are *Utonalities*, so they "hang downward to the left" from the "root", and we'll walk the root 
*up* and to the left.
|#

;(set-microtone-instrument)
(setf *dur* 1500)
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 4/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 5/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 6/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 7/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 9/4) 127))
;play-events (make-cope-chord 0 *dur* (construct-utonality-hexad 11/4) 127))

#|
It's a little easier to hear what's going on if we add one note at a time
|#

; otonalities
;(set-microtone-instrument)
(setf *dur* 1500)
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/5) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/6) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/7) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/9) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-otonality-hexad 4/11) 127))

; utonalities
;(set-microtone-instrument)
(setf *dur* 1500)
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 4/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 5/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 6/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 7/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 9/4) 127))
;play-events (make-cope-arpeggio 0 *dur* (construct-utonality-hexad 11/4) 127))
