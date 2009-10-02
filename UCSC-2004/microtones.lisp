#|  
;;;2004 Soren Goodman
;;;From code by Jon Hallstrom, Dale Skrien, Alexander Repenning, and David Cope
;;;For WACM 2004 
;;;Microtones.lisp

Sample events:

(setq *events* (loop for n from 1 to 30 collect (list (* n 1000) (+ 50 (* n 1.5)) 1000 1 127)))
(setq *events* (loop for n from 1 to 30 collect (list (* n 250) (+ 50 (* n 1.5)) 300 1 127)))

The code in this document will allow you to effect program changes, including pitch bend. 
This is intended to be used with David Cope's standard midi init.lisp functions.

For microtones, simply make your pitches floats.  If *enable-microtones* is t, then floats automatically get converted to
pitch bend microtones.  All notes are also delayed by 500ms, in case the first note is microtonal.   

Note, however, that because of the way pitch bend works, it is necessary to cycle through channels.  
The available channels, also the available polyphony, are contained in *microtone-channels*, as a list of available channels.
You'll probably want to make sure that they are all set to the same instrument!  [Note:  Quicktime doesn't need this, since
we could allocate different channels with the same instrument, but any other midi device would need this to happen]. 


This new Play-Events and supporting files will allow you to insert controller changes into a list of events.  

The available controllers, not all of which will work with every (or any!) instrument are:

kControllerModulationWheel    = 1
kControllerBreath             = 2 
kControllerFoot               = 4 
kControllerPortamentoTime     = 5 
kControllerVolume             = 7 
kControllerBalance            = 8 
kControllerPan                = 10 
kControllerExpression         = 11 
kControllerLever1             = 16 
kControllerLever2             = 17 
kControllerLever3             = 18 
kControllerLever4             = 19 
kControllerLever5             = 80 
kControllerLever6             = 81 
kControllerLever7             = 82 
kControllerLever8             = 83 
kControllerPitchBend          = 32
kControllerAfterTouch         = 33 
kControllerSustain            = 64 
kControllerSostenuto          = 66 
kControllerSoftPedal          = 67 
kControllerReverb             = 91 
kControllerTremolo            = 92 
kControllerChorus             = 93 
kControllerCeleste            = 94 
kControllerPhaser             = 95 
kControllerEditPart           = 113 
kControllerMasterTune         = 114 


PitchBend is 32, Sustain pedal is 64.


to play a control change, insert an event with a list of (ontime new-value nil channel nil controller-number)

This will set controller-number on channel to new-value at ontime
the third and fifth slots are ignored.

These can be inserted into a regular list of events, thus 

'((0 50 2000 1 100)(750 128 nil 1 nil 32)(1500 0 nil 1 nil 32))

will play a note at pitch 50, bend it up a quarter-tone, then back to 0.
CAUTION:  Duration has no effect for controller changes; they stick until you change them again.  In other words, if 
you do a pitch bend on channel 1, that applies to every note played on channel 1.  

******Make sure to set the bend back to 0 when you're done!***********
You can also call (stop-the-music) to reset everything.

|#

(defparameter *enable-microtones* t)  ;;if enabled, automatically convert pitches to microtones
(defparameter *microtone-channels* '(1 2 3 4 5 6))  ;;these are the channels, and thus polyphony, available to microtones.  Should be same instrument.

(defparameter *microtone-instrument* 1)
(defun set-microtone-instrument ()
  (setf *microtone-instrument* (user-pick-instrument)))

(defun PLAY-Controller (Amt Controller &optional (Instrument-Number (cons 1 (user-pick-instrument))))
  "Set value of Controller <controller> to <amt> on <Allocation.Instrument-Number>."
       ;(when (= 0 (cdr Instrument-Number)) (return-from play-controller))
       (let ((Instrument (gethash (car Instrument-Number) *Instruments*)))
         (cond
          (Instrument ;;; found instrument in cache!
           (QTMA-error (#_NASetController (first Instrument) (second Instrument) Controller Amt)))
          (t
           (multiple-value-bind (Allocator Channel Request)
                                (make-instrument-allocator-and-channel (or (and *enable-microtones* *microtone-instrument*)
                                                                           (cdr Instrument-Number)))
             (setf (gethash (car Instrument-Number) *Instruments*) (list Allocator Channel Request))
             (QTMA-error (#_NASetNoteChannelVolume Allocator Channel 100000))
             (QTMA-error (#_NASetController Allocator Channel Controller Amt)))))))


(defun PLAY-NOTE (Pitch Velocity &optional (Instrument-Number (cons 1 (user-pick-instrument))))
  "Play a note with <Pitch> at <Velocity> on <Allocation.Instrument-Number>."
  ;(when (= 0 (cdr Instrument-Number)) (return-from play-note))
  (let ((Instrument (gethash (car Instrument-Number) *Instruments*)))
    (cond
     (Instrument ;;; found instrument in cache!
      (QTMA-error (#_NAPlayNote (first Instrument) (second Instrument) Pitch Velocity)))
     (t
      (multiple-value-bind (Allocator Channel Request)
                           (make-instrument-allocator-and-channel (or (and *enable-microtones* *microtone-instrument*)
                                                                      (cdr Instrument-Number)))
        (setf (gethash (car Instrument-Number) *Instruments*) (list Allocator Channel Request))
        (QTMA-error (#_NASetNoteChannelVolume Allocator Channel 100000))
        (QTMA-error (#_NAPlayNote Allocator Channel Pitch Velocity)))))))




(defun Play-Events (events)
  (stop-the-music)
  (sleep 1)
  (play-xevents events))

(defun PLAY-xEvents (events) 
    (do* ((changed-events (make-Xplay-list events))
          (time (get-internal-real-time))
          (event (first changed-events) (first changed-events)))
         ((null changed-events) t)
      (if (<= (first event) (- (get-internal-real-time) time))
        (progn
          (if (fifth event) 
            (play-controller
             (second event)
             (third event)
             ; (or (and *enable-microtones* (cons *microtone-instrument* (get-channel-instrument (fourth event))))
             (cons (fourth event)(get-channel-instrument (fourth event))))  ;;need to keep track of the allocation number, not just the instrument!
            (play-note
             (second event)
             (third event)
             ;(or (and *enable-microtones* (cons *microtone-instrument* (get-channel-instrument (fourth event))))
             (cons (fourth event)(get-channel-instrument (fourth event)))))  ;;here too!
          (setf changed-events (rest changed-events))))))
    
(defun MAKE-xPlay-LIST (events)
  (sortcar  
     #'< 
     (apply #'append 
            (loop for event in (convert-the-xevents (or (and *enable-microtones* (convert-microtonal-events events)) events))
                  collect (if (sixth event)
                            (list 
                             (list 
                              (first event)
                              (second event)
                              (sixth event)
                              (fourth event)
                              'controller))
                            (list 
                             (list 
                              (first event)
                              (second event)
                              (fifth event)
                              (fourth event))
                             (list (+ (first event)(third event))
                                   (second event) 
                                   0 
                                   (fourth event))))))))

(defun CONVERT-THE-xEVENTS (events)
  (loop for event in events collect
        (if (sixth event)
          (cons (floor (* (/ 60 *tempo*)(first event)))
                (rest event))
          (list (floor (* (/ 60 *tempo*)(car event)))
                (+ (second event) *key*)
                (floor (* (/ 60 *tempo*)(third event)))
                (fourth event) 
                (fifth event)))))
 

(defun convert-microtonal-events (events)
  "this function takes any pitch with float and converts it to a bend-event plus the original note. 
*microtone-channels* is a list determining what range to use for the microtones, since it needs to
cycle through channels for controller timing and to keep the notes individual."

  (do* ((n 0 (1+ n))
        (new-events (mapcar #'(lambda (x) (cons (+ 500 (first x)) (rest x))) events))  ;;we need to delay the start by 500 in case we need to start with a bend.
        (num-events (length new-events))
        (event (first new-events)(nth n new-events))
        (output nil)
        (channel-range (length *microtone-channels*))
        (current-channel (first *microtone-channels*) (nth (mod n channel-range) *microtone-channels*)))
       ((>= n num-events)(apply #'append (reverse output)))
    (push
       (list 
        (list (- (first event) 500)
              (round (* (- (second event)(floor (second event))) 256))
              nil
              current-channel
              nil
              32)
        (list (first event)
              (floor (second event))
              (third event)
              current-channel
              (fifth event)))
       output)))
