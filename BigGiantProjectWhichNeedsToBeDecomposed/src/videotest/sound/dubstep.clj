(ns videotest.sound.dubstep
  (:use [overtone.core]))

(defsynth dubstep [bpm 120 wobble 1 note 50 snare-vol 1 kick-vol 1 v 1 out-bus 0]
  (let [trig (impulse:kr (/ bpm 120))
        freq (midicps note)
        swr (demand trig 0 (dseq [wobble] INF))
        sweep (lin-exp (lf-tri swr) -1 1 40 3000)
        wob (apply + (saw (* freq [0.99 1.01])))
        wob (lpf wob sweep)
        wob (* 0.8 (normalizer wob))
        wob (+ wob (bpf wob 1500 2))
        wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

        kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
        kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
        kick (clip2 kick 1)

        snare (* 3 (pink-noise) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
        snare (+ snare (bpf (* 4 snare) 2000))
        snare (clip2 snare 1)]

    (out out-bus    (* v (clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)))))

(comment
  ;;Control the dubstep synth with the following:
  (def d (dubstep))
  (ctl d :wobble 8)
  (ctl d :note 40)
  (ctl d :bpm 250)
  (stop)
  )


;;(stop)
