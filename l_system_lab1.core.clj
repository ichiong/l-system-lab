(ns l_system_lab1.core
  (:require [quil.core :refer :all])  )

;; # Angle Utilities
;; The built-in functions use radians.  Good for computers, bad for humans.
;; These utilties will use angles.

(defonce ang-to-rad (/ (* Math/PI 2.0) 360.0))

(defn ang-cos "Take the cosine of the argument in degrees."
  [deg]
  (cos (* deg ang-to-rad)))

(defn ang-sin "Take the sine of the argument in degrees."
  [deg]
  (sin (* deg ang-to-rad)))


;; ## Turtle Commands
;;
;; The Quil system has some specific commands for drawing graphics.
;; This function runs those commands.


(defn run-turtle
  "Draws a line, if the next command is a line vector."
  [v]
  (case (first v)
    :line (let [[_ x1 y1 x2 y2] v]
            (line x1 y1 x2 y2))))

(defn l-to-turtle [init-cmds init-state init-stack]
  (loop [cmds init-cmds
         state init-state
         stack init-stack
         out []]
    (if (not-empty cmds)
      (let [c (first cmds)]
        (case c
          :<  (recur (rest cmds)
                state
                (cons state stack)
                out)
          :f  (let [nux (+ (:x state)  (ang-cos (:angle state)))
                    nuy (+ (:y state)  (ang-sin (:angle state)))]
                 (recur (rest cmds)
                   (assoc state :x nux :y nuy)
                   stack
                   (conj out [:line (:x state) (:y state) nux nuy])))
          :> (recur (rest cmds)
               (first stack)
               (rest stack)
               out)
          :+ (recur (rest cmds)
               (update-in state [:angle] + (:theta state)) stack out)
          :- (recur (rest cmds)
               (update-in state [:angle] - (:theta state)) stack out)
          (recur (rest cmds)
               state
               stack
               out)))
       out)))

(defn remove-empties
  [v]
  (filter #(not= [] %) v))

;; Student function: scale-turtle
;;
;; Input: a list of [:line a b c d] vectors.  Some of the vectors may be empty: []
;; Output: a list that has been centered about coordinates 250,250, scaled to size 480x480.
;; I.e. '([:line 30 0 50 -50] [])  would be converted to '([10 250 480 10])
;;


;; You will need to rewrite this.  This code is just for show.


(defn vec-x
  [line-vec]
  "Creates a vector of all x-elements from the line vector."
  (let [v     (vec (rest line-vec))
        out     []]
    (if (empty? v) nil
          (conj out (v 0)(v 2)))))

(defn vec-y
  [line-vec]
  "Creates a vector of all x-elements from the line vector."
  (let [v     (vec (rest line-vec))
        out     []]
    (if (empty? v) nil
          (conj out (v 1)(v 3)))))

(defn get-xy-scale-one
  [line-vec]
  "Get the scaling factor for the coordinates."
  (let [l-vec1 (line-vec 0)
        [a b c d] (rest l-vec1)
        out []
        x-vec (into out [a c])
        min-x (apply min x-vec)
        max-x (apply max x-vec)
        range-x (- max-x min-x)
        y-vec (into out [b d])
        max-y (apply max y-vec)
        min-y (apply min y-vec)
        range-y (- max-y min-y)]
   (cond (< range-x range-y)
             {:scale (double (/ 480 range-y)) :min-x min-x :min-y min-y}
             :else  {:scale (double (/ 480 range-x)) :min-x min-x :min-y min-y})))

(defn get-xy-scale-two
  [line-vec]
  "Get the scaling factor for the coordinates."
  (let [l-vec1 (line-vec 0)
        l-vec2 (line-vec 1)
        [a b c d] (rest l-vec1)
        [e f g h] (rest l-vec2)
        out []
        x-vec (into out [a c e g])
        min-x (apply min x-vec)
        max-x (apply max x-vec)
        range-x (- max-x min-x)
        y-vec (into out [b d f h])
        max-y (apply max y-vec)
        min-y (apply min y-vec)
        range-y (- max-y min-y)]
       (cond (< range-x range-y)
             {:scale (double (/ 480 range-y)) :min-x min-x :min-y min-y}
             :else  {:scale (double (/ 480 range-x)) :min-x min-x :min-y min-y})))

(defn get-xy-scale
  [line-vec]
  (cond (= (count line-vec) 1)  (get-xy-scale-one line-vec)
        :else (get-xy-scale-two line-vec)))


(defn scale-turtle-one
  [line-vec]
  (let [l-vec1 (line-vec 0)
        [a b c d] (rest l-vec1)
        out []
        x-vec (into out [a c])
        min-x (apply min x-vec)
        max-x (apply max x-vec)
        range-x (- max-x min-x)
        y-vec (into out [b d])
        max-y (apply max y-vec)
        min-y (apply min y-vec)
        range-y (- max-y min-y)
        scale   (:scale (get-xy-scale line-vec))]
    (into [] [(double (-> a (- min-x) (* scale) (+ 10)))
          (double (-> b (- min-y) (* scale) (+ 10)))
          (double (-> c (- min-x) (* scale) (+ 10)))
          (double (-> d (- min-y) (* scale) (+ 10)))])))

(defn transform-help
  [init-pat rules]
  (loop [v-pat       init-pat
         out         []]
    (if empty? rules) init-pat
    (if (empty? v-pat) out
      (if (= (first (keys rules)) (first v-pat))
        (recur (rest v-pat) (into out (rules (first (keys rules)))))
        (recur (rest v-pat) (conj out (first v-pat)))))))

;; Make it do something here! --- I just did.

(defn transform
  [init-pat rules]
  (loop [n (count (keys rules))
         patout init-pat]
    (cond (= n 0) patout
      :else (do (def currkey ((into [] (keys rules))(- n 1)))
           (def currpat (currkey rules))
        (recur (- n 1) (transform-help patout {currkey currpat}))))))



;; # Some fractals to start out with.  Add some of your own!

(def dragon-curve
  {:init [:f :x]
   :rules {:x [:x :+ :y :f :+]
           :y [:- :f :x :- :y]}
   :theta 90
   :init-angle 0})

(def koch-curve
  {:init  [:f :- :- :f :- :- :f]
   :rules {:f [:f :+ :f :- :- :f :+ :f]}
   :theta 60
   :init-angle 0})

(def menger-curve
  {:init [:f :- :f :- :f :- :f]
   :rules {:f [:f :+ :f :- :f :- :f :+ :f]}
   :theta 90
   :init-angle 0})

;; # Quil graphics routines.  You don't need to do much with these.

(defn setup []
  (smooth)
  (background 200))

(def current (atom {}))

(defn l-system [pattern]
  (fn []
    (background 255)
    (stroke 255 0 0)
    (stroke-weight 1)
    (let [state {:x 100 :y 300 :theta (:theta pattern) :angle (:init-angle pattern)}
          cmds (nth (iterate #(transform % (:rules pattern)) (:init pattern)) (dec (frame-count) ))
          t-cmds (l-to-turtle cmds state nil)
          scaled-t-cmds (scale-turtle (remove-empties t-cmds))
          _ (reset! current scaled-t-cmds)]
      (doall (doseq [c scaled-t-cmds]
               (run-turtle c))))
    (no-loop)))

(defsketch l-system-sketch
  :title "An L-System"
  :setup setup
  :draw (l-system menger-curve)
  :mouse-pressed redraw
  :size [500 500])


;; test

  ((into [] (keys (:rules dragon-curve)))(- 2 1))

(transform (:init koch-curve) (:rules koch-curve))
(get-xy-scale [[:line 1 2 3 4] [:line 9 8 7 6]])
(scale-turtle [:line ])


(count (keys(:rules dragon-curve)))

(def currkey ((into [] (keys (:rules dragon-curve)))(- 1 1)))

(def currpat (currkey (:rules dragon-curve)))


(transform [:please :work] {:please [:it] :work [:works!]})

(get-xy-scale [[:line 10 90 1000 95]])

(get-xy-scale [[:line 10 90 1000 95] [:line 2000 100 3000 110]])



(scale-turtle-one [[:line 90 90 95 1000]])