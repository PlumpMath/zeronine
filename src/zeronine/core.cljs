(ns zeronine.core
  (:require [reagent.core :as r :refer [atom]]
            [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
    [cljs.core.async.macros :refer [go-loop go]]))

(comment

  hellooooo
  I am a comment and I'm soooo lovely

  )

(enable-console-print!)

(def spacing-x 100)
(def spacing-y 100)
(def dot-size 80)

(defn fps-to-millis [fps]
  (/ 1000 fps))

(def time-loop-interval (fps-to-millis 60))
(def playhead-loop-interval (fps-to-millis 3))

;                    0       1       2        3         4         5         6         7         8         9
;                    nil     solo    duo      triad     quad      pent      hex       sept      oct       non
(def key-positions [[[0 0 0] [0 0 1] [-1 0 1] [-1 -1 1] [-1 -1 1] [-1 -1 1] [-2 -2 1] [-2 -2 1] [-2 -2 1] [-2 -2 1]]
                    [nil     [0 0 0] [1 0 1]  [1 -1 1]  [1 -1 1]  [1 -1 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]]
                    [nil     nil     [1 0 0]  [0 1 1]   [1 1 1]   [1 1 1]   [2 2 1]   [2 2 1]   [2 2 1]   [2 2 1]]
                    [nil     nil     nil      [0 1 0]   [-1 1 1]  [-1 1 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]]
                    [nil     nil     nil      nil       [-1 1 0]   [0 0 1]   [-1 0 1]  [-1 -1 1] [-1 -1 1] [-1 -1 1]]
                    [nil     nil     nil      nil       nil       [0 0 0]   [1 0 1]   [1 -1 1]  [1 -1 1]  [1 -1 1]]
                    [nil     nil     nil      nil       nil       nil       [1 0 0]   [0 1 1]   [1 1 1]   [1 1 1]]
                    [nil     nil     nil      nil       nil       nil       nil       [0 1 0]   [-1 1 1]  [-1 1 1]]
                    [nil     nil     nil      nil       nil       nil       nil       nil       [-1 1 0]   [0 0 1]]])

;(def step-sequence  [0 1 2 3 4 5 6 7 8 9])
;(def step-sequence  [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1])
(def step-sequence  [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1
                     0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
                     0 1 2 3 4 5 6 7 6 5 4 3 2 1
                     0 1 2 3 4 5 6 5 4 3 2 1
                     0 1 2 3 4 5 4 3 2 1
                     0 1 2 3 4 3 2 1
                     0 1 2 3 2 1
                     0 1 2 1
                     0 1
                     0])




(defn make-trackable []
  {:target  0
   :forces  0
   :current 0})

(defn make-tracking-dot []
  (assoc {} :x (make-trackable)
            :y (make-trackable)
            :a (make-trackable)))



(defonce app-state (atom {:step-index        0
                          :running           true
                          :target-positions  [nil nil nil nil nil nil nil nil nil]
                          :current-positions [nil nil nil nil nil nil nil nil nil]
                          :forces            [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
                          :wrapping          true
                          :blorg             0
                          :tracking-dots     (take 9 (repeat (make-tracking-dot)))
                          }))

(defn dot [index dot-position]
  (let [[x y a] dot-position]
    (if dot-position
      [:div {:key index
             :style {:position "absolute"
                     :width 0
                     :height 0
                     :left (* x spacing-x)
                     :top (* y spacing-y)
                     :opacity a}
             }
       [:div {:style {:position "relative"
                      :top (- (/ spacing-y 2))
                      :left (- (/ spacing-x 2))
                      :width dot-size
                      :height dot-size
                      :background-color "#000"
                      :display "flex"
                      :justify-content "center"
                      :alignItems "center"
                      :font-size 30
                      ;:color "white"
                      :color "rgba(0, 0, 0, 0.2)"
                      :font-family "\"Lucida Console\", Monaco, monospace"
                      :border-style "solid"
                      :border-width 8
                      :border-color "rgba(0, 0, 0, 0.1)"
                      :border-radius dot-size
                      ;:box-shadow "0px 20px 40px rgba(0, 0, 0, 0.2)"
                      }
               :onClick (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (println (str "dot click " dot-position)))}
        (inc index)]])))

(defn set-wrapping-2 [is-wrapping]
  (swap! app-state (fn [state]
                     (assoc state :wrapping is-wrapping))))

(defn on-mouse-down [e]
  (let [{:keys [forces current-positions] :as state} @app-state]
    ;(set-wrapping-2 true)
    ;(println (str "forces: " forces))
    (println (str "current-positions: " current-positions))
    (println "mouse down")
    ))

(defn on-mouse-up [e]
  ;(set-wrapping-2 false)
  (println "mouse up"))

(defn step-tracking-dot [{:keys [x y a] :as tracking-dot}]
  (println (str "lehhhhhhh" tracking-dot))
  {:x {:target  (+ (:target x) 1)
       :forces  (+ (:forces x) 1)
       :current (+ (:current x) 1)}
   :y y
   :a a}
  )

(defn assign-targets [{:keys [target-positions tracking-dots] :as state}]
  (map-indexed
    (fn [index tracking-dot]
      (let [{:keys [x y a]} tracking-dot
            [tx ty ta] (get target-positions index)]
        (println (str tx ty ta))
        {:x (assoc x :target (get (get target-positions index) 0))
         :y (assoc y :target (get (get target-positions index) 1))
         :a (assoc a :target (get (get target-positions index) 2))}))
    tracking-dots))

(defn app []
  (let [{:keys [current-positions]} @app-state
        dot-divs (map-indexed dot current-positions)
        ]
    [:div {:style       {:position         "relative"
                         :width            (.-innerWidth js/window)
                         :height           (.-innerHeight js/window)
                         ;:background-color "#ececec"
                         :background-color "#444"
                         }
           :onMouseDown on-mouse-down
           :onMouseUp   on-mouse-up
           }
     [:div {:style {:position "relative"
                    :height 0
                    :width 0
                    :left (/ (.-innerWidth js/window) 2)
                    :top (/ (.-innerHeight js/window) 2)}}
      dot-divs]]))

(r/render-component [app] (. js/document (getElementById "app")))

;(defn move-num [n]
;  ; diff target - current
;  ; spring, friction
;  ;
;  (+ n 0.01)
;  )

;(defn move-position [[x y a :as position]]
;  (if (nil? position)
;    nil
;    [(move-num x) (move-num y) (move-num a)]
;    ))

;(defn step-tracking-dot [td]
;  (println "hello")
;  )

(defn set-target-positions [{:keys [position-index] :as state}]
  (assoc state :target-positions (into [] (map (fn [dot] (get dot position-index)) key-positions))))

(defn move-step-index [amount-to-move]
  (swap! app-state
         (fn [{:keys [step-index wrapping tracking-dots] :as state}]
           (let [last-step-index (- (count step-sequence) 1)
                 new-step-index (+ step-index amount-to-move)
                 step-index-for-swap (cond
                                       (< new-step-index 0) (if wrapping last-step-index 0)
                                       (> new-step-index last-step-index) (if wrapping 0 last-step-index)
                                       :else new-step-index)
                 position-index (get step-sequence step-index-for-swap)
                 ]
             ;(println tracking-dots)
             ;(map step-tracking-dot tracking-dots)
             (-> state
                 (assoc :step-index step-index-for-swap)
                 (assoc :position-index position-index)
                 ;(assoc :target-positions (into [] (map (fn [dot] (get dot position-index)) key-positions)))
                 (set-target-positions)
                 ;(assoc :current-positions (map (fn [dot] (get dot position-index)) key-positions))
                 ;(assoc :tracking-dots (map step-tracking-dot tracking-dots))
                 )))))

(defn toggle-running []
  (swap! app-state
         (fn [state]
           (-> state
               (assoc :running (not (:running state)))))))

;(defn on-click [e]
;  ;(println (str "step index: " (:step-index @app-state)))
;  (let [{:keys [steps step-index step-sequence positions]} @app-state]
;    (println "clicffk")))

(defn on-key-down [e]
  (let [keyCode (.-keyCode e)]
    (case keyCode
      ;32 (println "space")
      32 (toggle-running)
      ;37 (println "left")
      ;39 (println "right")
      37 (move-step-index -1)
      39 (move-step-index 1)
      (println (str keyCode " is not recognized key")))))

(defn add-listeners []

  (println "adding listeners")
  (set! (.-onkeydown js/document) on-key-down)
  ;(set! (.-onclick js/document) on-click)
  )

(defn divide-to-val [cv tv divide-by]
  (let [diff (- tv cv)
        move (/ diff divide-by)]
    (+ cv move)))

;(defn divide-tween-current-positions [{:keys [target-positions current-positions] :as state}]
;  (assoc state :current-positions (into [] (map-indexed (fn [index [tx ty ta :as targ-pos]]
;                                                          (if (nil? targ-pos)
;                                                            nil
;                                                            (let [[cx cy ca :as cur-pos] (get current-positions index)]
;                                                              [(divide-to-val cx tx 2)
;                                                               (divide-to-val cy ty 2)
;                                                               (divide-to-val ca ta 2)]))) target-positions))))


(defn get-jitter-amount []
  (* (- (rand) (rand)) 0.02))

(defn jitter-position [[x y a :as position]]
  (if (nil? position)
    nil
    [(+ x (get-jitter-amount)) (+ y (get-jitter-amount)) (+ a (get-jitter-amount))]
    ))

(defn jitter-current-positions [{:keys [current-positions] :as state}]
  (assoc state :current-positions (into [] (map jitter-position current-positions))))

(defn force-tween-val [current-val target-val force]
  (let [diff (- target-val current-val)]
    ;(println (str "force tweening val diff: " diff))
    target-val))

;(defn force-tween-current-pos [current-pos target-pos force]
;  [
;   (force-tween-val (get current-pos 0) (get target-pos 0) (get force 0))
;   (force-tween-val (get current-pos 1) (get target-pos 1) (get force 1))
;   (force-tween-val (get current-pos 2) (get target-pos 2) (get force 2))
;   ])

(defn apply-force [force cval tval]
  (let [diff (- tval cval)
        new-force (+ force (* diff 0.1))                   ; spring
        new-force (* new-force 0.8)]                        ;friction
    ;(println (str "applying force: " force cval tval))
    new-force))

(defn update-forces-before-apply [{:keys [target-positions current-positions forces] :as state}]
  (-> state
      (assoc :forces
             (into []
                   (map-indexed
                     (fn [index force]
                       (let [cp (get current-positions index)
                             tp (get target-positions index)]
                         [(apply-force (get force 0) (get cp 0) (get tp 0))
                          (apply-force (get force 1) (get cp 1) (get tp 1))
                          (apply-force (get force 2) (get cp 2) (get tp 2))]))
                     forces)))))

(defn apply-forces [{:keys [current-positions forces] :as state}]
  (-> state
      (assoc :current-positions
             (into []
                   (map-indexed
                     (fn [index current-pos]
                       (let [force (get forces index)]
                         [(+ (get current-pos 0) (get force 0))
                          (+ (get current-pos 1) (get force 1))
                          (+ (get current-pos 2) (get force 2))]))
                     current-positions)))))

(defn update-forces-after-apply [{:keys [forces] :as state}]
  (-> state
      (assoc :forces
             (into []
                   (map
                     (fn [force]
                       (let [drag 0.98]
                         [(* (get force 0) drag)
                          (* (get force 1) drag)
                          (* (get force 2) drag)]))
                     forces)))))


(defn step-world [app-state]
  ;(println "stepping world")
  (swap! app-state (fn [{:keys [target-positions] :as state}]
                     (-> state
                         ;(force-tween-current-positions)
                         (update-forces-before-apply)
                         (apply-forces)
                         (update-forces-after-apply)
                         ;(divide-tween-current-positions)
                         (jitter-current-positions)
                         ))))

(defn time-loop []
  (go
    (<! (timeout time-loop-interval))
    (when (:running @app-state)
      ;(move-step-index 1)
      (step-world app-state)
      ;(println "tick")
      )
    (time-loop)))



(defn playhead-loop []
  (go
    (<! (timeout playhead-loop-interval))
    (when (:running @app-state)
      ;(move-step-index 1)
      ;(step-world app-state)
      (move-step-index 1)
      )
    (playhead-loop)))

(defonce init
         (do
           (println "kickoff!")
           (add-listeners)
           (time-loop)
           (playhead-loop)
           ))

(defn on-js-reload []
  ;;; (println "reload")
  ;;; optionally touch your app-state to force rerendering depending on
  ;;; your application
  ;;; (swap! app-state update-in [:__figwheel_counter] inc)
  (add-listeners)
)

(comment

  :step-sequence [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1
                  0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
                  0 1 2 3 4 5 6 7 6 5 4 3 2 1
                  0 1 2 3 4 5 6 5 4 3 2 1
                  0 1 2 3 4 5 4 3 2 1
                  0 1 2 3 4 3 2 1
                  0 1 2 3 2 1
                  0 1 2 1
                  0 1
                  0]

  )