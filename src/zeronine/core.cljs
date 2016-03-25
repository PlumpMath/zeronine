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
(def spacing-y 90)
(def dot-size 100)

(defn fps-to-millis [fps]
  (/ 1000 fps))

(def time-loop-interval (fps-to-millis 20))

;                    0       1       2        3         4         5         6         7         8         9
;                    nil     solo    duo      triad     quad      pent      hex       sept      oct       non
(def key-positions [[[0 0 0] [0 0 1] [-1 0 1] [-1 -1 1] [-1 -1 1] [-1 -1 1] [-2 -2 1] [-2 -2 1] [-2 -2 1] [-2 -2 1]]
                    [nil     [0 0 0] [1 0 1]  [1 -1 1]  [1 -1 1]  [1 -1 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]]
                    [nil     nil     [1 0 0]  [0 1 1]   [1 1 1]   [1 1 1]   [2 2 1]   [2 2 1]   [2 2 1]   [2 2 1]]
                    [nil     nil     nil      [0 1 0]   [-1 1 1]  [-1 1 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]]
                    [nil     nil     nil      nil       [0 0 0]   [0 0 1]   [-1 0 1]  [-1 -1 1] [-1 -1 1] [-1 -1 1]]
                    [nil     nil     nil      nil       nil       [0 0 0]   [1 0 1]   [1 -1 1]  [1 -1 1]  [1 -1 1]]
                    [nil     nil     nil      nil       nil       nil       [1 0 0]   [0 1 1]   [1 1 1]   [1 1 1]]
                    [nil     nil     nil      nil       nil       nil       nil       [0 1 0]   [-1 1 1]  [-1 1 1]]
                    [nil     nil     nil      nil       nil       nil       nil       nil       [0 0 0]   [0 0 1]]])

(def step-sequence  [0 1 2 3 4 5 6 7 8 9])
;(def step-sequence  [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1])




(defn make-trackable []
  {:target  0
   :force   0
   :current 0})

(defn make-tracking-dot []
  (assoc {} :x (make-trackable)
            :y (make-trackable)
            :a (make-trackable)))



(defonce app-state (atom {:step-index        0
                          :running           true
                          :target-positions  [nil nil nil nil nil nil nil nil nil]
                          :current-positions [nil nil nil nil nil nil nil nil nil]
                          :force             [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
                          :wrapping          false
                          :blorg             0
                          :tracking-dots (take 9 (repeat (make-tracking-dot)))
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
                      :box-shadow "0px 5px 20px rgba(0, 0, 0, 0.03)"}
               :onClick (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (println (str "dot click " dot-position)))}
        (inc index)]])))

(defn set-wrapping-2 [is-wrapping]
  (swap! app-state (fn [state]
                     (assoc state :wrapping is-wrapping))))

(defn on-mouse-down [e]
  (set-wrapping-2 true)
  (println "mouse down"))

(defn on-mouse-up [e]
  (set-wrapping-2 false)
  (println "mouse up"))

(defn step-tracking-dot [{:keys [x y a] :as tracking-dot}]
  (println (str "lehhhhhhh" tracking-dot))
  {:x {:target (+ (:target x) 1)
       :force (+ (:force x) 1)
       :current (+ (:current x) 1)}
   :y y
   :a a}
  )

(defn assign-targets [{:keys [target-positions tracking-dots] :as state}]
  (map-indexed
    (fn [index tracking-dot]
      (let [{:keys [x y a]} tracking-dot]
        (println (get target-positions 1))
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
                         :background-color "#ececec"}
           :onClick     (fn [e]
                          (swap! app-state (fn [{:keys [tracking-dots] :as state}]
                                             ;(println tracking-dots)
                                             (-> state
                                                 ;(assoc :tracking-dots (map step-tracking-dot tracking-dots))
                                                 ;(assoc :tracking-dots (map step-tracking-dot tracking-dots))
                                                 (assoc :tracking-dots (assign-targets state))
                                                 )
                                             ;state
                                             ))
                          (println (str "tdots: " (:tracking-dots @app-state)))
                          )
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


(defn get-jitter-amount []
  (* (- (rand) (rand)) 0.01))

(defn jitter-position [[x y a :as position]]
  (if (nil? position)
    nil
    [(+ x (get-jitter-amount)) (+ y (get-jitter-amount)) (+ a (get-jitter-amount))]
    ))

(defn move-num [n]
  ; diff target - current
  ; spring, friction
  ;
  (+ n 0.01)
  )

(defn move-position [[x y a :as position]]
  (if (nil? position)
    nil
    [(move-num x) (move-num y) (move-num a)]
    ))

;(defn step-tracking-dot [td]
;  (println "hello")
;  )

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
             (map step-tracking-dot tracking-dots)
             (-> state
                 (assoc :step-index step-index-for-swap)
                 (assoc :position-index position-index)
                 (assoc :target-positions (into [] (map (fn [dot] (get dot position-index)) key-positions)))
                 (assoc :current-positions (map (fn [dot] (get dot position-index)) key-positions))
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

(defn step-world [app-state]
  ;(println "stepping world")
  (swap! app-state (fn [{:keys [blorg target-positions current-positions] :as state}]
                     (-> state
                         (assoc :current-positions (map jitter-position target-positions))
                         ;(assoc :current-positions (map jitter-position current-positions))
                         (assoc :blorg (inc blorg))
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

(defonce init
         (do
           (println "kickoff!")
           (add-listeners)
           (time-loop)))

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