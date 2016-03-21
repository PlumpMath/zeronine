(ns zeronine.core
  (:require [reagent.core :as r :refer [atom]]
            [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
    [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(def spacing-x 100)
(def spacing-y 90)
(def size 100)
(def corner-radius 60)

(defn fps-to-millis [fps]
  (/ 1000 fps))

(def time-loop-interval (fps-to-millis 10))

(def looping true)

(defonce app-state (atom {:text "Hello world!"
                          ;:steps ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
                          :steps     [ "zero"  "one"   "two"    "three"   "four"   "five"     "six"     "seven"   "eight"   "nine"]
                          :positions [[[0 0 0] [0 0 1] [-1 0 1] [-1 -1 1] [-1 -1 1] [-1 -1 1] [-2 -2 1] [-2 -2 1] [-2 -2 1] [-2 -2 1]]
                                      [nil     [0 0 0] [1 0 1]  [1 -1 1]  [1 -1 1]  [1 -1 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]  [2 -2 1]]
                                      [nil     nil     [1 0 0]  [0 1 1]   [1 1 1]   [1 1 1]   [2 2 1]   [2 2 1]   [2 2 1]   [2 2 1]]
                                      [nil     nil     nil      [0 1 0]   [-1 1 1]  [-1 1 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]  [-2 2 1]]
                                      [nil     nil     nil      nil       [0 0 0]   [0 0 1]   [-1 0 1]  [-1 -1 1] [-1 -1 1] [-1 -1 1]]
                                      [nil     nil     nil      nil       nil       [0 0 0]   [1 0 1]   [1 -1 1]  [1 -1 1]  [1 -1 1]]
                                      [nil     nil     nil      nil       nil       nil       [1 0 0]   [0 1 1]   [1 1 1]   [1 1 1]]
                                      [nil     nil     nil      nil       nil       nil       nil       [0 1 0]   [-1 1 1]  [-1 1 1]]
                                      [nil     nil     nil      nil       nil       nil       nil       nil       [0 0 0]   [0 0 1]]]
                          :step-sequence [0 1 2 3 4 5 6 7 8 9]
                          ;:step-sequence [0 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2 1
                          ;                0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
                          ;                0 1 2 3 4 5 6 7 6 5 4 3 2 1
                          ;                0 1 2 3 4 5 6 5 4 3 2 1
                          ;                0 1 2 3 4 5 4 3 2 1
                          ;                0 1 2 3 4 3 2 1
                          ;                0 1 2 3 2 1
                          ;                0 1 2 1
                          ;                0 1
                          ;                0]
                          :step-index 0
                          :running true}))

(defn make-dot-divs [index dot-position]
  (let [[x y a] dot-position]
    (if dot-position
      [:div {:key   index
             :style {:position         "absolute"
                     :left             (* x spacing-x)
                     :top              (* y spacing-y)
                     :opacity          a
                     :color            "black"}
             :onClick (fn [e] (println dot-position))}
       [:div {:style {:position         "relative"
                      :top              (- (/ spacing-y 2))
                      :left             (- (/ spacing-x 2))
                      :width            size
                      :height           size
                      :background-color "black"
                      :display          "flex"
                      :justify-content  "center"
                      :alignItems       "center"
                      :border-radius    corner-radius}}
        (inc index)]])))

(defn hello-world []
  (let [{:keys [steps step-index step-sequence positions]} @app-state
        dots (map positions (fn [dot] (get dot step-index)))
        position-index (get step-sequence step-index)
        current-dot-positions (map (fn [dot] (get dot position-index)) positions)
        dot-divs (map-indexed make-dot-divs current-dot-positions)]
    [:div {:style {:width (.-innerWidth js/document)
                   :height (.-innerHeight js/document)
                   }}
     [:div {:style {:position "relative"
                    :height   0
                    :width    0
                    :left     (/ (.-innerWidth js/window) 2)
                    :top      (/ (.-innerHeight js/window) 2)}}
      dot-divs]]))

(r/render-component [hello-world]
                    (. js/document (getElementById "app")))

(defn move-step-index [amount-to-move]
  (swap! app-state
         (fn [{:keys [steps step-index step-sequence] :as state}]
           (let [last-step-index (- (count step-sequence) 1)
                 new-step-index (+ step-index amount-to-move)
                 step-index-for-swap (cond
                                       ;(< new-step-index 0) last-step-index
                                       ;(< new-step-index 0) 0
                                       (< new-step-index 0) (if looping last-step-index 0)
                                       ;(> new-step-index last-step-index) 0
                                       ;(> new-step-index last-step-index) last-step-index
                                       (> new-step-index last-step-index) (if looping 0 last-step-index)
                                       :else new-step-index)]
             (-> state
                 (assoc :step-index step-index-for-swap))))))

(defn toggle-running []
  (swap! app-state
         (fn [state]
           (-> state
               (assoc :running (not (:running state)))))))

(defn on-click [e]
  ;(println (str "step index: " (:step-index @app-state)))
  (let [{:keys [steps step-index step-sequence positions]} @app-state]
    (println "click")))

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
  (set! (.-onclick js/document) on-click))

(defn time-loop []
  (go
    (<! (timeout time-loop-interval))
    (when (:running @app-state)
      (move-step-index 1)
      (println "tick"))
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