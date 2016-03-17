(ns zeronine.core
  (:require [reagent.core :as r :refer [atom]]
            [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
    [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defonce app-state (atom {:text "Hello world!"
                          :steps ["zero" "one" "two" "three"]
                          :step-index 0
                          :running true}))

(defn fps-to-millis [fps]
  (/ 1000 fps))

(def time-loop-interval (fps-to-millis 30))

(defn hello-world []
  (let [{:keys [steps step-index]} @app-state]
    [:h1 (get steps step-index)]))

(r/render-component [hello-world]
                    (. js/document (getElementById "app")))


(defn add-to-step-index [to-add]
  (swap! app-state
         (fn [{:keys [steps step-index] :as state}]
           (let [last-step-index (- (count steps) 1)
                 new-step-index (+ step-index to-add)
                 step-index-for-swap (cond
                                       (< new-step-index 0) last-step-index
                                       (> new-step-index last-step-index) 0
                                       :else new-step-index)]
             (-> state
                 (assoc :step-index step-index-for-swap))))))


(defn on-click [e]
  (println (str "step index: " (:step-index @app-state))))

(defn toggle-running []
  (println (str "speeeeeeeeeexxxxxxxx" (:running @app-state)))
  (swap! app-state
         (fn [state]
           (-> state
               (assoc :running (not (:running state)))))))

(defn on-key-down [e]
  (let [keyCode (.-keyCode e)]
    (case keyCode
      ;32 (println "space")
      32 (toggle-running)
      ;37 (println "left")
      ;39 (println "right")
      37 (add-to-step-index -1)
      39 (add-to-step-index 1)
      (println (str keyCode " is not recognized key")))))

(defn add-listeners []
  (println "adding listeners")
  (set! (.-onkeydown js/document) on-key-down)
  (set! (.-onclick js/document) on-click))

(defn time-loop []
  (go
    (<! (timeout time-loop-interval))
    (when (:running @app-state)
      (add-to-step-index 1)
      (println "time tick!"))
    (time-loop)))

(defonce init
         (do
           (println "kickoffff!")
           (add-listeners)
           ;(render)
           (time-loop)))

(defn on-js-reload []
  ;;; (println "reload")
  ;;; optionally touch your app-state to force rerendering depending on
  ;;; your application
  ;;; (swap! app-state update-in [:__figwheel_counter] inc)
  (add-listeners)
)
