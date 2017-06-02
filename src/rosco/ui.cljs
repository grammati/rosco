(ns rosco.ui
  (:require [reagent.core :as r]))

(enable-console-print!)

(defonce app-state (atom {:text "Hello world!"}))

(def test-data
  (r/atom
   [{:t 8} {:t 3} {:t 9} {:t 6} {:t 10} {:t 4} {:t 7} {:t 5}]))

(defn flame [data]
  [:svg {:width        400
         :height       400
         :stroke       "white"
         :stroke-width 1
         :fill         "black"
         :class        "Paired"}
   (let [d              @data
         values         (map :t d)
         running-totals (->> values (concat [0]) (reductions +))
         total          (last running-totals)
         pairs          (map vector values running-totals (shuffle (range (count values))))]
     (for [[v rt c] pairs]
       [:rect {:x             10
               :y             (* 400 (/ rt total))
               :width         20
               :height        (* 400 (/ v total))
               :class         (str "q" (mod c 9) "-9")
               :on-mouse-over #(println "foo")
               :style         {:cursor "hand"}}]))])

(defn hello-world []
  [:div
   [:h1 "My sweet cljs app"]
   [flame test-data]])

(r/render-component [hello-world]
                    (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
