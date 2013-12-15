(ns buffers.core
  (:require
   [cljs.core.async :refer (<! >! chan put! take! alts! timeout close! dropping-buffer sliding-buffer)]
   [domina :as dom]
   [domina.css :as css]
   [domina.events :as ev]
   [hiccups.runtime :as hiccupsrt])
  (:require-macros
   [cljs.core.async.macros :refer (go alt!)]
   [hiccups.core :as hiccups]))

(defn ^:export log [thing] (.log js/console (clj->js thing)))
(aset js/window "log" log)

(def countr (atom 0))
(def buffer-length 250)
(def ul (dom/by-id "list"))

(defn rand-hsl []
  (map Math/floor [(rand 360) (rand 100) (+ 25 (rand 50))]))

(def base-color (atom (rand-hsl)))

(defn make-base [hsl]
  (let [[h s l] hsl]
    (cond
     (< h 359) [(+ h 1) s l]
     (< s 100) [h (+ s 1) l]
     (< l 100) [h s (+ l 1)]
     :else (rand-hsl))))

(defn make-bg []
  (let [color @base-color
        [h s l] (make-base color)
        color (conj [h] (str s "%") (str l "%"))]
    (reset! base-color [h s l])
    (str "background-color: hsl(" (clojure.string/join "," color) ")")))

(defn make-li [id]
  (let [lid (str "li_" id)
        hsl (make-bg)]
    (dom/append! ul (hiccups/html [:li {:id lid :style hsl}]))))

(defn remove-li [id]
  (let [lid (str "li_" id)]
    (dom/detach! (dom/by-id lid))))

(defn load-squares [v]
  (let [c (chan buffer-length)]
    (doseq [i v]
      (put! c i)
      (swap! countr inc))
    c))

(defn render-squares [c]
  (go
   (while true
     (let [id (<! c)
           offset (- @countr id)
           floor (- @countr buffer-length)]
       (make-li id)
       (remove-li (- floor offset))
       (<! (timeout 10))))))

(ev/listen! (dom/by-id "button-pause") :click (fn [evt]
                                                (ev/prevent-default evt)
                                                (render-squares (load-squares (vec (range @countr (+ @countr buffer-length)))))))
