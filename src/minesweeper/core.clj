(ns minesweeper.core
  (:require [clojure.string :as str])
  (:gen-class))

;; ------------------------
;; Helpers

(defn valid-position
  [size position]
  (apply = true (map #(<= 0 % (dec size)) position)))

(defn cartesian-product
  [l1 l2]
  (for [i l1
        j l2]
    [i j]))

(defn neighbours
  [size position]
  (->> (for [p position]
         (for [f [dec identity inc]]
           (f p)))
       (apply cartesian-product)
       (filter (partial valid-position size))
       (remove #{position})))

(defn count-neighboring-mines
  [size position mines]
  (->> (neighbours size position)
       (filter (partial get-in mines))
       count))

;; ------------------------
;; Updating board state

(defn unveil
  "Unveils position. This position must not be mined. Returns new"
  [{:keys [size flags mines data remaining] :as state} position]
  {:pre [(not (get-in mines position))]}
  (if (or (get-in data position)
          ;; Don't unveil flagged locations
          (get-in flags position))
    state
    (let [neighbouring-mines (count-neighboring-mines size position mines)
          new-data (assoc-in data position neighbouring-mines)
          next-state (-> state
                         (assoc :data new-data)
                         (update :remaining dec))]
      (if (zero? neighbouring-mines)
        (reduce unveil next-state (neighbours size position))
        next-state))))

(defn toggle-flag
  [{:keys [size data flags] :as state} [r c :as position]]
  {:pre [(valid-position size position)]}
  (if (get-in data position)
    ;; Don't toggle unveiled locations
    state
    (let [present (get-in flags position)
          new-sub-flags (let [m (get flags r)]
                          (if present
                            (dissoc m c)
                            (assoc m c true)))]
      (-> state
          (assoc :flags (assoc flags r new-sub-flags))
          (update :remaining (if present inc dec))))))

;; ------------------------
;; Rendering

(defn- render-position
  [{:keys [size flags data]} position]
  {:pre [(valid-position size position)]}
  (let [status (get-in data position)]
    (cond
      (number? status) (str status)
      (get-in flags position) "F"
      :else "-")))

(defn- render-position-reveal
  [{:keys [mines flags size data]} position]
  {:pre [(valid-position size position)]}
  (let [status (get-in data position)
        mined? (get-in mines position)
        flagged? (get-in flags position)]
    (cond
      (number? status) (str status)
      (and flagged? mined?) "F"
      (and (not flagged?) mined?) "*"
      (and flagged? (not mined?)) "!"
      :else "-")))

(defn- render-row
  ([state index]
   (render-row false state index))
  ([reveal? {:keys [size] :as state} index]
   (let [f (if reveal?
             #(render-position-reveal state [index %])
             #(render-position state [index %]))]
     (str/join " "(map f (range size))))))

(defn render
  [{:keys [size] :as state} & [reveal?]]
  (str/join "\n" (map (partial render-row reveal? state) (range size))))

;; ------------------------
;; Gameplay

(defn get-input
  "Waits for user to enter text and hit enter,
     then cleans the input by trimming and lowercasing it"
  [default]
  (let [input (clojure.string/trim (read-line))]
    (if (empty? input)
      default
      (clojure.string/lower-case input))))

(defn get-input-and-validate
  "Gets input, then validates it with the validation function
   Will ask for input again if validation fails.
   Returns nil only when user wants to quit"
  ([default validate]
   (get-input-and-validate default validate "I don't understand you, try again"))
  ([default validate message]
   (let [input (get-input default)]
     (cond
       (= input "q") nil
       (validate input) input
       :else (do
               (println message)
               (recur default validate message))))))

(defn randomize-mines
  [size]
  ;; Number of mines is capped at (x-1)(y-1)
  ;; According to http://www.minesweeper.info/custom.php
  (let [max-mines (int (* 0.2 (dec size) (dec size)))
        rows (take max-mines (repeatedly #(int (rand size))))
        cols (take max-mines (repeatedly #(int (rand size))))]
    (reduce #(assoc-in %1 %2 true)
            (sorted-map)
            (map vector rows cols))))

(defn- parse-position
  [input]
  (if-let [result (re-find #"(\d+)\s+(\d+)" input)]
    (let [[_ r c] result]
      [(Integer. r) (Integer. c)])))

(defn- parse-toggle
  [input]
  (let [first-letter (re-find #"[a-z]" input)]
    (if (or (nil? first-letter) (= "f" first-letter))
      (= "f" first-letter))))

(defn parse-move
  [input]
  (if-not (or (nil? input) (str/blank? input))
    (let [position (parse-position input)
          toggle (parse-toggle input)]
      (if-not (or (nil? position) (nil? toggle))
        [toggle position]))))

(declare prompt-size)

(defn game-over
  [state win?]
  (println (if win?
             "You win!"
             "Game over!"))
  (println (render state true))
  (println "Play again? [Y/n]")
  (if-let [input (get-input-and-validate "y"
                                         #{"y" "n"}
                                         "Enter either y or n please!")]
    (if (= "y" input)
      (prompt-size))))

(defn handle-move
  [{:keys [data flags mines] :as state} [toggle? position]]
  (let [flagged? (get-in flags position)
        mined? (get-in mines position)]
    (cond
      (and flagged? (not toggle?)) state
      toggle? (toggle-flag state position)
      mined? (game-over state false)
      :else (unveil state position))))

(defn count-items
  "In a nested map, count all truthy values"
  [m]
  (apply + (map (comp count seq (partial filter second) second) m)))

(defn print-state
  [{:keys [mines flags] :as state}]
  (println "Here's your board: ")
  (println (render state))
  (println "Mines left:" (- (count-items mines) (count-items flags))))

(defn flags-match-mines
  [{:keys [mines flags]}]
  (= mines flags))

(defn prompt-move
  [{:keys [remaining] :as state}]
  (if state
    (if (and (zero? remaining) (flags-match-mines state))
      (game-over state true)
      (do
        (print-state state)
        (println "Move?")
        (if-let [input (parse-move
                        (get-input-and-validate nil parse-move
                                                "That is not a valid move"))]
          (recur (handle-move state input)))))))

(defn parse-board-size
  [input]
  (try
    (Integer. input)
    (catch NumberFormatException _)
    (catch NullPointerException _)))

(defn prompt-size
  []
  (println "How big will your minesweeper board be? [5]")
  (if-let [size (parse-board-size
                 (get-input-and-validate 5
                                         parse-board-size
                                         "Enter a valid size please."))]
    (let [mines (randomize-mines size)
          state {:size size
                 :data {}
                 :flags (sorted-map)
                 :mines mines
                 :remaining (* size size)}]
      (prompt-move state))))

(defn -main
  [& args]
  (println "Enter 'q' anytime to quit!")
  (println "How to enter a move:")
  (println "  1. 'f 0 0' to toggle flag at row 0 and col 0")
  (println "  2. '1 2' to unveil row 1 col 2")
  (prompt-size))
