(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

;; Board Creation

(defn- tri*
  "Generates a lazy sequence of triangular numbers.
  (def tri (tri*))
  (take 5 tri)
  => (1 3 6 10 15)"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def ^:private tri (tri*))

(defn- triangular?
  "Determines whether a number is triangular.
  e.g. 1, 3, 6, 10, 15, etc."
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn- row-tri
  "Returns the last triangular number at the end of row n."
  [n]
  (last (take n tri)))

(defn- row-num
  "Returns the row number the position belongs to.
   pos 1 in row 1, pos 2 and 3 in row 2, etc."
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn- connect
  "Form a mutual connection between two positions."
  [board max-pos pos neighbour destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbour))
            board
            [[pos destination] [destination pos]])
    board))

(defn- connect-right
  [board max-pos pos]
  (let [neighbour (inc pos)
        destination (inc neighbour)]
    (if-not (or (triangular? neighbour) (triangular? pos))
      (connect board max-pos pos neighbour destination)
      board)))

(defn- connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn- connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn- add-pos
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board create-connection-fn]
              (create-connection-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Given rows, creates a new board by adding a peg to each position
  and defining all connections between pegs."
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;;; Peg Moving

(defn- pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn- remove-peg
  "Take a peg at the given position out of the board."
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn- place-peg
  "Put a peg in the given position of the board."
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn- move-peg
  "Take peg out of p1 and put it in p2."
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn- valid-moves
  "Returns a list of all the valid moves for pos, where the
  key is the destination and the value is the jumped pos."
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn- valid-move?
  "Return the jumped position if the move from p1 to p2 is valid,
  returns nil otherwise."
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move p1 to p2, removing jumped peg."
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the peg positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))
