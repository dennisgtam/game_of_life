(ns game-of-life.core
  (:require [game-of-life.patterns :as patterns]))

(declare print-world evolve generate-positions-values-array mark-point-for-life-or-death neighborhood-values return-neighborhood return-mod-neighborhood
          create-random-world world-coordinates neighbors clear-scr)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn print-and-format-world
  [world]
  (print (reduce (fn [final-string row]
            (let [row-string (clojure.string/replace (clojure.string/join "" row) #"0|1" {"0" "   " "1" " ▇ "})]
            (str final-string "\n" row-string))) "" world)))

(defn print-world
  [world]
  (for [row world]
        (println row)))

(defn evolve
  [world]
  (let [positions (world-coordinates world)
        position-values (generate-positions-values-array world positions)]
        (reduce (fn [world position-value]
                  (let [[position value] position-value]
                  (assoc-in world position value)))
                  world position-values)
        ))

(defn watch-evolve
  [world]
  (loop [n-world world]
        (print-and-format-world n-world)
        (clear-scr)
        ; (Thread/sleep 100)
        (recur (evolve n-world))))

(defn auto-watch-evolve
  [world]
  (loop [old-world world]
    (let [next-world (evolve old-world)
          next-next-world (evolve next-world)]
          (print-and-format-world next-world)
          (clear-scr)
          (Thread/sleep 75)
          (if (or (= old-world next-world) (= old-world next-next-world))
            (auto-watch-evolve world)
            (recur next-world)
          )
    )
  )
)

(defn auto-watch-evolve-2
  [world previous-world]
  (loop [current-world world
         previous-world previous-world]
    (let [next-world (evolve current-world)]
          (print-and-format-world next-world)
          (clear-scr)
          (if (or (= current-world next-world) (= previous-world next-world))
            (auto-watch-evolve world world)
            (recur next-world world)
          )
    )
  )
)


(defn generate-positions-values-array
  "return an array containing the coordinates and life/death value for each position in the given positions in the world"
  [world positions]
  (mapv (fn [position]
    [position (mark-point-for-life-or-death world position)]) positions))

(defn mark-point-for-life-or-death
  "determine if the position is to live or die"
  [world position]
  (let [neighborhood (return-mod-neighborhood world position)
        values (neighborhood-values world neighborhood)
        live-neighbors (count (filter #(= 1 %) values))
        point-value (get-in world position)]
        (cond
          (and (= point-value 1) (< live-neighbors 2)) 0
          (and (= point-value 1) (or (= live-neighbors 2) (= live-neighbors 3))) 1
          (and (= point-value 1) (> live-neighbors 3)) 0
          (and (= point-value 0) (= live-neighbors 3)) 1
          :else 0
        )
  ))

(defn neighborhood-values
  "returns values of each element in the neighborhood"
  [world neighborhood]
  (map (fn [neighbor]
          (let [[row column] neighbor]
          (get-in world [row column]))) neighborhood))

(defn return-neighborhood
  "returns coordinates of up to 8 neighbors not wrapping to other side of the world"
  [world position]
  (let [max-row (count world)
        max-col (count (nth world 0))
        [row col] position
        naive-neighbors (mapv (fn [neighbor-coor]
                                (let [[n-row n-col] neighbor-coor]
                              [(+ row n-row) (+ col n-col)])) neighbors)]
    (filter (fn [neighbor]
            (let [[n-row n-col] neighbor]
            ; refactor with macro?
            (and (>= n-row 0) (>= n-col 0) (<= n-row max-row) (<= n-col max-col))))
            naive-neighbors)))

(defn return-mod-neighborhood
  "returns coordinates of 8 neighbors including those on other side of the world"
  [world position]
  (let [max-row (count world)
        max-col (count (nth world 0))
        [row col] position]
        (mapv (fn [neighbor-coor]
                (let [[n-row n-col] neighbor-coor]
                  [(mod (+ row n-row) max-row) (mod (+ col n-col) max-col)]
                  )) neighbors)
        ))

(defn create-empty-world
  "returns a 2D array full of zeros"
  [num-rows num-columns]
  (let [row (vec (repeat num-columns 0))]
    (vec (repeat num-rows row))
  ))

(defn create-random-world
  [num-rows num-columns num-lives]
  (let [world (create-empty-world num-rows num-columns)
        coordinates (world-coordinates world)
        lives (take num-lives (shuffle coordinates))]
        (reduce (fn [world live]
          (assoc-in world live 1)
          ) world lives)))

(defn world-coordinates
  "return an array of 2 unit coordinates"
  [world]
  (let [rows (take (count world) (range))
        cols (take (count (nth world 0)) (range))]
        (for [x rows
              y cols]
              [x y])))

(def neighbors
  [[-1 -1] [-1 0] [-1 1]
   [0 -1] [0 1]
  [1 -1] [1 0] [1 1]
  ])

  (defn clear-scr []
    (let [esc (char 27)]
      (print (str esc "[2J"))
      (print (str esc "[;H"))))
