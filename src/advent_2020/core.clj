(ns advent-2020.core
  (:gen-class))

(defn -main [& args])

(defn task-1 []
  (let [nums (map #(Integer/parseInt %) (read-file "src/advent_2020/1/input"))]
    (println (do-job nums))
    (println (do-job-* nums))))

(ns task-1)

(defn do-job [nums]
  (some (fn [list]
          (some (fn [elem]
                  (when (= 2020 (+ elem (first list)))
                    (* elem (first list))))
                (rest list)))
        (maplist identity nums)))

(defn do-job-* [nums]
  (some (fn [list-1]
          (some (fn [list-2]
                  (some (fn [elem]
                          (when (= 2020 (+ elem (first list-1) (first list-2)))
                            (* elem (first list-1) (first list-2))))
                        (rest list-2)))
                (maplist identity list-1)))
        (maplist identity nums)))

(defn maplist [f & seqs]
  "Maps f to all sequences, then to their tails, until the end
   E.g: (maplist identity '(1 2 3))
   => ((1 2 3) (1 2) (3))"
  (lazy-seq
   (when (every? seq seqs)
     (cons (apply f seqs)
           (apply maplist (cons f (map next seqs)))))))

(defn read-file [filename]
  (with-open [f (clojure.java.io/reader filename)]
    (into [] (line-seq f))))

(ns task-2)
