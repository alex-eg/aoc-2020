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

(defn do-job []
  (->> "./src/advent_2020/2/input.txt"
       (read-file)
       (prepare-records)
       (filter is-valid)
       (count)))

(defn do-job-* []
  (->> "./src/advent_2020/2/input.txt"
       (read-file)
       (prepare-records)
       (filter is-valid-*)
       (count)))

(defn is-valid [[min max char password]]
  (let [count (count (filter (partial = char) password))]
    (and (>= count min)
         (<= count max))))

(defn is-valid-* [[min max char password]]
  (let [p1 (dec min)
        p2 (dec max)]
    (or (and (= (nth password p1) char)
             (not= (nth password p2) char))
        (and (not= (nth password p1) char)
             (= (nth password p2) char)))))

(defn prepare-records [records]
  (map (fn [[range char pw]]
         (let [min-max (clojure.string/split range #"-")]
           (vector
            (Integer/parseInt (nth min-max 0))
            (Integer/parseInt (nth min-max 1))
            (nth char 0)
            pw)))
       records))

(defn read-file [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))))

(ns task-3)

(defn do-job []
  (->> "./src/advent_2020/3/input.txt"
       (read-file)
       (prepare-array)
       (find-hits 3 1)))

(defn do-job-* []
  (let [data (->> "./src/advent_2020/3/input.txt"
                  (read-file)
                  (prepare-array))]
    (* (find-hits 1 1 data)
       (find-hits 3 1 data)
       (find-hits 5 1 data)
       (find-hits 7 1 data)
       (find-hits 1 2 data))))

(defn find-hits [dx dy data]
  (let [width (nth data 0)
        length (nth data 1)
        map (nth data 2)]
    (loop [hits 0
           cur-x 0
           cur-y 0]
      (if (>= cur-y length)
        hits
        (recur
         (+ hits (nth (nth map cur-y) cur-x))
         (if (>= (+ cur-x dx) width)
           (mod (+ cur-x dx) width)
           (+ cur-x dx))
         (+ cur-y dy))))))

(defn read-file [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)))

(defn parse-line [line]
  (map (fn [char] (if (= char \#) 1
                      0))
       line))

(defn prepare-array [lines]
  (let [width (count (nth lines 0))
        length (count lines)]
    [width length
     (map parse-line lines)]))

(ns task-4)

(defn do-job []
  (->> "./src/advent_2020/4/input.txt"
       (read-file)
       (parse-records)
       (filter validate)
       (count)))

(defn read-file [filename]
  (->> filename
       (slurp)))

(defn validate [record]
  (or (= 8 (count record))
      (and (= 7 (count record))
           (not (some (partial = 'cid) record)))))

(defn parse-records [whole]
  (->> (clojure.string/split whole #"\n\n")
       (map #(clojure.string/split % #"[\n ]"))
       (map (partial map #(subs % 0 3)))
       (map (partial map symbol))))

(defn do-job-* []
  (->> "./src/advent_2020/4/input.txt"
       (read-file)
       (parse-records-*)
       (filter validate-*)
       (count)))

(defn parse-records-* [whole]
  (->> (clojure.string/split whole #"\n\n")
       (map #(clojure.string/split % #"[\n ]"))
       (map (partial map #(clojure.string/split % #":")))
       (map (partial map
                     (fn [[sym-name value]]
                       (let [sym (symbol sym-name)]
                         (cond (or (= sym 'byr)
                                   (= sym 'iyr)
                                   (= sym 'eyr)) [sym (Integer/parseInt value)]
                               (= sym 'hgt) [sym (parse-hgt value)]
                               (= sym 'ecl) [sym (symbol value)]
                               :else [sym value])))))))

(defn parse-hgt [str]
  [(Integer/parseInt (nth (clojure.string/split str #"[ci]") 0))
   (if (clojure.string/ends-with? str "cm")
     'cm
     'in)])

(defn validate-* [record]
  (and
   (or (= 8 (count record))
       (and (= 7 (count record))
            (not (some (fn [[field _]] (= field 'cid)) record))))
   (validate-fields record)))

(defn validate-fields [record]
  (reduce #(and %1 %2) true
          (map (fn [[fld val]]
                 (cond (= fld 'byr)
                       (and (>= val 1920)
                            (<= val 2002))
                       (= fld 'iyr)
                       (and (>= val 2010)
                            (<= val 2020))
                       (= fld 'eyr)
                       (and (>= val 2020)
                            (<= val 2030))
                       (= fld 'hgt)
                       (let [[v unit] val]
                         (cond (= unit 'cm)
                               (and (>= v 150)
                                    (<= v 193))
                               (= unit 'in)
                               (and (>= v 59)
                                    (<= v 76))
                               :else false))
                       (= fld 'hcl)
                       (re-find #"#[a-f0-9]{6}$" val)
                       (= fld 'ecl)
                       (some (partial = val) '(amb blu brn gry grn hzl oth))
                       (= fld 'pid)
                       (re-find #"^[0-9]{9}$" val)
                       (= fld 'cid) true
                       :else false))
               record)))
