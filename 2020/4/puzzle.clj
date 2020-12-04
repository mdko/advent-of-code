; Run like so:
; $ clojure puzzle.clj

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn is-valid-part-1? [entry]
    (let [fields-values (str/split entry #"\s")
          fields (set
                    (for
                        [fv fields-values]
                        (let [l (str/split fv #":")]
                            (nth l 0))))
          required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}]
          (set/subset? required fields)))

(defn in-range [n low high]
    (try
        (let [n (Integer. n)]
            (and (>= n low) (<= n high)))
        (catch ClassCastException e false)))

(defn valid-birth-year? [by]
    (in-range by 1920 2002))

(defn valid-issue-year? [iy]
    (in-range iy 2010 2020))

(defn valid-expire-year? [ey]
    (in-range ey 2020 2030))

(defn valid-height? [h]
    (let [parts (re-matches #"(\d+)(cm|in)" h)]
        (if parts
            (try
                (let [height (Integer. (nth parts 1))
                      units (nth parts 2)]
                    (cond
                        (= "cm" units) (in-range height 150 193)
                        (= "in" units) (in-range height 59 76)))
                (catch ClassCastException e false)))))

(defn valid-hair-color? [h]
    (boolean (re-matches #"#[0-9a-f]{6}" h)))

(defn valid-eye-color? [e]
    (boolean (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} e)))

(defn valid-pid? [pid]
    (boolean (re-matches #"[0-9]{9}" pid)))

(defn is-valid-part-2? [entry]
    (let [fields-values (str/split entry #"\s")
          fvmap (into {}
                  (for [fv fields-values]
                    (let [l (str/split fv #":")]
                        (assoc nil (keyword (nth l 0)) (nth l 1)))))
          validation-funcs {:byr valid-birth-year?
                            :iyr valid-issue-year?
                            :eyr valid-expire-year?
                            :hgt valid-height?
                            :hcl valid-hair-color?
                            :ecl valid-eye-color?
                            :pid valid-pid?}]
          (and
            (set/subset? (set (keys validation-funcs)) (set (keys fvmap)))
            (every? true?
                (for [kv validation-funcs]
                    (let [k (key kv)
                          f (val kv)]
                        (f (get fvmap k))))))))

(let [file (slurp "input")
      entries (str/split file #"\n\n")]
    (do
        (-> (filter is-valid-part-1? entries) count println)
        (-> (filter is-valid-part-2? entries) count println)))