(ns org.nfrac.cppn.util
  (:require [clojure.test.check.random :as random]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen])
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

(s/def ::rng (-> #(satisfies? random/IRandom %)
                 (s/with-gen #(gen/fmap random/make-random (gen/int)))))

(defn rand
  (^double [rng ^double upper]
   (-> (random/rand-double rng)
       (* upper)))
  (^double [rng ^double lower ^double upper]
   {:pre [(<= lower upper)]}
   (-> (random/rand-double rng)
       (* (- upper lower))
       (+ lower))))

(defn rand-int
  "Uniform integer between lower (inclusive) and upper (exclusive)."
  (^long [rng ^long upper]
   (-> (random/rand-double rng)
       (* upper)
       (Math/floor)
       (long)))
  (^long [rng ^long lower ^long upper]
   (-> (random/rand-double rng)
       (* (- upper lower))
       (+ lower)
       (Math/floor)
       (long))))

(defn rand-nth
  [rng xs]
  (nth xs (rand-int rng (count xs))))

;; copied from
;; https://github.com/clojure/data.generators/blob/bf2eb5288fb59045041aec01628a7f53104d84ca/src/main/clojure/clojure/data/generators.clj
;; adapted to splittable RNG

(defn ^:private fisher-yates
  "http://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm"
  [rng coll]
  (let [as (object-array coll)]
    (loop [i (dec (count as))
           r rng]
      (if (<= 1 i)
        (let [[r1 r2] (random/split r)
              j (rand-int r1 (inc i))
              t (aget as i)]
          (aset as i (aget as j))
          (aset as j t)
          (recur (dec i) r2))
        (into (empty coll) (seq as))))))

(defn shuffle
  [rng coll]
  (fisher-yates rng coll))
