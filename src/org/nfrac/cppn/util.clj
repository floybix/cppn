(ns org.nfrac.cppn.util
  (:require [clojure.test.check.random :as random]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

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
