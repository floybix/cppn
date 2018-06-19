(ns org.nfrac.cppn.strata
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [clojure.spec.alpha :as s]))

(defn- index-contains
  "Returns the (first) index into sets which contains item, or nil if none
  of the sets contain item."
  [sets item]
  (loop [sets (seq sets)
         i 0]
    (when-let [x (first sets)]
      (if (contains? x item)
        i
        (recur (rest sets) (inc i))))))

(s/fdef index-contains
        :args (s/cat :sets (s/every set?)
                     :item any?)
        :ret (s/nilable nat-int?))

(defn strata
  "Collects nodes into strata in topologically sorted order.
  The nodes at index 1 depend on index 0; those at 2 depend on 0 and
  1, and so on. Those within a a set have no mutual dependencies.
  Returns nil if the graph contains cycles."
  [g]
  (when-let [ts (alg/topsort g)]
    (loop [ts ts
           out []]
      (if-let [node (first ts)]
        (let [pres (graph/predecessors g node)
              is (map #(or (index-contains out %) -1) pres)
              strata-i (inc (apply max -1 is))]
          (recur (rest ts)
                 (update out strata-i #(conj (or % #{}) node))))
        out))))

(s/fdef strata
        :args (s/cat :g graph/directed?)
        :ret (s/every set? :kind vector?))
