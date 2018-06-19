(ns org.nfrac.cppn
  (:require [org.nfrac.cppn.util :as util]
            [org.nfrac.cppn.strata :as strata]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check.random :as random]))

(s/def ::probability
  (s/double-in :min 0.0 :max 1.0 :NaN? false))

(s/def ::weight-perturbation
  (-> (s/double-in :min 0.0 :max 50.0 :NaN? false)
      (s/with-gen #(s/gen (s/double-in :min 0.0 :max 1.5 :NaN? false)))))

(def parameter-defaults
  {:add-node-prob 0.1
   :add-conn-prob 0.2
   :rewire-conn-prob 0.2
   :weight-perturbation 0.5})

;; network topology is defined by dependencies between nodes.
;; so all non-input nodes must have at least one input edge.
;; and cycles are not allowed.
(def example-cppn
  {:inputs #{:bias :x :y :d}
   :outputs #{:h :s :v}
   :finals #{:h :s}
   :nodes {:i0 :gaussian}
   :edges {:i0 {:d 1.0
                :y 1.0}
           :v {:i0 1.0}
           :h {:i0 1.0}
           :s {:i0 0.5
               :v -1.0}}})

(def all-node-types
  #{:linear :gaussian :sigmoid :sine :sawtooth})

(def auto-node-types
  #{:linear :gaussian :sigmoid :sine})

(s/def ::node-id keyword?)
(s/def ::inputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::outputs (s/coll-of ::node-id, :min-count 1, :kind set?))
(s/def ::nodes (s/map-of ::node-id all-node-types, :min-count 1))
(s/def ::weight (s/double-in :min -100 :max 100 :NaN? false))
(s/def ::node-edges (s/map-of ::node-id ::weight))
(s/def ::edges (s/map-of ::node-id ::node-edges, :min-count 1))

(declare cppn-graph)
(declare cppn-graph-no-finals)

(s/def ::cppn
  (->
   (s/and
    (s/keys :req-un [::inputs
                     ::outputs
                     ::nodes
                     ::edges])
    #(not-any? (::outputs %) (::inputs %))
    #(not-any? (::inputs %) (::outputs %))
    #(alg/dag? (cppn-graph %))
    #(alg/dag? (cppn-graph-no-finals %)))
   (s/with-gen #(gen/return example-cppn))))

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (into (or (empty m) {})
        (map (fn [[k v]] [k (f v)]))
        m))

(defn finals
  [cppn]
  (or (:finals cppn) (:outputs cppn)))

(defn cppn-graph
  [cppn]
  (let [edges (for [[target ins] (:edges cppn)
                    [from w] ins]
                [from target])]
    (apply graph/digraph edges)))

(defn cppn-graph-no-finals
  [cppn]
  (let [finals (finals cppn)
        edges (for [[target ins] (:edges cppn)
                    [from w] ins
                    :when (not (finals target))]
                [from target])]
    (apply graph/digraph edges)))

(defn cppn-strata
  "A list of sets. The first set contains only the inputs, the last only the
  final outputs."
  [cppn]
  (concat (strata/strata (cppn-graph-no-finals cppn))
          [(finals cppn)]))

(defn downstream
  "Returns the set of all downstream nodes including self."
  [cppn node-id]
  (let [g (cppn-graph cppn)]
    (loop [done #{}
           todo (list node-id)]
      (if-let [item (first todo)]
        (if (done item)
          (recur done (rest todo))
          (recur (conj done item)
                 (into todo (graph/successors g item))))
        done))))

(defn edge-list
  [cppn]
  (sort
   (for [[to m] (:edges cppn)
         from (keys m)]
    [to from])))

(defn cppn-weights
  [cppn]
  (mapv #(get-in (:edges cppn) %) (edge-list cppn)))

(defn set-cppn-weights
  [cppn ws]
  (reduce (fn [cppn [[to from] w]]
            (assoc-in cppn [:edges to from] w))
          cppn
          (map vector (edge-list cppn) ws)))

(s/fdef set-cppn-weights
        :args (s/cat :cppn ::cppn
                     :ws (s/coll-of ::weight))
        :ret ::cppn)

(defn rand-skew
  [rng max power]
  (-> (util/rand rng (Math/pow max (/ 1 power)))
      (Math/pow power)))

(defn rand-sign [rng] (if (pos? (util/rand-int rng 2)) 1 -1))

(defn rand-init-weight [rng]
  (let [[r1 r2] (random/split rng)]
    (* (rand-skew r1 3 2) (rand-sign r2))))

(defn gen-node-id
  [rng]
  (->> (random/rand-long rng)
       (format "%h")
       (keyword)))

(defn mutate-add-node-before
  [cppn before rng]
  (let [[r1 r2 r3 r4 r5] (random/split-n rng 5)
        type (util/rand-nth r1 (seq auto-node-types))
        id (gen-node-id r5)
        _ (assert (not (contains? (:edges cppn) id)) "gen-node-id collision!")
        [from1 w1] (util/rand-nth r2 (seq (get-in cppn [:edges before])))
        w2 (rand-init-weight r3)
        w3 (rand-init-weight r4)]
    (-> cppn
        (update :nodes assoc id type)
        (update :edges assoc id {from1 w2})
        ;(update-in [:edges before] dissoc from1)
        (update-in [:edges before] assoc id w3))))

(s/fdef mutate-add-node-before
        :args (s/and
               (s/cat :cppn ::cppn
                      :before ::node-id
                      :rng ::util/rng)
               #(contains? (:edges (:cppn %)) (:before %)))
        :ret ::cppn)

(defn mutate-add-node
  [cppn rng]
  (let [[rng rng*] (random/split rng)
        before (util/rand-nth rng* (keys (:edges cppn)))]
    (mutate-add-node-before cppn before rng)))

(defn mutate-append-node
  [cppn rng]
  (let [[rng rng*] (random/split rng)
        before (util/rand-nth rng* (seq (:outputs cppn)))]
    (mutate-add-node-before cppn before rng)))

(defn mutate-add-conn-to
  [cppn to-node rng]
  (let [to-edges (get (:edges cppn) to-node)
        candidates (remove (set (concat (keys to-edges)
                                        (downstream cppn to-node)
                                        (finals cppn)))
                           (concat (keys (:nodes cppn))
                                   (:inputs cppn)
                                   (:outputs cppn))) ;; outputs ok if not final
        [r1 r2] (random/split-n rng 2)
        w (util/rand r1 -2.0 2.0)]
    (if (seq candidates)
      (-> cppn
          (assoc-in [:edges to-node (util/rand-nth r2 (seq candidates))] w))
      cppn)))

(defn mutate-add-conn
  [cppn rng]
  (let [[r1 r2] (random/split-n rng 2)
        to (util/rand-nth r1 (keys (:edges cppn)))]
    (mutate-add-conn-to cppn to r2)))

(s/fdef mutate-add-conn
        :args (s/cat :cppn ::cppn
                     :rng ::util/rng)
        :ret ::cppn)

(defn mutate-rewire-conn
  [cppn rng]
  (let [[r1 r2 r3] (random/split-n rng 3)
        [to-node to-edges] (util/rand-nth r1 (seq (:edges cppn)))
        [rm-from old-w] (util/rand-nth r2 (seq to-edges))
        cppn2 (mutate-add-conn-to cppn to-node r3)
        new-from (-> (apply dissoc (get-in cppn2 [:edges to-node])
                            (keys to-edges))
                     keys first)]
    (if new-from
      (-> cppn2
          (update-in [:edges to-node] dissoc rm-from)
          (assoc-in [:edges to-node new-from] old-w))
      cppn)))

(s/fdef mutate-rewire-conn
        :args (s/cat :cppn ::cppn
                     :rng ::util/rng)
        :ret ::cppn)

(defn delete-node
  [cppn node]
  (let [above-ed (get-in cppn [:edges node])
        above (keys above-ed)
        below (-> (cppn-graph cppn)
                  (graph/successors node))]
    (->
     (reduce (fn [m below-node]
               (update-in m [:edges below-node]
                          (fn [below-ed]
                            (if (== 1 (count below-ed))
                              ;; node would be orphaned
                              (select-keys above-ed [(rand-nth above)])
                              (dissoc below-ed node)))))
             cppn
             below)
     (update :edges dissoc node)
     (update :nodes dissoc node))))

(defn link-nodes
  "Attempt to link node a -> b,
   but if that would be cyclic, link b -> a instead."
  [cppn node-a node-b]
  (if (or (contains? (finals cppn) node-a)
          (contains? (:inputs cppn) node-b)
          (contains? (downstream cppn node-b) node-a))
    (assoc-in cppn [:edges node-a node-b] 1.0)
    (assoc-in cppn [:edges node-b node-a] 1.0)))

(s/fdef link-nodes
        :args (s/cat :cppn ::cppn
                     :node-a ::node-id
                     :node-b ::node-id)
        :ret ::cppn)

(defn remove-edge
  [cppn from to]
  (update-in cppn [:edges to]
             (fn [m]
               (let [m (dissoc m from)]
                 ;; ensure all nodes have at least one input
                 (if (empty? m)
                   (assoc m (rand-nth (seq (:inputs cppn))) 1.0)
                   m)))))

(s/fdef remove-edge
        :args (s/cat :cppn ::cppn
                     :from ::node-id
                     :to ::node-id)
        :ret ::cppn)

(defn interp
  [from to z]
  (+ from (* z (- to from))))

(defn rand-weight
  [from-w perturbation rng]
  (let [global-w (rand-init-weight rng)
        locally (+ from-w (* perturbation 0.5 global-w))
        globally (interp from-w global-w (* perturbation perturbation))]
    (+ (* perturbation globally)
       (* (- 1.0 perturbation) locally))))

(defn randomise-weights
  [cppn perturbation rng]
  (let [ws (cppn-weights cppn)
        new-ws (mapv (fn [w r]
                       (rand-weight w perturbation r))
                     ws
                     (random/split-n rng (count ws)))]
    (set-cppn-weights cppn new-ws)))

(s/fdef randomise-weights
        :args (s/cat :cppn ::cppn
                     :perturbation ::weight-perturbation
                     :rng ::util/rng)
        :ret ::cppn)

(defn mutate-structure
  [cppn rng parameters]
  (let [{:keys [add-node-prob
                add-conn-prob
                rewire-conn-prob]} parameters
        [r1 r2 r3 r4 r5 r6] (random/split-n rng 6)]
    (cond-> cppn
      (< (random/rand-double r1) add-node-prob)
      (mutate-add-node r2)
      (< (random/rand-double r3) add-conn-prob)
      (mutate-add-conn r4)
      (< (random/rand-double r5) rewire-conn-prob)
      (mutate-rewire-conn r6))))

(defn mutate-with-perturbation
  [cppn rng parameters]
  (let [parameters (merge parameter-defaults parameters)
        {:keys [weight-perturbation]} parameters
        [rng rng*] (random/split rng)]
    (-> (mutate-structure cppn rng* parameters)
        (randomise-weights weight-perturbation rng))))
