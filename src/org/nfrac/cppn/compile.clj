(ns org.nfrac.cppn.compile
  (:require [org.nfrac.cppn :as cppn]))

(defn sine
  ^double [^double x]
  (Math/sin (* x 3.1415 2)))

(defn gaussian
  ^double [^double x]
  (-> (* x 2.5)
      (Math/pow 2.0)
      (* -1.0)
      (Math/exp)))
      ;(* 2.0)
      ;(- 1.0)))

(defn sigmoid
  ^double [^double x]
  (-> (/ 1.0
       (-> (* x -4.9) (Math/exp) (+ 1.0)))))
      ;(* 2.0)
      ;(- 1.0)))

(defn sawtooth
  ^double [^double x]
  (-> (mod x 1.0)
      (* 2.0)
      (- 1.0)))

(defn abs ^double [^double x] (if (neg? x) (- x) x))

(defn xy->d
  ^double [^double x ^double y]
  (-> (Math/sqrt (+ (* x x) (* y y)))
      (/ (Math/sqrt 2.0))))
      ;(* 2.0)
      ;(- 1.0)))

(defn build-cppn-fn-expr
  [cppn]
  (let [sym (comp symbol name)
        strata (cppn/cppn-strata cppn)
        zerod? (:zerod cppn #{})
        sorted-nids (apply concat (rest strata))
        lets (->>
              sorted-nids
              (map (fn [nid]
                     (let [node-type (get-in cppn [:nodes nid] :linear)
                           edges (get-in cppn [:edges nid])
                           adds (->> edges
                                     (map (fn [[k w]]
                                            (list '* w (sym k)))))
                           sum (if (> (count edges) 1)
                                 (apply list '+ adds)
                                 (first adds))
                           sumw  (->> (vals edges)
                                      (map abs)
                                      (reduce +))
                           expr (cond
                                  (zerod? nid)
                                  0.0
                                  (= :linear node-type)
                                  ;(list '/ sum sumw)
                                  `(max -1.0 (min 1.0 ~sum))
                                  :else
                                  (list (symbol "org.nfrac.cppn.compile"
                                                (name node-type))
                                        sum))]
                       [(sym nid) expr])))
              (apply concat)
              (vec))
        out-exprs (into {} (map (fn [nid]
                                  [nid (sym nid)])
                                (sort (concat (:inputs cppn) (keys (:nodes cppn)) (:outputs cppn)))))
        prims? (<= (count (:inputs cppn)) 4)
        in-syms (mapv (fn [k]
                        (cond-> (sym k)
                          prims? (vary-meta assoc :tag 'double)))
                      (sort (:inputs cppn)))] ;(disj (:inputs cppn) :d)))]
        ;assigns (if (contains? (:inputs cppn) :d)
        ;          (conj assigns '[d (xy->d x y)])
        ;          assigns)]
    `(fn ~in-syms
       (let ~lets
         ~out-exprs))))

(defn build-cppn-fn
  [cppn]
  (eval (build-cppn-fn-expr cppn)))
