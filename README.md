# cppn

A Clojure implementation of Ken Stanley's [CPPNs: Compositional
Pattern Producing
Networks](https://en.wikipedia.org/wiki/Compositional_pattern-producing_network).

## Minimum Viable Snippet

``` clojure
(ns foo
  (:require [org.nfrac.cppn :as cppn]
            [org.nfrac.cppn.compile :refer [build-cppn-fn xy->d]]
            [clojure.test.check.random :as random]))

(def x
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
```

To evaluate the CPPN we first compile it into a function.

``` clojure
(def xf (build-cppn-fn x))
```

That function will be:

``` clojure
(fn [bias d x y]
 (let
  [i0 (org.nfrac.cppn.compile/gaussian (+ (* 1.0 d) (* 1.0 y)))
   v (max -1.0 (min 1.0 (* 1.0 i0)))
   s (max -1.0 (min 1.0 (+ (* 0.5 i0) (* -1.0 v))))
   h (max -1.0 (min 1.0 (* 1.0 i0)))]
  {:bias bias, :d d, :h h, :i0 i0, :s s, :v v, :x x, :y y}))
```

Note that the input arguments are in alphabetical order.

``` clojure
(doseq [y [-0.5 0 0.5]
        :let [x 0.5
              d (xy->d x y)]]
  (println (:h (xf 1.0 d x y))))

; 1.0
; 0.4578333617716143
; 0.0019304541362277093
```

### Weights

``` clojure
(cppn/cppn-weights x)
; [1.0 1.0 1.0 0.5 -1.0 1.0]
(cppn/set-cppn-weights x [0.1 0.2 0.3 0.4 0.5 0.6])
; {:inputs #{:y :d :x :bias}, :outputs #{:v :s :h}, :finals #{:s :h}, :nodes {:i0 :gaussian}, :edges {:i0 {:d 0.2, :y 0.3}, :v {:i0 0.6}, :h {:i0 0.1}, :s {:i0 0.4, :v 0.5}}}
```


### Mutation

``` clojure
(def rng (random/make-random 42))
(cppn/mutate-with-perturbation x rng cppn/parameter-defaults)
; {:inputs #{:y :d :x :bias}, :outputs #{:v :s :h}, :finals #{:s :h}, :nodes {:i0 :gaussian}, :edges {:i0 {:d 0.7561835970724577, :y 0.8705349405016322}, :v {:i0 1.206827781331554}, :h {:i0 1.1366109959829727}, :s {:i0 1.1722153513510696, :v -0.9750796909590522}}}
```

### Mutation parameters

* `:add-node-prob` - probability of adding a new internal node to the CPPN, with type one of `cppn/auto-node-types`: `#{:linear :gaussian :sigmoid :sine}`.
* `:add-conn-prob` - probability of adding a new input connection in the CPPN.
* `:rewire-conn-prob` - probability of rewiring a connection (remove + add).
* `:weight-perturbation` - a way to scale the mutation of weights, between 0.0 (no change) to 1.0 (random new weights).



## License

Copyright © 2018 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
