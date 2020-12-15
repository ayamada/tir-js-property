(ns tir.js.property-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [tir.js.property :as p :include-macros true]))

(deftest basic
  (let [t #js {"fooBar" 1
               "foo-buz" 2
               "WebGLVersion" 2
               "jsLikeCamelProperty3" 3
               "isEnableCache" true
               "subMenu" #js {"itemName" "exit"}}]
    (is (= 1 (p/get t :foo-bar))) ; Camelize keyword automatically
    (is (= 1 (p/get t :fooBar))) ; But, you can give camel case keyword
    (is (= 2 (p/get t "foo-buz"))) ; Not camelize string automatically
    (is (= 2 (p/get t "WebGLVersion"))) ; Use string for complex word
    (is (= 3 (p/get t :js-like-camel-property-3)))
    (is (= 3 (p/get t :js-like-camel-property3))) ; Same
    (is (= true (p/get t :enable-cache?))) ; `?` convert to `is` prefix
    (is (= "exit" (p/get t :sub-menu/item-name))) ; Suport until two depth
    (p/set! t :a-new-property-123 123)
    (is (= 123 (aget t "aNewProperty123")))
    (p/set! t :sub-menu/item-num 45) ; Can set two depth property
    (is (= 45 (aget (aget t "subMenu") "itemNum")))
    (is (= t (p/set! t :new-value 6))) ; `p/set!` return target object
    (is (= (js->clj (p/set! #js {} :depth/layer 1))
           {})) ; Cannot set child to nonexistence node directly
    (p/set! t :--private-cljs-prop 7) ; `:--` as a mark of private use
    (is (= 7 (aget t "-privateCljsProp")))))

(deftest util
  (is (= "isEnableCache" (p/kebab-string->camel-string "enable-cache?")))
  (is (= "isEnableCache" (p/kebab->camel "enable-cache?"))) ; Same
  (is (thrown? js/Error (p/kebab->camel :enable-cache?))) ; Must be string
  (is (= (js->clj (p/merge! (js-obj)
                            {:foo-bar 1 :visible? 2}
                            {:a 3}))
         {"fooBar" 1 "isVisible" 2 "a" 3}))
  (is (= (js->clj (p/map->js-obj {:version-number 4}))
         {"versionNumber" 4})))

(deftest about-nil
  (let [t #js {"fooBar" 1
               "subMenu" #js {"subItem" 2}}]
    (is (nil? (p/get nil :a))) ; Don't throw exception
    (is (nil? (p/get t :a)))
    (is (nil? (p/get t nil)))
    (is (nil? (p/get t :sub-menu/nonexistent-item)))
    (is (nil? (p/get nil :sub-menu/nonexistent-item)))
    (is (nil? (p/get t :none/sub-item))) ; Don't throw exception
    (is (nil? (p/set! nil :new-value 1))) ; Don't throw exception
    (p/set! t :foo-bar nil)
    (is (nil? (p/get t :foo-bar)))
    (p/set! t :sub-menu nil)
    (is (nil? (p/get t :sub-menu/sub-item)))))

(defn- contains-recursively? [tree target]
  (cond
    (= tree target) true
    (map? tree) (loop [kvs tree]
                  (cond
                    (empty? kvs) false
                    (contains-recursively? (ffirst kvs) target) true
                    (contains-recursively? (nfirst kvs) target) true
                    :else (recur (rest kvs))))
    (coll? tree) (some #(contains-recursively? % target) tree)
    ;; NB: Omit to support atom, js-obj, js-array, and so on
    :else false))

;;; `p/get` and `p/set!` are macro, expand to `aget` and `aset`.
;;; But if key was expr, run as fn.
;;; It's slowly and will cause GC, but works correctly
(deftest about-fn
  (let [t #js {"fooBarBaz" 1
               "subLayer" #js {"item2" 2}}
        k1 :foo-bar-baz
        k2 :sub-layer/item2
        k3 :found?
        k4 :none/item]
    (is (= 1 (p/get t k1)))
    (is (= 1 (p/get t (identity k1))))
    (is (= 2 (p/get t k2)))
    (is (= 2 (p/get t (identity k2))))
    (is (nil? (p/get nil k1)))
    (is (nil? (p/get nil (identity k1))))
    (is (nil? (p/get nil k2)))
    (is (nil? (p/get nil (identity k2))))
    (is (nil? (p/get t nil)))
    (p/set! t k1 3)
    (is (= 3 (p/get t k1)))
    (p/set! t k2 4)
    (is (= 4 (p/get t k2)))
    (p/set! t k3 true)
    (is (= true (p/get t k3)))
    (p/set! t k4 5)
    (is (nil? (p/get t k4))) ; because (aget t "none") is not exist
    (is (nil? (p/set! nil k1 true)))
    (is (nil? (p/set! nil (identity k1) true)))
    (is (nil? (p/set! nil k2 true)))
    (is (nil? (p/set! nil (identity k2) true)))))

;;; Expand to `aget` / `aset`.
;;; Don't expand like `(. obj -propertyName)`
;;; (Because avoid to mangle name always)
(deftest about-macroexpand
  (is (contains-recursively? (macroexpand '(p/get t :foo-bar))
                             "fooBar"))
  (is (contains-recursively? (macroexpand '(p/get t :foo-bar))
                             'clojure.core/aget))
  (is (contains-recursively? (macroexpand '(p/get t :parent-node/child-node))
                             "parentNode"))
  (is (contains-recursively? (macroexpand '(p/get t :parent-node/child-node))
                             "childNode"))
  (is (contains-recursively? (macroexpand '(p/set! t :foo-bar 123))
                             "fooBar"))
  (is (contains-recursively? (macroexpand '(p/set! t :foo-bar 123))
                             'clojure.core/aset))
  (is (contains-recursively? (macroexpand '(p/set! t :par-ent/chi-ld 45))
                             "parEnt"))
  (is (contains-recursively? (macroexpand '(p/set! t :par-ent/chi-ld 45))
                             "chiLd"))
  (is (not (contains-recursively? (macroexpand '(p/get t k))
                                  'clojure.core/aget)))
  (is (not (contains-recursively? (macroexpand '(p/get t (expr)))
                                  'clojure.core/aget)))
  (is (not (contains-recursively? (macroexpand '(p/set! t k 123))
                                  'clojure.core/aset)))
  (is (not (contains-recursively? (macroexpand '(p/set! t (expr) 123))
                                  'clojure.core/aset))))

(deftest avoid-eval-object-twice-by-macro
  (let [a-count (atom 0)
        o #js {"table" #js {}}
        o-fn (fn []
               (swap! a-count inc)
               o)]
    (p/set! (o-fn) :foo 1)
    (is (= 1 @a-count))
    (p/set! (o-fn) :table/child 2)
    (is (= 2 @a-count))
    (p/get (o-fn) :table/child)
    (is (= 3 @a-count))
    (p/get (o-fn) :none/item)
    (is (= 4 @a-count))
    (p/get (o-fn) :foo)
    (is (= 5 @a-count))
    (p/merge! (o-fn) {:a 1 :b 2 :c 3} {:d 4 :e 5})
    (is (= 6 @a-count))))

