(ns tir.js.property-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [tir.js.property :as p :include-macros true]))

(deftest basic
  (let [t #js {"fooBar" 1
               "foo-buz" 2
               "WebGLVersion" 2
               "jsLikeCamelProperty3" 3
               "isVisible" true
               "subMenu" #js {"itemName" "exit"}}]
    ;; p/get
    (is (= 1 (p/get t :foo-bar))) ; Camelize keyword automatically
    (is (= 1 (p/get t :fooBar))) ; But, can give camel case keyword too
    (is (= 2 (p/get t "foo-buz"))) ; Not camelize string
    (is (= 2 (p/get t "WebGLVersion"))) ; Use string for complex word
    (is (= 3 (p/get t :js-like-camel-property-3)))
    (is (= 3 (p/get t :js-like-camel-property3))) ; Same
    (is (= true (p/get t :visible?))) ; Convert from `?` suffix to `is` prefix
    (is (= "exit" (p/get t :sub-menu/item-name))) ; Suport until two depth
    ;; p/set!
    (p/set! t :a-new-property-123 123) ; `p/set!` can set new property value
    (is (= 123 (aget t "aNewProperty123"))) ; `p/set!` works
    (p/set! t :i 4 :j 5 :k 6) ; `p/set!` can set multiple properties
    (is (= 4 (p/get t :i)))
    (is (= 5 (p/get t :j)))
    (is (= 6 (p/get t :k)))
    (p/assoc! t :i 7 :j 8 :k 9) ; `p/assoc!` is alias of `p/set!`
    (is (= 7 (p/get t :i)))
    (is (= 8 (p/get t :j)))
    (is (= 9 (p/get t :k)))
    (is (= t (p/set! t :new-value 1))) ; `p/set!` return target object always
    (is (= 1 (aget t "newValue")))
    (p/set! t :sub-menu/item-num 23) ; `p/set!` can set two depth property
    (is (= 23 (aget (aget t "subMenu") "itemNum")))
    (is (= "exit" (aget (aget t "subMenu") "itemName"))) ; Not affected
    (is (= (js->clj (p/set! #js {} :none/child 4)) ; `p/set!` may create
           {"none" {"child" 4}})) ; nonexistence parent node automatically
    (p/set! t :--private-cljs-prop 5) ; `:--` as a mark of private use
    (is (= (aget t "-privateCljsProp") 5)) ; Js property name don't begin `-`
    (p/set! t :foo-bar/child 6) ; Do nothing, because "fooBar" already exists,
    (is (= 1 (p/get t :foo-bar))) ; and js allow `(1)["child"] = 6` but inanity
    ;; p/set-js!
    (p/set-js! t :new-p 12)
    (is (= 12 (aget t "newP"))) ; `p/set-js!` works
    (p/set-js! t :new-p2 34 :sub-menu/new-p2 56)
    (is (= 34 (aget t "newP2"))) ; `p/set-js!` works
    (is (= 56 (p/get t :sub-menu/new-p2)))
    (is (thrown? js/Error (p/set-js! #js {} :none/child 7))) ; Not found "none"
    (p/set-js! t :foo-bar/child 6) ; Js allow `(1)["child"] = 6` but inanity
    ;; p/delete!
    (p/delete! t :new-p)
    (is (nil? (aget t "newP")))
    (is (= 3 (count (seq (js->clj (p/get t :sub-menu))))))
    (p/delete! t :sub-menu/item-name)
    (is (= 2 (count (seq (js->clj (p/get t :sub-menu))))))
    (p/delete! t :sub-menu)
    (is (nil? (p/get t :sub-menu)))
    (is (= 1 (p/get t :foo-bar)))
    (is (= 12 (count (seq (js->clj t)))))
    (is (nil? (p/get t :none-leaf)))
    (p/delete! t :none-leaf) ; Do nothing
    (is (nil? (p/get t :none-leaf)))
    (is (= 12 (count (seq (js->clj t)))))
    (is (nil? (p/get t :none-node)))
    (p/delete! t :none-node/sub-item) ; Do nothing
    (is (nil? (p/get t :none-node)))
    (is (= 12 (count (seq (js->clj t)))))
    ))

(deftest util
  ;; These are useful for make sure key conversion
  (is (= "isEnableCache" (p/kebab-string->camel-string "enable-cache?")))
  (is (= "isEnableCache" (p/kebab->camel "enable-cache?"))) ; Alias
  (is (thrown? js/Error (p/kebab->camel :enable-cache?))) ; Must be string
  ;; These are utility functions
  (is (= (js->clj (p/merge! (js-obj)
                            {:foo-bar 1 :visible? 2}
                            {:a 3}))
         {"fooBar" 1 "isVisible" 2 "a" 3}))
  ;; Caution: this is shallow, cannot convert depth of property
  (is (= (js->clj (p/map->js-obj {:version-number 4
                                  :parent/child 5
                                  :foo-bar {:baz-hoge 123}}))
         {"versionNumber" 4
          "parent" {"child" 5}
          "fooBar" {:baz-hoge 123}})))

(deftest about-nil
  (let [t #js {"fooBar" 1
               "fooBar2" 2
               "subMenu" #js {"subItem" 2}}]
    ;; p/get
    (is (nil? (p/get nil :a))) ; Don't throw exception
    (is (nil? (p/get t :a)))
    (is (nil? (p/get t nil)))
    (is (nil? (p/get t :sub-menu/nonexistent-item)))
    (is (nil? (p/get nil :sub-menu/nonexistent-item)))
    (is (nil? (p/get t :none/sub-item))) ; Don't throw exception
    ;; p/set!
    (is (nil? (p/set! nil :new-value 1))) ; Don't throw exception
    (p/set! t :foo-bar nil)
    (is (nil? (p/get t :foo-bar)))
    (p/set! t :sub-menu nil)
    (is (nil? (p/get t :sub-menu/sub-item)))
    (p/set! t nil 1) ; Do nothing
    (is (nil? (p/get t nil)))
    ;; p/set-js!
    (is (thrown? js/Error (p/set-js! nil :new-value 3)))
    (is (thrown? js/Error (p/set-js! nil :none-node/sub-item 4)))
    (is (thrown? js/Error (p/set-js! t nil 2))) ; Throw exception
    (is (= 3 (count (seq (js->clj t)))))
    (is (= 2 (p/get t :foo-bar2)))
    (p/set-js! t :foo-bar2 nil) ; Can set nil, but not deleted
    (is (nil? (p/get t :foo-bar2)))
    (is (= 3 (count (seq (js->clj t)))))
    ;; p/delete!
    (p/delete! nil :foo-bar) ; Don't throw exception
    (p/delete! nil :none-node/sub-item) ; Don't throw exception
    (p/delete! t nil) ; Do nothing
    ))

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
        knil nil
        k1 :foo-bar-baz
        k2 :sub-layer/item2
        k3 :found?
        k4 :none/item
        k1' :foo-bar-baz/child
        k2' :sub-layer/new]
    ;; p/get
    (is (= 1 (p/get t k1)))
    (is (= 1 (p/get t (identity k1))))
    (is (= 2 (p/get t k2)))
    (is (= 2 (p/get t (identity k2))))
    (is (nil? (p/get t k3)))
    (is (nil? (p/get t (identity k3))))
    (is (nil? (p/get t k4)))
    (is (nil? (p/get t (identity k4))))
    (is (nil? (p/get t k1')))
    (is (nil? (p/get t (identity k1'))))
    (is (nil? (p/get nil k1)))
    (is (nil? (p/get nil (identity k1))))
    (is (nil? (p/get nil k2)))
    (is (nil? (p/get nil (identity k2))))
    (is (nil? (p/get t knil)))
    (is (nil? (p/get t (identity knil))))
    ;; p/set!
    (p/set! t k1 10)
    (is (= 10 (p/get t k1)))
    (p/set! t k2 20)
    (is (= 20 (p/get t k2)))
    (p/set! t k3 true)
    (is (= true (p/get t k3)))
    (p/set! t k4 40)
    (is (= 40 (p/get t k4)))
    (p/set! t k1' 50) ; Js allow `(10)["child"] = 50` but inanity
    (is (nil? (p/get t k1')))
    (is (not= 50 (p/get t k1)))
    (p/set! t k2' 60)
    (is (= 60 (p/get t k2')))
    (is (nil? (p/set! nil k1 true)))
    (is (nil? (p/set! nil (identity k1) true)))
    (is (nil? (p/set! nil k2 true)))
    (is (nil? (p/set! nil (identity k2) true)))
    (p/set! t knil 70) ; Do nothing
    (p/set! t (identity knil) 80) ; Do nothing
    (is (nil? (p/get t nil)))
    (is (nil? (p/get t knil)))
    (is (nil? (p/get t (identity knil))))
    ;; p/set-js!
    (js-delete t (p/kebab->camel (name k3))) ; cleanup
    (is (nil? (p/get t k3)))
    (js-delete (p/get t :sub-layer) "new") ; cleanup
    (is (nil? (p/get t k2')))
    (p/set-js! t k1 11)
    (is (= 11 (p/get t k1)))
    (p/set-js! t k2 21)
    (is (= 21 (p/get t k2)))
    (p/set-js! t k3 true)
    (is (= true (p/get t k3)))
    (p/set-js! t k4 41)
    (is (= 41 (p/get t k4)))
    (p/set-js! t k1' 51) ; Js allow `(11)["child"] = 51` but inanity
    (is (nil? (p/get t k1')))
    (is (not= 51 (p/get t k1)))
    (p/set-js! t k2' 61)
    (is (= 61 (p/get t k2')))
    (is (thrown? js/Error (p/set-js! nil k1 true)))
    (is (thrown? js/Error (p/set-js! nil (identity k1) true)))
    (is (thrown? js/Error (p/set-js! nil k2 true)))
    (is (thrown? js/Error (p/set-js! nil (identity k2) true)))
    (is (thrown? js/Error (p/set-js! t knil 71)))
    (is (thrown? js/Error (p/set-js! t (identity knil) 81)))
    ;; p/delete!
    (p/set! t k1 1)
    (p/delete! t k1)
    (is (nil? (p/get t k1)))
    (p/set! t (identity k2) 2)
    (p/delete! t (identity k2))
    (is (nil? (p/get t (identity k2))))
    (is (= 3 (count (seq (js->clj t)))))
    (p/delete! t k3)
    (is (= 2 (count (seq (js->clj t)))))
    (p/delete! t k3)
    (p/delete! t (identity k3))
    (is (= 2 (count (seq (js->clj t)))))
    (p/delete! t :none)
    (is (nil? (p/delete! t (identity k4))))
    (p/set! t k1 1)
    (is (= 2 (count (seq (js->clj t)))))
    (p/delete! t (identity k1'))
    (p/delete! nil (identity k1))
    (p/delete! nil (identity k2))
    (p/delete! t (identity knil))
    (is (= 2 (count (seq (js->clj t)))))
    ))

;;; Expand to `aget` / `aset`.
;;; Don't expand like `(. obj -propertyName)`
;;; (Because avoid to mangle name always)
(deftest about-macroexpand
  (let [getter-symbol 'unchecked-get
        setter-symbol 'tir.js.property/aset
        setter-symbol-js 'tir.js.property/aset-js
        delete-symbol 'js-delete]
    (is (contains-recursively? (macroexpand '(p/get t :foo-bar))
                               "fooBar"))
    (is (contains-recursively? (macroexpand '(p/get t :foo-bar))
                               getter-symbol))
    (is (contains-recursively? (macroexpand '(p/get t :parent-node/child-node))
                               "parentNode"))
    (is (contains-recursively? (macroexpand '(p/get t :parent-node/child-node))
                               "childNode"))
    (is (contains-recursively? (macroexpand '(p/set! t :foo-bar 123))
                               "fooBar"))
    (is (contains-recursively? (macroexpand '(p/set! t :foo-bar 123))
                               setter-symbol))
    (is (contains-recursively? (macroexpand '(p/set! t :par-ent/chi-ld 45))
                               "parEnt"))
    (is (contains-recursively? (macroexpand '(p/set! t :par-ent/chi-ld 45))
                               "chiLd"))
    (is (contains-recursively? (macroexpand '(p/set-js! t :foo-bar 123))
                               "fooBar"))
    (is (contains-recursively? (macroexpand '(p/set-js! t :foo-bar 123))
                               setter-symbol-js))
    (is (contains-recursively? (macroexpand '(p/set-js! t :par-ent/chi-ld 45))
                               "parEnt"))
    (is (contains-recursively? (macroexpand '(p/set-js! t :par-ent/chi-ld 45))
                               "chiLd"))
    (is (contains-recursively? (macroexpand '(p/delete! t :foo-bar))
                               "fooBar"))
    (is (contains-recursively? (macroexpand '(p/delete! t :foo-bar))
                               delete-symbol))
    (is (contains-recursively? (macroexpand '(p/delete! t :parent-node/child-node))
                               "parentNode"))
    (is (contains-recursively? (macroexpand '(p/delete! t :parent-node/child-node))
                               "childNode"))
    (is (not (contains-recursively? (macroexpand '(p/get t k))
                                    getter-symbol)))
    (is (not (contains-recursively? (macroexpand '(p/get t (expr)))
                                    getter-symbol)))
    (is (not (contains-recursively? (macroexpand '(p/set! t k 123))
                                    setter-symbol)))
    (is (not (contains-recursively? (macroexpand '(p/set! t (expr) 123))
                                    setter-symbol)))
    (is (not (contains-recursively? (macroexpand '(p/set-js! t k 123))
                                    setter-symbol-js)))
    (is (not (contains-recursively? (macroexpand '(p/set-js! t (expr) 123))
                                    setter-symbol-js)))
    (is (not (contains-recursively? (macroexpand '(p/delete! t k))
                                    delete-symbol)))
    (is (not (contains-recursively? (macroexpand '(p/delete! t (expr)))
                                    delete-symbol)))
    ))

(deftest eval-arguments-only-once-by-macro
  (let [a-count (atom 0)
        o #js {"table" #js {}}
        count! (fn [v]
                 (swap! a-count inc)
                 v)]
    ;; check o
    (p/set! (count! o) :foo 1)
    (is (= 1 @a-count))
    (p/set! (count! o) :table/child 2)
    (is (= 2 @a-count))
    (p/set! (count! o) :a-node/a-child 3)
    (is (= 3 @a-count))
    (p/set! (count! o) nil 4)
    (is (= 4 @a-count))
    (p/set! (count! o) :a1 11 :a2 12 :a3 13)
    (is (= 5 @a-count))
    (p/set-js! (count! o) :foo 5)
    (is (= 6 @a-count))
    (p/set-js! (count! o) :table/child 6)
    (is (= 7 @a-count))
    (is (thrown? js/Error (p/set-js! (count! o) :b-node/b-child 7)))
    (is (= 8 @a-count))
    (is (thrown? js/Error (p/set-js! (count! o) nil 8)))
    (is (= 9 @a-count))
    (p/set-js! (count! o) :a1 11 :a2 12 :a3 13)
    (is (= 10 @a-count))
    (p/get (count! o) :table/child)
    (is (= 11 @a-count))
    (p/get (count! o) :none/item)
    (is (= 12 @a-count))
    (p/get (count! o) :foo)
    (is (= 13 @a-count))
    (p/get (count! o) nil)
    (is (= 13 @a-count)) ; Don't counted, but no problem probably
    (p/delete! (count! o) :table/child)
    (is (= 14 @a-count))
    (p/delete! (count! o) :none/item)
    (is (= 15 @a-count))
    (p/delete! (count! o) :foo)
    (is (= 16 @a-count))
    (p/delete! (count! o) nil)
    (is (= 16 @a-count)) ; Don't counted, but no problem probably
    ;;; check v
    (reset! a-count 0)
    (p/set! o :foo (count! 1))
    (is (= 1 @a-count))
    (p/set! o :table/child (count! 2))
    (is (= 2 @a-count))
    (p/set! o :c-node/c-child (count! 3))
    (is (= 3 @a-count))
    (p/set! o nil (count! 4))
    (is (= 3 @a-count)) ; Don't counted, but no problem probably
    (p/set! o :a1 (count! 11) :a2 (count! 12))
    (is (= 5 @a-count))
    (p/set! o (count! :a1) 11 (count! :a2) 12)
    (is (= 7 @a-count))
    (p/set-js! o :foo (count! 5))
    (is (= 8 @a-count))
    (p/set-js! o :table/child (count! 6))
    (is (= 9 @a-count))
    (is (thrown? js/Error (p/set-js! o :d-node/d-child (count! 7))))
    (is (= 9 @a-count)) ; Don't counted, but no problem probably
    (is (thrown? js/Error (p/set-js! o nil (count! 8))))
    (is (= 9 @a-count)) ; Don't counted, but no problem probably
    (p/set-js! o :a1 (count! 11) :a2 (count! 12))
    (is (= 11 @a-count))
    (p/set-js! o (count! :a1) 11 (count! :a2) 12)
    (is (= 13 @a-count))
    ))
