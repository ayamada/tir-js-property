(ns tir.js.property
  (:refer-clojure :exclude [get set! assoc!])
  (:require [clojure.string :as string]))

;;; TODO: implement delete! (but scarcely used)

;;; differ between this and camel-snake-kebab a bit
(defn kebab-string->camel-string [s]
  (assert (string? s))
  (let [s (if-let [[_ m] (re-find #"^(.*)\?$" s)]
            (str "is-" m)
            s)
        s (string/replace s #"-." #(string/upper-case (subs % 1)))]
    s))
(def kebab->camel kebab-string->camel-string)

(defn- expr? [o]
  (cond
    (symbol? o) true
    (list? o) true
    :else false))

#?(:clj (defmacro -set! [o & args]
          (assert (even? (count args)))
          (let [kv-list (partition 2 args)
                ok-list (remove (fn [[k v]] (expr? k)) kv-list)
                ng-list (filter (fn [[k v]] (expr? k)) kv-list)
                o-sym `o#
                ok-bodies (map (fn [[k v]]
                                 (if-not (keyword? k)
                                   `(aset ~o-sym ~k ~v)
                                   (let [nk-camel (kebab->camel (name k))]
                                     (if-let [nsk (namespace k)]
                                       `(when-let [o2# (aget ~o-sym ~(kebab->camel nsk))]
                                          (aset o2# ~nk-camel ~v))
                                       `(aset ~o-sym ~nk-camel ~v)))))
                               ok-list)
                ng-body (if (empty? ng-list)
                          o-sym
                          `(set!-fn ~o-sym ~@(apply concat ng-list)))]
            `(when-let [~o-sym ~o]
               ~@ok-bodies
               ~ng-body))))

#?(:clj (defmacro -get [o k]
          (cond
            (expr? k) `(get-fn ~o ~k)
            (not (keyword? k)) `(when-let [o# ~o]
                                  (aget o# ~k))
            :else (let [nk-camel (kebab->camel (name k))]
                    (if-let [nsk (namespace k)]
                      `(when-let [o# ~o]
                         (when-let [o2# (aget o# ~(kebab->camel nsk))]
                           (aget o2# ~nk-camel)))
                      `(when-let [o# ~o]
                         (aget o# ~nk-camel)))))))

#?(:clj (defmacro assoc! [o & args] `(-set! ~o ~@args)))
#?(:clj (defmacro set! [o & args] `(-set! ~o ~@args)))
#?(:clj (defmacro get [o k] `(-get ~o ~k)))

#?(:cljs (defn set!-fn [o & args]
           (when o
             (assert (even? (count args)))
             (loop [kv-list args]
               (when-not (empty? kv-list)
                 (let [[k v & carry-over] kv-list
                       has-name? (or (keyword? k) (symbol? k))
                       nk-camel (if has-name?
                                  (kebab->camel (name k))
                                  k)
                       o (if-let [nsk (when has-name?
                                        (namespace k))]
                           (aget o (kebab->camel nsk))
                           o)]
                   (when o
                     (aset o nk-camel v))
                   (recur carry-over))))
             o)))

#?(:cljs (defn get-fn [o k]
           (when o
             (let [has-name? (or (keyword? k) (symbol? k))
                   nk-camel (if has-name?
                              (kebab->camel (name k))
                              k)
                   o (if-let [nsk (when has-name?
                                    (namespace k))]
                       (aget o (kebab->camel nsk))
                       o)]
               (when o
                 (aget o nk-camel))))))

#?(:cljs (defn merge! [o & ms]
           (doseq [m ms]
             (doseq [[k v] m]
               (set!-fn o k v)))
           o))

;;; NB: this is shallow !!!
#?(:cljs (defn map->js-obj [m]
           (merge! (js-obj) m)))
