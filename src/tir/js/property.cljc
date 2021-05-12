(ns tir.js.property
  (:refer-clojure :exclude [aget aset get set! assoc!])
  #?(:cljs (:require-macros [tir.js.property :as m]))
  (:require [clojure.string :as string]))

;;; differ between this and camel-snake-kebab a bit
(defn kebab-string->camel-string [s]
  (assert (string? s))
  (let [[_ prefix s] (re-matches #"(\-*)(.*)" s)
        s (if-let [[_ m] (re-matches #"(.*)\?" s)]
            (str "is-" m)
            s)
        s (string/replace s #"-." #(string/upper-case (subs % 1)))]
    (str prefix s)))
(def kebab->camel kebab-string->camel-string)

(defn- expr? [o]
  (cond
    (symbol? o) true
    (list? o) true
    :else false))



;;; cljs.core/aget and cljs.core/aset is not better for about nil

#?(:clj (defmacro aget [o k]
          `(let [o# ~o
                 k# ~k]
             (when-not (nil? o#)
               (when-not (nil? k#)
                 (~'unchecked-get o# k#))))))

#?(:clj (defmacro aset [o k v]
          `(let [o# ~o
                 k# ~k]
             (when-not (nil? o#)
               (when-not (nil? k#)
                 (~'unchecked-set o# k# ~v))))))

#?(:clj (defmacro censor-nil [o error-msg]
          `(let [o# ~o]
             (if (nil? o#)
               (throw (js/Error. ~error-msg))
               o#))))

#?(:clj (defmacro aset-js [o k v]
          `(~'unchecked-set (censor-nil ~o "Object not found")
                            (censor-nil ~k "Key must not be nil")
                            ~v)))

#?(:clj (defmacro js-delete [o k]
          `(let [o# ~o
                 k# ~k]
             (when-not (nil? o#)
               (when-not (nil? k#)
                 (~'js-delete o# k#))))))



#?(:cljs (defn set!-fn [o & args]
           (when-not (nil? o)
             (assert (even? (count args)))
             (loop [kv-list args]
               (when-not (empty? kv-list)
                 (let [[k v & carry-over] kv-list]
                   (when-not (nil? k)
                     (let [has-name? (or (keyword? k) (symbol? k))
                           nk-camel (if has-name?
                                      (kebab->camel (name k))
                                      k)
                           o (if-let [nsk (when has-name?
                                            (namespace k))]
                               (let [nsk-camel (kebab->camel nsk)
                                     o2 (m/aget o nsk-camel)]
                                 (if-not (nil? o2)
                                   o2
                                   (let [o2 (js-obj)]
                                     (m/aset o nsk-camel o2)
                                     o2)))
                               o)]
                       (when-not (nil? o)
                         (m/aset o nk-camel v))))
                   (recur carry-over))))
             o)))

#?(:cljs (defn set-js!-fn [o & args]
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
                         (m/aget o (kebab->camel nsk))
                         o)]
                 (m/aset-js o nk-camel v)
                 (recur carry-over))))
           o))

#?(:cljs (defn get-fn [o k]
           (when-not (nil? o)
             (let [has-name? (or (keyword? k) (symbol? k))
                   nk-camel (if has-name?
                              (kebab->camel (name k))
                              k)
                   o (if-let [nsk (when has-name?
                                    (namespace k))]
                       (m/aget o (kebab->camel nsk))
                       o)]
               (m/aget o nk-camel)))))

#?(:cljs (defn delete!-fn [o k]
           (when-not (nil? o)
             (let [has-name? (or (keyword? k) (symbol? k))
                   nk-camel (if has-name?
                              (kebab->camel (name k))
                              k)
                   o (if-let [nsk (when has-name?
                                    (namespace k))]
                       (m/aget o (kebab->camel nsk))
                       o)]
               (m/js-delete o nk-camel)))))

#?(:cljs (defn merge! [o & ms]
           (doseq [m ms]
             (doseq [[k v] m]
               (set!-fn o k v)))
           o))

#?(:cljs (defn merge-js! [o & ms]
           (doseq [m ms]
             (doseq [[k v] m]
               (set-js!-fn o k v)))
           o))

;;; NB: this is shallow !!!
#?(:cljs (defn map->js-obj [m]
           (merge! (js-obj) m)))



#?(:clj (defmacro -set! [o & args]
          (assert (even? (count args)))
          (let [kv-list (partition 2 args)
                ok-list (remove (fn [[k v]] (expr? k)) kv-list)
                ng-list (filter (fn [[k v]] (expr? k)) kv-list)
                o-sym (gensym)
                ok-bodies (map (fn [[k v]]
                                 (when-not (nil? k)
                                   (if-not (keyword? k)
                                     `(aset ~o-sym ~k ~v)
                                     (let [nk-camel (kebab->camel (name k))]
                                       (if-let [nsk (namespace k)]
                                         (let [nsk-camel (kebab->camel nsk)]
                                           `(let [v# ~v
                                                  o2# (aget ~o-sym ~nsk-camel)]
                                              (if-not (nil? o2#)
                                                (aset o2# ~nk-camel v#)
                                                (let [o2# (cljs.core/js-obj)]
                                                  (aset ~o-sym ~nsk-camel o2#)
                                                  (aset o2# ~nk-camel v#)))))
                                         `(aset ~o-sym ~nk-camel ~v))))))
                               ok-list)
                ng-body (if (empty? ng-list)
                          o-sym
                          `(set!-fn ~o-sym ~@(apply concat ng-list)))]
            `(let [~o-sym ~o]
               (when-not (nil? ~o-sym)
                 ~@ok-bodies
                 ~ng-body)))))

#?(:clj (defmacro -set-js! [o & args]
          (assert (even? (count args)))
          (let [kv-list (partition 2 args)
                ok-list (remove (fn [[k v]] (expr? k)) kv-list)
                ng-list (filter (fn [[k v]] (expr? k)) kv-list)
                o-sym (gensym)
                ok-bodies (map (fn [[k v]]
                                 (if-not (keyword? k)
                                   `(aset-js ~o-sym ~k ~v)
                                   (let [nk-camel (kebab->camel (name k))]
                                     (if-let [nsk (namespace k)]
                                       (let [nsk-camel (kebab->camel nsk)]
                                         `(aset-js (aget ~o-sym ~nsk-camel)
                                                ~nk-camel
                                                ~v))
                                       `(aset-js ~o-sym ~nk-camel ~v)))))
                               ok-list)
                ng-body (if (empty? ng-list)
                          o-sym
                          `(set-js!-fn ~o-sym ~@(apply concat ng-list)))]
            `(let [~o-sym ~o]
               ~@ok-bodies
               ~ng-body))))

#?(:clj (defmacro -get [o k]
          (cond
            (nil? o) nil
            (nil? k) nil
            (expr? k) `(get-fn ~o ~k)
            (not (keyword? k)) `(aget ~o ~k)
            :else (let [nk-camel (kebab->camel (name k))]
                    (if-let [nsk (namespace k)]
                      `(aget (aget ~o ~(kebab->camel nsk))
                             ~nk-camel)
                      `(aget ~o ~nk-camel))))))

#?(:clj (defmacro -delete! [o k]
          (cond
            (nil? o) nil
            (nil? k) nil
            (expr? k) `(delete!-fn ~o ~k)
            (not (keyword? k)) `(js-delete ~o ~k)
            :else (let [nk-camel (kebab->camel (name k))]
                    (if-let [nsk (namespace k)]
                      `(js-delete (aget ~o ~(kebab->camel nsk))
                                  ~nk-camel)
                      `(js-delete ~o ~nk-camel))))))

#?(:clj (defmacro assoc! [o & args] `(-set! ~o ~@args)))
#?(:clj (defmacro set! [o & args] `(-set! ~o ~@args)))
#?(:clj (defmacro set-js! [o & args] `(-set-js! ~o ~@args)))
#?(:clj (defmacro get [o k] `(-get ~o ~k)))
#?(:clj (defmacro delete! [o k] `(-delete! ~o ~k)))


#?(:clj (defmacro update! [o k f & args]
          (assert (ifn? f))
          (if (keyword? k)
            `(let [o# ~o]
               (-set! o# ~k (~f (-get o# ~k) ~@args)))
            `(let [o# ~o
                   k# ~k]
               (-set! o# k# (~f (-get o# k#) ~@args))))))





