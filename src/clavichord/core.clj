(ns clavichord.core
  (:refer-clojure :exclude [require])
  (:require
    [clavichord.method :as method])
  (:import
    [java.lang.reflect Modifier]))

(defn ainto
  [arr coll]
  (loop [coll coll
         i 0]
    (if (empty? coll)
      arr
      (do
        (aset arr i (first coll))
        (recur (rest coll) (inc i))))))

(defn seq->array
  ([seq]
   (seq->array java.lang.Object seq))
  ([type seq]
   (let [arr (make-array type (count seq))]
     (ainto arr seq))))

(defn public?
  [entity]
  (Modifier/isPublic (.getModifiers entity)))

(defn static?
  [entity]
  (Modifier/isStatic (.getModifiers entity)))

(defn get-public-methods
  [class]
  (filter public? (.getDeclaredMethods class)))

(defn get-public-static-methods
  [class]
  (filter static? (get-public-methods class)))

(defn get-public-object-methods
  [class]
  (filter (complement static?) (get-public-methods class)))

(defn instantiate
  [class params]
  (if (empty? params)
    (try
      (.newInstance class)
      (catch NoSuchMethodException e nil)
      (catch IllegalAccessException e nil))
    (when-let [constructor (.getConstructor class (seq->array java.lang.Class (map clojure.core/class params)))]
      (.newInstance constructor (seq->array params)))))

(defn method->fn
  [object method]
  (fn [& params]
    (.invoke method object (seq->array params))))

(defn require
  [[class & {:keys [as params namer static-only object-only] predicate filter
             :or {predicate identity namer method/name}}]]
  (let [namer (eval namer)
        predicate (eval predicate)
        class (Class/forName (name class))
        static-methods (filter predicate (get-public-static-methods class))
        object-methods (filter predicate (get-public-object-methods class))
        instance (when-not (empty? object-methods) (instantiate class params))
        ns (or as (symbol (.getName class)))]
    (create-ns ns)
    (when-not object-only
      (doseq [method static-methods]
        (let [name (symbol (.getName method))
              params (vec (take (count (.getParameters (eval method))) (repeatedly gensym)))
              f (method->fn nil method)]
          (intern ns name f))))
    (when-not static-only
      (doseq [method object-methods]
        (let [name (symbol (namer method))
              params (vec (take (count (.getParameters (eval method))) (repeatedly gensym)))
              f (method->fn instance method)]
          (intern ns name f))))
    nil))
