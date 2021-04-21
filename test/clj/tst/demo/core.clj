(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require [com.rpl.specter :as sp]
            [schema.core :as s]
            [tupelo.misc :as misc]
            [tupelo.schema :as tsk]))


(declare walk-impl walk-map walk-list walk-set)

(defn type-short
  [arg]
  (cond (xmap? arg)        :map
        (xsequential? arg) :list
        (set? arg)         :set
        :else              :leaf))

(dotest
  (is= :map (type-short {:a 1}))
  (is= :list (type-short [1 2 3]))
  (is= :set (type-short #{:a "b" 1}))
  (is= :leaf (type-short "Hello")))

(defn walk-leaf
  [stack leafy]
  (with-spy-indent
   ;(newline)
   ;(spy :walk-leaf leafy)
   ;(spyx-pretty stack)
  ))

(defn walk-map-entry
  [stack me]
  (with-spy-indent
    (newline)
    (spy :walk-map-entry me)
    (spyx-pretty stack)
    (let [me-key        (key me)
          me-val        (val me)
          info-key      {:type :map-entry-key, :raw me-key}
          info-val      {:type :map-entry-val, :raw me-val}
          stack-new-key (prepend info-key stack)
          stack-new-val (prepend info-val stack)]
      (walk-impl stack-new-key me-key)
      (walk-impl stack-new-val me-val))))

(defn walk-list-entry
  [stack le]
  (with-spy-indent
    (newline)
    (spy :walk-list-entry le)
    (spyx-pretty stack)
    (let [me-key        (xfirst le)
          me-val        (xsecond le)
          info-idx      (glue {:type :list-entry-idx, :raw me-key})
          info-val      (glue {:type :list-entry-val, :raw me-val})
          stack-new-idx (prepend info-idx stack)
          stack-new-val (prepend info-val stack)]
      (walk-impl stack-new-idx me-key)
      (walk-impl stack-new-val me-val))))


(defn walk-map
  [stack mappy]
  (with-spy-indent
    ;(newline)
    ;(spy
    ;:-----------------------------------------------------------------------------)
    ;(spy :walk-map mappy)
    ;(spyx-pretty stack)
    (forv [me mappy]
          (let [me-key    (key me)
                me-val    (val me)
                info      (glue {:type :map-entry, :raw me} (vals->map me-key me-val))
                stack-new (prepend info stack)]
            (walk-map-entry stack-new me)))))

(defn walk-list
  [stack listy]
  (with-spy-indent
    ;(newline)
    ;(spy
    ;:-----------------------------------------------------------------------------)
    ;(spy :walk-list listy)
    ;(spyx-pretty stack)
    (forv [le (indexed listy)]
          (let [le-idx    (xfirst le)
                le-val    (xsecond le)
                info      (glue {:type :list-entry, :raw le} (vals->map le-idx le-val))
                stack-new (prepend info stack)]
            (walk-list-entry stack-new le)))))

(defn walk-set
  [stack setty]
  (with-spy-indent
    ;(newline)
    ;(spy
    ;:-----------------------------------------------------------------------------)
    ;(spy :walk-set setty)
    ;(spyx-pretty stack)
    (forv [item setty]
          (let [info      {:type :set-elem, :raw item}
                stack-new (prepend info stack)]
            (walk-impl stack-new item)))))

(defn walk-impl
  [stack item]
  (let [type      (type-short item)
        raw       item
        info      (vals->map type raw)
        stack-new (prepend info stack)]
    (newline)
    (spy :walk-impl item)
    (spyx-pretty stack-new)
    (cond (= :map type)  (walk-map stack-new item)
          (= :list type) (walk-list stack-new item)
          (= :set type)  (walk-set stack-new item)
          :else          (walk-leaf stack-new item))))

(defn walk [item] (walk-impl [] item))

