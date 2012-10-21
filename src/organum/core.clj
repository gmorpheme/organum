(ns organum.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defrecord Root [content])
(defrecord Section [level text content tags keyword])
(defrecord Block [type qualifier content])
(defrecord Drawer [content])
(defrecord Line [type line])

(defn classify-line
  "Classify a line for dispatch to handle-line multimethod."
  [line]
  (let [headline-re #"^(\*+)\s*(.*)$"
        pdrawer-re #"^\s*:(PROPERTIES|END):"
        pdrawer (fn [line] (second (re-matches pdrawer-re line)))
        pdrawer-item-re #"^\s*:([0-9A-Za-z_\-]+):\s*(.*)$"
        block-re #"^\s*#\+(BEGIN|END)_(\w*)\s*([0-9A-Za-z_\-]*)?"
        block (fn [line] (rest (re-matches block-re line)))
        def-list-re #"^\s*(-|\+|\s+[*])\s*(.*?)::"
        ordered-list-re #"^\s*\d+(\.|\))\s+"
        unordered-list-re #"^\s*(-|\+|\s+[*])\s+"
        metadata-re #"^\s*(CLOCK|DEADLINE|START|CLOSED|SCHEDULED):"
        table-sep-re #"^\s*\|[-\|\+]*\s*$"
        table-row-re #"^\\s*\\|"
        inline-example-re #"^\s*:\s"
        horiz-re #"^\s*-{5,}\s*$"]
    (cond
     (re-matches headline-re line) :headline
     (string/blank? line) :blank
     (re-matches def-list-re line) :definition-list
     (re-matches ordered-list-re line) :ordered-list
     (re-matches unordered-list-re line) :unordered-list
     (= (pdrawer line) "PROPERTIES") :property-drawer-begin-block
     (= (pdrawer line) "END") :property-drawer-end-block
     (re-matches pdrawer-item-re line) :property-drawer-item
     (re-matches metadata-re line) :metadata
     (= (first (block line)) "BEGIN") :begin-block
     (= (first (block line)) "END") :end-block
     (= (second (block line)) "COMMENT") :comment
     (= (first line) \#) :comment
     (re-matches table-sep-re line) :table-separator
     (re-matches table-row-re line) :table-row
     (re-matches inline-example-re line) :inline-example
     (re-matches horiz-re line) :horizontal-rule
     :else :paragraph
     )))

(defn strip-tags
  "Return the line with tags stripped out and list of tags"
  [line]
  (if-let [[_ text tags] (re-matches #"(.*?)\s*(:[\w:]*:)\s*$" line)]
    [text (remove string/blank? (string/split tags #":"))]
    [line nil]))

(defn strip-keyword
  "Return the line with keyword stripped out and list of keywords"
  [line]
  (let [keywords-re #"(TODO|DONE)?"
        words (string/split line #"\s+")]
    (if (re-matches keywords-re (words 0))
      [(string/triml (string/replace-first line (words 0) "")) (words 0)] 
      [line nil])))

(defn parse-headline [l]
  (when-let [[_ prefix text] (re-matches  #"^(\*+)\s*(.*?)$" l)]
    (let [[text tags] (strip-tags text)
          [text kw] (strip-keyword text)]
      (Section. (count prefix) text nil tags kw))))

(defn parse-block [l]
  (let [block-re #"^\s*#\+(BEGIN|END)_(\w*)\s*([0-9A-Za-z_\-]*)?"
        [_ _ type qualifier] (re-matches block-re l)]
    (Block. type qualifier nil)))

;; State helpers

(defn subsume
  "Updates the current node (header, block, drawer) to contain the specified
   item."
  [state item]
  (let [top (last state)
        new (update-in top [:content] conj item)]
    (conj (pop state) new)))

(defn subsume-top
  "Closes off the top node by subsuming it into its parent's content"
  [state]
  (let [top (last state)
        state (pop state)]
    (subsume state top)))

(defmulti handle-line (fn [state line] (classify-line line)))

(defmethod handle-line :headline [state line]
  (let [hl (parse-headline line)]
    (conj state hl)))

(defmethod handle-line :begin-block [state line]
  (let [bl (parse-block l)]
    (conj state bl)))

(defmethod handle-line :end-block [state line]
  (subsume-top state))

(defmethod handle-line :property-drawer-begin-block [state line]
  (conj state (Drawer. nil)))

(defmethod handle-line :property-drawer-end-block [state line]
  (subsume-top state))

(defmethod handle-line :default [state line]
  (subsume state (Line. (classify-line line) line)))

(defn in-buffer-setting [line]
  (rest (re-matches #"^#\+(\w+):\s*(.*)$" line)))

(defn initial-state [] [(Root. [])])

(defn parse-file [f]
  (with-open [rdr (io/reader f)]
    (reduce handle-line (initial-state) (line-seq rdr))))