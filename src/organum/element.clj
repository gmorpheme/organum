(ns ^{:doc "Org-mode parsers / interpreters based on direct functional port of org-element.el,
omitting visibility settings and only using maxium granularity."}
  organum.element
  (:require [clojure.string :as str]))

(defn re-match-all
  "Regular expression match returning sequence of groups with index of start and end
as well as match contents."
  [re s]
  (let [m (re-matcher re s)]
    (and
     (.matches m)
     (for [i (range (inc (.groupCount m)))]
       {:start (.start m i)
        :end (.end m i)
        :group (.group m i)}))))

(defn re-search-forward
  [re s i]
  (let [m (re-matcher re s)]
    (and
     (.find m i)
     (for [i (range (inc (.groupCount m)))]
       {:start (.start m i)
        :end (.end m i)
        :group (.group m i)}))))

(defn re-search-backward
  [re s n]
  (let [m (re-matcher re s)
        find-after (fn [x]
                     (when (.find m x)
                       (let [result (.start m 0)]
                         (and (< result n) result))))]
    (when-let [fst (find-after 0)]
      (loop [i fst]
        (if-let [next (find-after (inc i))]
          (recur next)
          i)))))

;; buffer abstraction

(defprotocol Buffer
  (point [self])
  (point-max [self])
  (goto-char [self n])
  (at [self n])
  (looking-at [self re]))

(defrecord IndexedString [s i]
    Buffer
    (point [self] i)
    (point-max [self] (count s))
    (goto-char [self n]
      (IndexedString. s n))
    (at [self n]
      (.charAt s n))
    (looking-at [self re]
      (re-match-all re s)))

(defn buffer
  ([str]
     (buffer str 0))
  ([str point]
     (if (instance? String str)
       (IndexedString. str point))))

(defn bolp [b]
  (or
   (= (point b) 0)
   (#{\newline \return} (at b (dec (point b))))))

(defn backward-char [b n]
  (goto-char b (- (point b) n)))

(defn skip-chars-forward [b s]
  (let [p (into #{} s)]
    (loop [i (point b)]
      (if (and
           (< i (point-max b))
           (p (at b i)))
        (recur (inc i))
        (- i (point b))))))

(defn skip-chars-forward-m [s]
  (fn [b]
    (let [n (skip-chars-forward b s)]
      [n (goto-char b (+ n (point b)))])))

(defn skip-whitespace-m []
  (fn [[b n]]
    (loop [i n]
      (if (.isWhitespace (at b n))
        (recur (inc i))
        [nil [b i]]))))

;; org-emph-re

(def emphasis-regexp-components
  {:pre " \t('\"{"
   :post "- \t.,:!?;'\")}\\\\"
   :border " \t\r\n,\"'"
   :body "."
   :newline 1})

(def emph-re

  (let [{:keys [pre post border body newline]} emphasis-regexp-components
        markers (apply str ["*" "/" "_" "=" "~" "+"])
        body (str body "*?")]
    (re-pattern    
     (str "([" pre "]|^)"
          "("
          "([" markers "])"
          "([^" border "]|[^" border "]" body "[^" border "])"
          "\\3"
          ")"
          "([" post "]|$)"
          ))))

;; org element parsers

(defn- emph-parser-m 
  "Parse emphasis object at point.

Return a vector whose first element is kw and second is a map with
`:begin', `:end', `:contents-begin' and `:contents-end' and
`:post-blank' keywords.

Assume point is at the first emphasis marker."
  [kw]
  (fn [buf]
    (let [b (if (not (bolp buf)) (backward-char buf 1) buf)
          [_ _ {begin :start end :end} _ {contents-begin :start contents-end :end value :group} _] (looking-at b emph-re)
          [post-blank b] ((skip-chars-forward-m " \t") (goto-char b end))]
      [[kw {:begin begin
            :end end
            :contents-begin contents-begin
            :contents-end contents-end
            :post-blank post-blank}]
       buf])))

(defn bold-parser-m []
  (emph-parser-m :bold))

(defn bold-interpreter
  "Interpret bold object as Org syntax.
contents is the contents of the object."
  [bold contents]
  (format "*%s*" contents))

(defn italic-parser-m []
  (emph-parser-m :italic))

(defn italic-interpreter
  "Interpret italic object as Org syntax.
contents is the contents of the object."
  [italic contents]
  (format "/%s/" contents))

(defn underline-parser-m []
  (emph-parser-m :underline))

(defn underline-interpreter
  "Interpret underline object as Org syntax.
contents is the contents of the object."
  [underline contents]
  (format "_%s_" contents))

(defn strike-through-parser-m []
  (emph-parser-m :strike-through))

(defn strike-through-interpreter
  "Interpret strike-through object as Org syntax.
contents is the contents of the object."
  [strike-through contents]
  (format "+%s+" contents))

(defn verbatim-parser-m []
  (emph-parser-m :verbatim))

(defn verbatim-interpreter 
  "Interpret verbatim object as Org syntax.
contents is the contents of the object."
  [verbatim contents]
  (format "=%s=" contents))

(defn code-parser-m []
  (emph-parser-m :code))

(defn code-interpreter
    "Interpret code object as Org syntax.
contents is the contents of the object."
  [code contents]
  (format "~%s~" contents))

(defn heading-components []
  )