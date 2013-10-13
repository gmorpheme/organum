(ns organum.element-test
  (:use [midje.sweet]
        [organum.element]))

(facts
  "Regex fns"
  (re-search-forward #"(x)" " x x x x" 3) => [{:start 3, :end 4, :group "x"} {:start 3, :end 4, :group "x"}]
  (re-search-forward #"(x)" " x x x x" 4) => [{:start 5, :end 6, :group "x"} {:start 5, :end 6, :group "x"}])

(facts
  "Emphasis regex"
  (re-matches emph-re " _Hello_ ") => [" _Hello_ " " " "_Hello_" "_" "Hello" " "]
  (re-matches emph-re " *Hello* ") => [" *Hello* " " " "*Hello*" "*" "Hello" " "]
  (re-matches emph-re " /f/\t") => [" /f/\t" " " "/f/" "/" "f" "\t"])

(facts
  "Basic buffer fns"
  (let [x (buffer "test")]
    (point x) => 0
    (point-max x) => 4
    (point (goto-char x 2)) => 2
    (at x 2) => \s))

(facts
  "Extended buffer utils"
  (bolp (goto-char (buffer "\nx") 1)) => \newline
  (bolp (goto-char (buffer "\rx") 1)) => \return
  (bolp (goto-char (buffer "blah") 0)) => true
  (bolp (goto-char (buffer "blah") 1)) => nil
  (point (backward-char (buffer "test" 3) 3)) => 0
  (skip-chars-forward (buffer "test") "te") => 2)

(facts
  "Emphasis parser"
  
  (first ((bold-parser-m) (buffer " *blah*" 1))) => [:bold {:begin 1
                                                            :end 7
                                                            :contents-begin 2
                                                            :contents-end 6
                                                            :post-blank 0}]

  (first ((italic-parser-m) (buffer " /blah/" 1))) => [:italic {:begin 1
                                                                :end 7
                                                                :contents-begin 2
                                                                :contents-end 6
                                                                :post-blank 0}]

  (first ((underline-parser-m) (buffer " /blah/" 1))) => [:underline {:begin 1
                                                                      :end 7
                                                                      :contents-begin 2
                                                                      :contents-end 6
                                                                      :post-blank 0}])

(facts
  "Emphasis interpreters"
  (bold-interpreter '_ "hello") => "*hello*"
  (italic-interpreter '_ "hello") => "/hello/"
  (underline-interpreter '_ "blah") => "_blah_")