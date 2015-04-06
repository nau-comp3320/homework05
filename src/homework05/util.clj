(ns homework05.util
  (:require [clojure.test :as t]))

(def __ '__)
(def ___ '__)

(def special-chars
  "A map of special caharacter values."
  {'backslash (char 92)
   'backspace (char 8)
   'newline (char 10)
   'return (char 13)
   'space (char 32)
   'tab (char 9)})

(defmacro defcharpred
  [pred-name]
  (let [char-sym (symbol (.replace (name pred-name) "?" ""))]
    `(defn ~pred-name
       ~(str "Returns true if the input is a " char-sym \.)
       [c#]
       (= c# (get special-chars '~char-sym)))))

(defcharpred backslash?)
(defcharpred backspace?)
(defcharpred newline?)
(defcharpred return?)
(defcharpred space?)
(defcharpred tab?)

(defmacro defstrpred
  [pred-name char-code]
  (let [the-char (char char-code)]
    `(defn ~pred-name
       ~(str "Returns true if the input is a string containing a " the-char \.)
       [s#]
       (and (string? s#)
            (some #(= % ~the-char) s#)))))

(defstrpred multiline-string? 10)
(defstrpred string-with-double-quote? 34)
(defstrpred string-with-backslash? 92)

(def java-list
  (doto (java.util.ArrayList.)
    (.add 1)
    (.add 2)
    (.add 3)))

(def java-array
  (into-array Long [1 2 3]))

(defmacro ^:private defequiv-functions
  [sym expected-fn inputs]
  `(defmethod t/assert-expr '~sym
     [message# form#]
     (let [actual-fn# (second form#)]
       `(if (fn? ~actual-fn#)
          (let [~'result# (map (fn [~'in#]
                                 (hash-map :inputs ~'in#
                                           :expected (apply ~~expected-fn ~'in#)
                                           :actual (apply ~actual-fn# ~'in#)))
                               '~~inputs)
                ~'errors# (drop-while (fn [~'r#]
                                        (= (:expected ~'r#) (:actual ~'r#)))
                                      ~'result#)]
            (if-let [~'error# (first ~'errors#)]
              (t/do-report {:type :fail :message ~message#
                            :actual (list '~'~'=
                                          (:actual ~'error#)
                                          (apply list '~actual-fn# (:inputs ~'error#)))
                            :expected (list '~'~'=
                                            (:expected ~'error#)
                                            (apply list '~actual-fn# (:inputs ~'error#)))})
              (t/do-report {:type :pass :message ~message#
                            :expected '~form# :actual '~actual-fn#})))
          (t/do-report {:type :fail :message ~message#
                     :expected '~form# :actual (list '~'not '~form#)})))))

#_(defequiv-functions adds-5-function?
  #(+ % 5)
  (map list (take 50 (repeatedly #(- (rand-int 1000) 500)))))

(defmethod t/assert-expr 'adds-5-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [in#]
                            (hash-map :input in#
                                      :expected (+ 5 in#)
                                      :actual (~actual-fn in#)))
                            (take 50 (repeatedly #(- (rand-int 1000) 500))))
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (list '~actual-fn (:input e#)))
                         :expected (list '~'= (:expected e#) (list '~actual-fn (:input e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'third-element-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [in#]
                            (hash-map :input in#
                                      :expected (nth in# 2 nil)
                                      :actual (~actual-fn in#)))
                          (for [i# (range 50)]
                            (take (rand-int 10) (repeatedly #(- (rand-int 1000) 500)))))
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (list '~actual-fn (:input e#)))
                         :expected (list '~'= (:expected e#) (list '~actual-fn (:input e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'sums-two-args-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [[in1# in2# :as in#]]
                            (hash-map :inputs in#
                                      :expected (+ in1# in2#)
                                      :actual (~actual-fn in1# in2#)))
                          (for [i# (range 50)]
                            (take 2 (repeatedly #(- (rand-int 1000) 500)))))
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (apply list '~actual-fn (:inputs e#)))
                         :expected (list '~'= (:expected e#) (apply list '~actual-fn (:inputs e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'second-of-three-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [[in1# in2# in3# :as in#]]
                            (hash-map :inputs in#
                                      :expected in2#
                                      :actual (~actual-fn in1# in2# in3#)))
                          (for [i# (range 50)]
                            (take 3 (repeatedly #(- (rand-int 1000) 500)))))
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (apply list '~actual-fn (:inputs e#)))
                         :expected (list '~'= (:expected e#) (apply list '~actual-fn (:inputs e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'higher-order-first-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [f#]
                            (hash-map :input f#
                                      :expected (first (f#))
                                      :actual (~actual-fn f#)))
                          [#(range 10)
                           #(range 0)
                           #(range 1 11)
                           #(range 10 0 -1)
                           #(repeat 5 :x)])
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (list '~actual-fn (:input e#)))
                         :expected (list '~'= (:expected e#) (list '~actual-fn (:input e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'higher-order-multiply-by-5-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [f#]
                            (hash-map :input f#
                                      :expected (* 5 (f#))
                                      :actual (~actual-fn f#)))
                          [#(do 0)
                           #(do 5)
                           #(do -15)
                           #(do 100)])
             errors# (drop-while (fn [r#]
                                   (= (:expected r#) (:actual r#)))
                                 result#)]
         (if-let [e# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :actual (list '~'= (:actual e#) (list '~actual-fn (:input e#)))
                         :expected (list '~'= (:expected e#) (list '~actual-fn (:input e#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'multiplier-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [rand-number# #(- (rand-int 100) 50)
             result# (map (fn [[in1# in2#]]
                            (hash-map :outer-input in1#
                                      :inner-input in2#
                                      :expected (* in1# in2#)
                                      :actual ((~actual-fn in1#) in2#)))
                          (for [i# (range 50)]
                            [(rand-number#) (rand-number#)]))
             errors# (drop-while (fn [r#] (= (:expected r#) (:actual r#))) result#)]
         (if-let [error# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :expected (list '~'= (:expected error#) (list (list '~actual-fn
                                                                             (:outer-input error#))
                                                                       (:inner-input error#)))
                         :actual (list '~'= (:actual error#) (list (list '~actual-fn
                                                                         (:outer-input error#))
                                                                   (:inner-input error#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))

(defmethod t/assert-expr 'first-n-function?
  [message form]
  (let [actual-fn (second form)]
    `(if (fn? ~actual-fn)
       (let [result# (map (fn [[in1# in2#]]
                            (hash-map :outer-input in1#
                                      :inner-input in2#
                                      :expected (take in2# in1#)
                                      :actual ((~actual-fn in1#) in2#)))
                          (for [i# (range 50)]
                            [(map (fn [_#] (rand-int 100)) (range (rand-int 10)))
                             (rand-int 10)]))
             errors# (drop-while (fn [r#] (= (:expected r#) (:actual r#))) result#)]
         (if-let [error# (first errors#)]
           (t/do-report {:type :fail :message ~message
                         :expected (list '~'= (:expected error#) (list (list '~actual-fn
                                                                             (:outer-input error#))
                                                                       (:inner-input error#)))
                         :actual (list '~'= (:actual error#) (list (list '~actual-fn
                                                                         (:outer-input error#))
                                                                   (:inner-input error#)))})
           (t/do-report {:type :pass :message ~message
                         :expected '~form :actual '~actual-fn})))
       (t/do-report {:type :fail :message ~message
                     :expected '~form :actual (list '~'not '~form)}))))
