(ns homework05.runner
  (:require [clojure.test :as t]
            [clojure.tools.namespace.repl :as ns-tools]
            [hawk.core :as hawk]
            [homework05 :as hw05]))

(defn no-errors-or-failures?
  "Returns true if there have been no errors or failures reported."
  []
  (when-let [{:keys [error fail]} @t/*report-counters*]
    (zero? (+ (or error 0)
              (or fail 0)))))

(def file-and-line
  "Alias for the function in clojure.test/file-and-line."
  @(ns-resolve (find-ns 'clojure.test) 'file-and-line))

(defn do-stop-on-first-report
  "A replacement for clojure.test/do-report that will stop once the first error
  or failure is encountered."
  [m]
  (case (:type m)
    :fail  (when (no-errors-or-failures?)
             (t/report (merge (file-and-line (Throwable.) 1) m)))
    :error (when (no-errors-or-failures?)
             (t/report (merge (file-and-line (:actual m) 0) m)))
    (t/report m)))

(def test-vars
  "The list of the tests that should be run in order."
  [#'hw05/primitives
   #'hw05/lists
   #'hw05/vectors
   #'hw05/maps
   #'hw05/sets
   #'hw05/basic-sequence-operations
   #'homework05/consuming-sequences
   #'homework05/manipulating-sequences
   #'homework05/generating-sequences
   #'homework05/creating-collections-from-sequences
   #'homework05/functions
   #'homework05/branching-control-structures
   #'homework05/map-filter-reduce])

(defn run-tests
  "Runs all of the tests in test-vars, stopping at the first error."
  []
  (with-redefs [t/do-report do-stop-on-first-report]
    (binding [t/*report-counters* (ref t/*initial-report-counters*)]
      (t/do-report {:type :begin-test-ns :ns (the-ns 'homework05)})
      (let [ok (atom true)]
        (doseq [v test-vars]
          (t/test-var v)))
      (t/do-report {:type :end-test-ns :ns (the-ns 'homework05)})
      (t/do-report (assoc @t/*report-counters* :type :summary)))))

(defn watch-callback
  "Called from the watcher, reloads namespaces and reruns tests."
  [_ _]
  (binding [*ns* (find-ns 'homework05.runner)]
    (ns-tools/refresh :after 'homework05.runner/run-tests)))

(defn -main
  "Main entry for the program."
  [& args]
  (run-tests)
  (hawk/watch! [{:paths ["src/homework05.clj"]
                 :handler watch-callback}]))
