 (ns homework05
  "The first homework assignment using Clojure for NAU‘s COMP 3320."
  (:require [clojure.test :refer :all]
            [homework05.util :refer :all]))

; Welcome to your first Clojure program.  The purpose of this assignment is to
; help familiarise yourself with basic Clojure syntax.  This is an example of a
; comment.  In Clojure, everything on a given line of source text starting with
; A semicolon (;) is ignored.


;;;
;;; Part 1: Primitives
;;;
;;; Clojure has a number of primitives.  These are low-level types that
;;; represent simple values.  In this part, you will learn how all of these
;;; primitives can be written in source code.
;;;

(deftest primitives
  (testing "integer literals"
    ; Let‘s start with simple integer literals.  In the following lines,
    ; replace the ‘__’ in each line with an integer literal.
    (is (integer? 1))
    (is (integer? 2))
    (is (integer? 3))
    (is (integer? 4))
    (is (integer? 15))
    (is (integer? -2)))

  (testing "floating point literals"
    ; Now, let's try some floating point literals.
    (is (float? 1.0))
    (is (float? 2.35))
    (is (float? 1.02))
    (is (float? 2.6))
    (is (float? 3.9))
    (is (float? 4.0)))

  (testing "rational literals"
    ; Like Ruby, Clojure supports rational numbers, e.g. 22/7.   Create some
    ; ratios.
    (is (ratio? 22/7))
    (is (ratio? 3/5))
    (is (ratio? 3/8))
    (is (ratio? 8/26))
    (is (ratio? 9/33))
    (is (ratio? 2/61)))

  (testing "character literals"
    ; Characters are preceded by a backslash.  Try creating some characters.
    (is (char? \a))
    (is (char? \c))
    (is (char? \d))
    (is (char? \e))
    (is (char? \y))
    (is (char? \q))

    ; Some characters are special.
    (is (backslash? \\))
    (is (backspace? \backspace))
    (is (newline? \newline))
    (is (return? \return))
    (is (space? \space))
    (is (tab? \tab)))

  (testing "string literals"
    ; Strings are enclosed in double quotes (").  Create some string literals.
    (is (string? "a string"))
    (is (string? "I am clojure"))
    (is (string? "Hello class"))
    (is (string? "planet earth"))
    (is (string? "secret garden"))
    (is (string? "empty class"))

    ; Strings can span multiple lines.  Create one here.
    (is (multiline-string? Hi. I have been drinking water for
                           couple days now.))

    ; You can also escape special characters in Clojure strings as you do in
    ; Java.  Create a string that contains a double quote (") and a backslash
    ; (\)
    (is (string-with-double-quote? ' "Hi. My name is Carlos" '))
    (is (string-with-backslash? "String with backslash \\")))

  (testing "boolean literals"
    ; Clojure also has literals for the two boolean values.
    (is (true? 1))
    (is (false? false)))

  (testing "the null literal"
    ; As in Ruby, 'nil' is the literal for a null value.  This is the same as
    ; Java‘s null.
    (is (nil? nil)))

  (testing "keyword literals"
    ; Like Ruby symbols, keywords are symbolic identifiers that evaluate to
    ; themselves.  Their literal representation starts with a colon (:)
    ; followed by one or more characters.  There are some limitations about
    ; what characters may show up in a keyword name.  To be safe, avoid any
    ; slashes (/), full stops (.), or additional colons.
    (is (keyword? :keyword))
    (is (keyword? :bro))
    (is (keyword? :variable))
    (is (keyword? :chain))
    (is (keyword? :scheme))
    (is (keyword? :turtle)))

  (testing "symbol literals"
    ; Symbols, like keywords, are symbolic identifiers.  However, unlike
    ; keywords, symbols are used as names for other values.  As such, if you
    ; actually need to refer to a symbol itself, it is necessary to prepend it
    ; with a single quote (').  Like keywords, avoid any slashes, full stops,
    ; or colons in the symbol name.
    (is (symbol? 'symbol))
    (is (symbol? 'whatsymbol))
    (is (symbol? 'dream))
    (is (symbol? 'classic))
    (is (symbol? 'scope))
    (is (symbol? 'just))))


;;;
;;; Part 2: Data structures
;;;
;;; In addition to all of the above primitive data types, Clojure also provides
;;; a number of composite data types, many of which have support from the
;;; Clojure reader.  Note that all of Clojure‘s collections are immutable,
;;; making them thread-safe.  Through the use of structural sharing, Clojure‘s
;;; collections are also efficient.
;;;
;;; In this part, you will learn how to write all of these data structures and
;;; learn about some functions that can be used with these data structures.
;;;

(deftest lists
  ; The most common data structure used in Clojure and other Lisps is the
  ; list.  Lists are made up of zero or more elements inside of parentheses.
  (testing "constructing lists"
    ; Lists are used for function invocation where the first element in the
    ; list is a function to invoke and the rest of the elements in the list are
    ; the arguments to that function.  As with symbols, if you want to get the
    ; list itself without having Clojure try to evaluate it, prepend it with a
    ; single quote.
    (is (list? '(1 2.0 6/2)))
    (is (list? '(1 2 9)))
    (is (list? '(8 6.5 1)))
    (is (list? '(1 5 88)))
    (is (list? '(3 4 9/3)))
    (is (list? '(2 5 6)))

    ; In addition to the above list literal syntax, you can also build lists
    ; using the ‘list’ function.
    (is (= '(1 2 3) (list 1 2 3)))
    (is (= '(:a :b) (list 6 8)))
    (is (= '(:a :b :c) (list "one" "two" "three"))))

  (testing "generic operations"
    ; All Clojure collections support a number of basic operations.  The first
    ; of these is ‘count’, which returns the number of elements in the
    ; collection.  In general, all Clojure collections support constant-time
    ; ‘count’.
    (is (= 2 (count '(\a \b))))
    (is (= 0 (count '())))
    (is (= 3 (count '(:a :b :c))))
    (is (= 3 (count __)))
    (is (= 4 (count '(1 2 3 4))))

    ; The second basic operation supported by all operations is ‘conj’, short
    ; for ‘conjoin’.  ‘conj’ appends to a collection in the most natural way
    ; for that collection.  For lists, that means prepending the new value to
    ; the list.
    (is (= '(\a \b \c) (conj '(\b \c) \a)))
    (is (= '(3 1 2) (conj '(1 2) 3)))
    (is (= '(0 1 2) (conj '(1 2) 0)))
    (is (= '(:a 2 3 :b) (conj '(2 3 :b) :a)))
    (is (= '(:a) (conj '() :a)))
    (is (= '(:c :b :a) (conj '(:b :a) :c)))
    (is (= '(1 2 3 4) (conj '(2 3 4) 1)))
    ; not that you can ‘conj’ more than one element in the same call
    (is (= '(1 2 3 4) (conj '(3 4) 2 1)))

    ; Lastly, we have ‘seq’, which returns a sequence of the items in the
    ; collection.  Sequences are a very important abstraction that we will get
    ; into later.  In the case of lists, ‘seq’ returns the list itself except
    ; in one important case: the empty list.
    (is (= '(1 2 3) (seq '(1 2 3))))
    (is (nil? (seq '())))
    (is (= (:a :b :c) (seq '(:a :b :c))))
    (is (= nil (seq '())))))

(deftest vectors
  ; In addition to lists, Clojure also supports vectors.  Like lists, vectors
  ; are sequential collections of values.  However, they behave much more like
  ; dynamic arrays, with relatively efficient access to elements by index.
  (testing "constructing vectors"
    ; In Clojure source code, vectors are constructed by square braces with zero
    ; or more forms inside.
    (is (vector? [1 2 3]))
    (is (vector? [5 6 9]))
    (is (vector? [0 1 3]))
    (is (vector? [12 57 99]))
    (is (vector? [0 3 5 3]))
    (is (vector? [8 1 5]))

    ; Similarly to lists, you can build vectors using the ‘vector’ function.
    (is (= [1 2 3] (vector 1 2 3)))
    (is (= [\a \b \c] (vector \a \b \c)))
    (is (= ['a 'b 'c] (vector 'a 'b 'c))))

  (testing "generic operations"
    ; Vectors support the ‘count’, ‘conj’, and ‘seq’ functions, like all
    ; collections.
    (is (= 3 (count ["a" "b" "c"])))
    (is (= 7 (count [\c \l \o \j \u \r \e])))
    (is (= 0 (count __)))

    ; In the case of vectors, ‘conj’ appends to the end of the vector.
    (is (= [1 2 3] (conj [1 2] 3)))
    (is (= [1 2] (conj [1] 2)))
    (is (= [2] (conj [] 2)))
    (is (= ['a \b :c 4] (conj ['a \b :c] 4)))
    (is (= [1 2 3 4 5] (conj [1 2] 3 4 5)))

    ; Finally, using ‘seq’ on a vector produces something that is no longer a
    ; vector.  Nor is the result a list, but it is a sequence nonetheless.
    ; Type some vectors into the blanks.
    (is (not (vector? (seq [1 2 3]))))
    (is (not (list? (seq [:a :c :x]))))
    (is (seq? (seq [1 2 6])))
    ; What happens if you try to ‘seq’ an empty vector?
    (is (nil? (seq []))))

  (testing "equivalence of vectors with lists and sequences"
    ; Two sequential lists are considered equivalent if they have elements in
    ; the same order.  This means that Clojure‘s equivalence checks on
    ; collections do not depend on the type of the collection.
    (is (= '(1 2 3) [1 2 3] (seq [1 2 3])))

    ; However, order of the data is important.
    (is (not= [1 2 3] [3 1 2]))

    ; Create lists equal to these vectors and vice-versa
    (is (= '(:a :b :c) [:a :b :c]))
    (is (= '() []))
    (is (= '(\a "a" :a) [\a "a" :a]))
    (is (= '(5 4 3 2 1) [5 4 3 2 1])))

  (testing "vectors as indexed collections"
    ; Due to the nature of vectors, they support some extra vector-specific
    ; operations.  The first of these is ‘nth’, which returns the value at the
    ; given index.
    (is (= 3 (nth [0 1 2 3 4 5] 3)))
    (is (= :c (nth [:a :b :c :d :e] 2)))
    (is (= \3 (nth [\1 \2 \3 \4] 2)))

    ; Additionally, it's possible to supply a ‘not-found’ value.   What happens
    ; when trying to access an out-of-bounds value with ‘nth’?
    (is (= :not-found (nth [] 3 :not-found)))
    (is (= :not-found (nth [1 2 3] 3 :not-found)))
    (is (= 4 (nth [1 2 3 4] 3 :not-found)))
    (is (= nil (nth [:a :b :c] -1 nil)))
    (is (= 5 (nth [1 2 3] 42 5))))

  (testing "vectors as associative collections"
    ; In Clojure, some collections are said to be associative, meaning that
    ; given a key, the collection can look up an associated value.  Vectors are
    ; associative by index.
    ;
    ; The primary way to get a value within an associative collection is ‘get’.
    ; You can pass a collection and a key to ‘get’ to retrieve the value for
    ; that key.  You can also pass a ‘not-found’ value.
    (is (= 3 (get [0 1 2 3] 3)))
    (is (= :not-found (get [0 1 2 3] 6 :not-found)))
    (is (= :c (get [:a :b :c] 2)))
    (is (= :foo (get [:a :b :c] 4 :foo)))
    ; note how ‘get’ differs from ‘nth’ when the index is out of bounds
    (is (= nil (get [:a :b :c] 3))))

  (testing "vectors as functions"
    ; One last bit before we move on from vectors: they also act as functions.
    ; You can place a vector in function call position and pass it an index to
    ; get the value at that index.
    (is (= 2 ([0 1 2 3 4 5] 2)))
    (is (= \b ([\a \b \b \c \d] 2)))
    (is (= :c ([:a :b :b :c :d] 3)))))

(deftest maps
  ; Maps are another associative collection, similar to Ruby‘s hashes or
  ; Python‘s dictionaries.  These are collections with arbitrary keys mapped to
  ; arbitrary values.
  (testing "constructing maps"
    ; In Clojure source code, maps are constructed by curly braces with an even
    ; number of forms inside.  If you like, you can use commas (,) to separate
    ; key/value pairs, but those are not necessary.
    (is (map? {:a 1 :b 2 :c 3}))
    (is (map? {:a 1 :b 7 :c 23}))
    (is (map? {:a 1 :b 2 :c 8}))
    (is (map? {:a 1 :b 5 :c 2}))
    (is (map? {:a 2 :b 1 :c 2}))
    (is (map? {:a 4 :b 3 :c 6}))

    ; Similarly to lists and vectors, you can build maps using the ‘hash-map’ function.
    (is (= {:a 1 :b 2 :c 3} (hash-map :a 1 :b 2 :c 3)))
    (is (= {\a 1, \b 2, \c 3} (hash-map \a 1 \b 2 \c 3)))
    (is (= {'a :a :b 'b} (hash-map 'a :a :b 'b)))

    ; Note that you can also get sorted maps using the ‘sorted-map’ function.
    ; They are not as efficient as hash maps, but are sorted.
    (is (= {"a" 1, "b" 2} (sorted-map "a" 1 "b" 2)))
    (is (= {\a \b, \c \d} (sorted-map \a \b \c \d)))
    (is (= {1 "one", 2 "two"} (sorted-map 1 "one" 2 "two")))
    (is (= {} (sorted-map))))

  (testing "generic operations"
    ; Maps support the ‘count’, ‘conj’, and ‘seq’ functions, like all
    ; collections.  Note that the count returns the number of entries in the
    ; map.
    (is (= 3 (count {1 \a 2 \b 3 \b})))
    (is (= 2 (count {:x 2 :y 3})))
    (is (= 0 (count {})))

    ; With hash maps, ‘conj’ appends in some implementation-defined way.  Also
    ; note that what you add to a map is a key-value pair.
    (is (= {:c 3 :a 1 :b 2 } (conj {:a 1 :b 2} [:c 3])))
    (is (= {:x 2 :y 3} (conj {:y 3} [:x 2])))
    (is (= {:foo :bar} (conj {} [:foo :bar])))
    (is (= {:key3 "three", :key2 "two", :key1 "one"} (conj {:key1 "one" :key2 "two"} [:key3 "three"])))
    (is (= {:d \d, :c \c, :b \b, :a \a} (conj {:a \a :b \b} [:c \c] [:d \d])))
    ; what happens when you try add an item with the same key to a map?
    (is (= {:b 2, :a 3} (conj {:a 1 :b 2} [:a 3])))

    ; As expected, ‘seq’ returns a sequence of key-value pairs, but the order
    ; is implementation-defined.
    (is (= '([:c 3] [:b 2] [:a 1]) (seq {:a 1 :b 2 :c 3})))
    ; What should the results be of calling ‘seq’ on the following?
    (is (= ([:key :value]) (seq {:key :value})))
    (is (= nil (seq {})))

    ; It should be noted that sorted maps have predictable orderings.
    (is (= ([:a 1] [:b 2] [:c 3]) (seq (sorted-map :b 2 :a 1 :c 3))))

    ; It is possible to query if a map is sorted or not
    (is (= false (sorted? #{:a 1 :b2})))
    (is (= false (sorted? (hash-map :a 1 :b 2))))
    (is (= true (sorted? (sorted-map :a 1 :b 2)))))

  (testing "merging maps"
    ; In addition to using ‘conj’ to add entries to maps, you can also ‘merge’
    ; maps together, where later maps override values in earlier maps.
    (is (= {:a 1 :b 2 :c 3} (merge {:a 1 :b 1 :c 1}
                                   {:b 2}
                                   {:c 3})))
    ; Now you give it a try…
    (is (= {:a 1} (merge {} {:a 1})))
    (is (= {:a 1} (merge {:a 1} {})))
    (is (= {:a 1} (merge {:a 2} {:a 1}))))

  (testing "maps as associative collections"
    ; As with vectors, maps are associative collections.  Of course, they are
    ; much more interesting associative collections than vectors.  As with
    ; vectors, you can use ‘get’ elements from a map.
    (is (= 3 (get {:a 1 :b 2 :c 3} :c)))
    (is (= :not-found (get {:a 1 :b 2} :c :not-found)))

    (is (= 2 (get {:a 1 :b 2 :c 3} :b)))
    (is (= :foo (get {:a 1 :b 2 :c 3} :w :foo)))
    ; What do you get when you when you try to get a value that is not in a map
    ; and you do not provide a ‘not-found’ argument
    (is (= nil (get {:a 1 :b 2 :c 3} :d)))

    ; Note that it is OK to have nil as a key or a value
    (is (= :a-val (get {nil :a-val} nil)))
    (is (= nil (get {:key nil} :key :not-found)))

    ; Beyond ‘get’, you can also check to see if a map contains a particular
    ; key using ‘contains?’.
    (is (contains? {:a 1 :b 2} :a))
    (is (not (contains? {:a 1 :b 2} :c)))

    (is (= true (contains? {1 :a 2 :b :c 3} :a)))
    (is (= false (contains? {1 :a 2 :b :c 3} :d)))

    ; A common mistake is try to use ‘contains?’ on non-associative
    ; collections.  It works on vectors, but not in the way you intuitively
    ; expect.
    (is (= false (contains? [5 4 3] 3)))
    (is (= true (contains? [5 4 3] 1)))

    ; In addition to ‘conj’, you can ‘assoc’ (short for associate) values into
    ; maps.  This is a more natural way to do it.  You can also do more than
    ; one value at a time.
    (is (= {:a 1 :b 2} (assoc {:a 1} :b 2)))
    (is (= {:a 1 :b 2} (assoc {} :a 1 :b 2)))

    (is (= {:a "a" :b 2} (assoc {:a "a"} :b 2)))
    (is (= {:a "a" :b "b"} (assoc {:a "a"} :b "b")))
    (is (= {:key :value} (assoc {} :key :value)))
    (is (= {:a \a :b \b} (assoc {:a 1 :b 2} :a \a :b \b)))

    ; The reverse operation to ‘assoc’ is ‘dissoc’ (short for dissociate).
    (is (= {:b 2} (dissoc {:a 1 :b 2} :a)))
    (is (= {} (dissoc {:a 1 :b 2} :a :b)))
    ; note that dissociating a key that is not there is a no-op
    (is (= {:a 1 :b 2} (dissoc {:a 1 :b 2} :c)))

    (is (= {\b 2 \c 3} (dissoc {\a 1 \b 2 \c 3} \a)))
    (is (= {\a 1 \b 2 \c 3} (dissoc {\a 1 \b 2 \c 3} \d)))
    (is (= {:key1 1 :key3 3} (dissoc {:key1 1 :key3 3} :key2)))
    (is (= {:1 1 :3 3} (dissoc {:1 1 :2 2 :3 3 :4 4 :5 5} :2 2 :4 4 :5 5)))

    ; Finally, you can use ‘keys’ and ‘vals’ to get the keys and values of
    ; the map, respectively.
    (is (= [:a :b :c] (keys (sorted-map :a 1 :b 2 :c 3))))
    (is (= [1 2 3] (vals (sorted-map :a 1 :b 2 :c 3))))

    (is (= [\1 \2 \3] (vals (sorted-map :a \1 :b \2 :c \3))))
    (is (= ["one" "three" "two"] (keys (sorted-map "one" 1 "three" 2 "two" 3)))))

  (testing "maps as functions"
    ; As with vectors, maps can also behave like functions.  They are functions
    ; of their keys and also accept a not-found value.
    (is (= 2 ({:a 1 :b 2 :c 3} :b)))
    (is (= nil ({:a 1 :b 2 :c 3} :d)))
    (is (= :not-found ({:a 1 :b 2 :c 3} :d :not-found)))

    (is (= \b ({"one" \a "two" \b "three" \c} "two")))
    (is (= nil ({"one" \a "two" \b "three" \c} "four")))
    (is (= \d ({"one" \a "two" \b "three" \c} "four" \d)))
    (is (= :four ({"four" :four } "four")))
    (is (= :woo! ({:a :woo!} :a :whee!)))
    (is (= :whee! ({} {} :whee!)))))

(deftest sets
  ; The last data type we will cover are sets, which are collections of unique
  ; values.  As with maps, there are both hashed and sorted variants.
  (testing "constructing sets"
    ; The reader notation for creating a set is ‘#{}’.
    (is (set? #{:a :b :c}))
    (is (set? #{:e :f :g}))
    (is (set? #{:t :x :y}))
    (is (set? #{:a :u :v}))
    (is (set? #{:g :s :q}))
    (is (set? #{:s :d :w}))

    ; You can also use the ‘hash-set’ function
    (is (= #{:a :b :c} (hash-set :a :b :c)))
    (is (= #{} (hash-set)))
    (is (= #{1 2 3 4 5} (hash-set 1 2 3 4 5)))

    ; To create a sorted set, use the ‘sorted-set’ function
    (is (= #{"a" "b" "c" "d"} (sorted-set "a" "b" "c" "d")))
    (is (= #{\a \b \c \d} (sorted-set \a \b \c \d)))
    (is (= #{:a :b :c} (sorted-set :a :b :c)))

    ; Again, you can query to see if a particular set is sorted or not.
    (is (= false (sorted? #{1 2 3})))
    (is (= false (sorted? (hash-set 1 2 3))))
    (is (= true (sorted? (sorted-set 1 2 3))))

    ; Finally, you can create sets from other collections using the ‘set’
    ; function.
    (is (= #{1 2} (set [1 2 2 1]) (set '(1 1 2 2))))
    (is (= #{[:a 1] [:b 2]} (set {:a 1 :b 2})))
    (is (= #{} (set '())))
    (is (= #{\a \b \c \d} (set [\a \b \c \d])))
    (is (= #{\a \g \i \m \n \o \p \r} (set [\p \r \o \g \r \a \m \m \i \n \g])))
    (is (= #{[1 \a] [2 \b] [3 \c]} (set {1 \a 2\b 3 \c}))))

  (testing "generic operations"
    ; Sets support the ‘count’, ‘conj’, and ‘seq’ functions, like all
    ; collections.
    (is (= 3 (count #{1 2 3})))
    (is (= 2 (count #{:x :y})))
    (is (= 0 (count #{})))




    ; With hash maps, ‘conj’ appends in some implementation-defined way.
    ; Naturally, you cannot add non-unique values.
    (is (= #{:a :b :c} (conj #{:a :b} :c :b)))

    (is (= #{:x :y} (conj #{:y} :x)))
    (is (= #{1 2 3} (conj #{} 1 2 3 2 1)))
    (is (= #{"a" "b" "c"} (conj #{"a" "b"} "c")))
    (is (= #{4 5 6} (conj #{4} 5 6)))

    ; As expected, the order of the elements returned from ‘seq’ returns is
    ; implementation-defined.
    (is (= '(:c :b :a) (seq #{:a :b :c})))
    ; What should the results be of calling ‘seq’ on the following?
    (is (= (true) (seq #{true})))
    (is (= nil (seq #{})))

    ; It should be noted that sorted sets have predictable orderings.
    (is (= (1 2 3 4 5) (seq (sorted-set 5 4 3 2 1)))))

  (testing "set functions"
    ; To remove an element from a set, use ‘disj’ (short for disjoin).  You can
    ; remove more than one element at a time.
    (is (= #{:a :b :c} (disj #{:a :b :c :d} :d)))
    (is (= #{:a :b} (disj #{:a :b :c :d} :d :c)))
    (is (= #{:a :b :c} (disj #{:a :b :c} :d)))
    ; Now you give it a try…
    (is (= #{1 2 3} (disj #{1 2 3 4} 4)))
    (is (= #{1 2 3} (disj #{1 2 3 4 5} 4 5)))
    (is (= #{1 2 3} (disj #{1 2 3 4 5} 4 5 6)))
    (is (= #{:a :d} (disj #{:a :d} :b :c :e)))
    (is (= #{\a \b \d} (disj #{\a \b \c \d} \c)))

    ; While not associative, you can still use ‘get’ and ‘contains?’ with sets.
    ; In the case of ‘get’, it will return the value that was equal to that
    ; value.
    (is (= :a (get #{:a :b :c} :a)))
    (is (= nil (get #{:a :b :c} :d)))
    (is (= :not-found (get #{:a :b :c} :d :not-found)))

    (is (= 3 (get #{1 2 3 4} 3)))
    (is (= nil (get #{1 2 3 4} 5)))
    (is (= :not-found (get #{1 2 3 4} 5 :not-found)))
    (is (= nil (get #{1 2 3 4} 8)))

    ; Sets also work with ‘contains?’, returning true or false if the element
    ; is in the set
    (is (= true (contains? #{:a :b :c} :a)))
    (is (= false (contains? #{:a :b :c} :d))))

  (testing "sets as functions"
    ; As with vectors and maps, sets also behave like functions.  They are functions
    ; of their members as with ‘get’, but do not accept a not-found argument.
    (is (= :a (#{:a :b :c} :a)))
    (is (= nil (#{:a :b :c} :d)))

    (is (= 3 (#{1 2 3 4} 3)))
    (is (= nil (#{1 2 3 4} 5)))
    (is (= nil (#{1 2 3} 4)))
    (is (= 2 (#{1 2 3 4} 2)))))


;;;
;;; Part 3: Sequences
;;;
;;; One of the most important abstractions in Clojure is the sequence.  All of
;;; the Clojure collections can be treated as sequences, as can some native
;;; Java types, such as strings, arrays, or Iterables.
;;;
;;; Note that many of these sequence operations are lazy, meaning that they do
;;; not do any actual work until you try to use the result.
;;;

(deftest basic-sequence-operations
  (testing "more on seq"
    ; We have already seen that we can use ‘seq’ on all of the Clojure
    ; collections, but what about other data types?

    ; Strings are sequences of characters
    (is (= '(\c \l \o \j \u \r \e) (seq "clojure")))

    (is (= (\h \e \l \l \o) (seq "hello")))
    (is (= '(\g \o \o \d \b \y \e) (seq "goodbye")))

    ; java-list here is an instance of a java.util.ArrayList containing the
    ; numbers 1, 2, and 3
    (is (= true (instance? java.util.ArrayList java-list)))
    (is (= false (list? java-list)))

    (is (= '(1 2 3) (seq java-list)))

    ; java-array is an array of Long objects containing the numbers 1, 2, and 3
    (is (= (Class/forName "[Ljava.lang.Long;") (type java-array)))
    (is (= false (list? java-array)))
    (is (= (1 2 3) (seq java-array))))

  (testing "first"
    ; The first basic sequence function is ‘first’, which returns the first
    ; element in its argument.  This function will coerce its argument into a
    ; sequence.
    (is (= :a (first [:a :b :c])))

    (is (= 1 (first '(1 2 3))))
    (is (= \c (first [\c \b \a])))
    (is (= [:a 1] (first (sorted-map :b 2 :a 1 :c 3))))
    (is (= "a" (first (sorted-set "c" "b" "a"))))
    (is (= \x (first "xyz")))
    (is (= 1 (first java-list)))
    (is (= 1 (first java-array)))

    ; what happens when the argument is empty?
    (is (= nil (first [])))
    (is (= nil (first {})))
    (is (= nil (first '())))

    ; what about if the argument is nil?
    (is (= nil (first nil))))

  (testing "rest"
    ; The second basic sequence function is ‘rest’, which returns the rest of
    ; the items in the sequence after the first.  Again, this function will
    ; coerce its argument into a sequence.
    (is (= '(:b :c) (rest [:a :b :c])))

    (is (= (2 3) (rest '(1 2 3))))
    (is (= \b \a (rest [\c \b \a])))
    (is (= ([:b 2] [:c 3]) (rest (sorted-map :b 2 :a 1 :c 3))))
    (is (= ("b" "c") (rest (sorted-set "c" "b" "a"))))
    (is (= (\y \z) (rest "xyz")))
    (is (= (2 3 ) (rest java-list)))
    (is (= (2 3) (rest java-array)))

    ; what happens when the argument is empty?
    (is (= () (rest [])))
    (is (= () (rest {})))
    (is (= () (rest '())))

    ; what about if the argument is nil?
    (is (= () (rest nil))))

  (testing "cons"
    ; The last of the basic sequence functions is ‘cons’, which returns a new
    ; sequence where the first argument is the first element in the new
    ; sequence and the second argument is the rest of the new sequence.  Note
    ; that this does not preserve the type argument.
    (is (= '(:a :b :c) (cons :a [:b :c])))

    (is (= (3 1 2) (cons 3 '(1 2))))
    (is (= (3 1 2) (cons 3 [1 2])))
    (is (= (3 [:a 1] [:b 2] [:c 3]) (cons 3 (sorted-map :a 1 :b 2 :c 3))))
    (is (= (3 1 2 3) (cons 3 (sorted-set 1 2 3))))
    (is (= (3 \a \b \c) (cons 3 "abc")))
    (is (= (3 1 2 3) (cons 3 java-list)))
    (is (= (3 1 2 3) (cons 3 java-array)))

    ; what happens with the first argument is empty or nil?
    (is (= ([] 1 2 3) (cons [] [1 2 3])))
    (is (= (nil 1 2 3) (cons nil [1 2 3])))

    ; what happens with the second argument is empty or nil?
    (is (= (1) (cons 1 [])))
    (is (= (1) (cons 1 nil)))))

(deftest consuming-sequences
  ; While ‘first’, ‘rest’, and ‘cons’ are nice, there are many functions that
  ; you can use to consume a list.  Let’s take a look at a few functions for
  ; consuming sequences.

  (testing "take"
    ; First, consider ‘take’, which returns the first n items from a sequence
    (is (= '(1 2 3) (take 3 [1 2 3 4 5])))
    (is (= '(1) (take 3 [1])))

    (is (= (1 2) (take 2 '(1 2 3 4))))
    (is (= () (take 0 '(1 2 3 4))))
    (is (= '(:a :b :c) (take 3 [:a :b :c :d])))
    (is (= '(\a \b) (take 2 (sorted-set \c \b \d \a)))))

  (testing "drop"
    ; Now, consider ‘drop’, which returns all but the first n items from a
    ; sequence
    (is (= '(4 5) (drop 3 [1 2 3 4 5])))
    (is (= '() (drop 3 [1])))

    (is (= (3 4) (drop 2 '(1 2 3 4))))
    (is (= (1 2 3 4) (drop 0 '(1 2 3 4))))
    (is (= '(2 3 5) (drop 3 '(0 1 4 2 3 5))))
    (is (= '(\d) (drop 3 (sorted-set \c \b \d \a)))))

  (testing "nth"
    ; It is also possible to ‘nth’ for arbitrary sequences, though it could
    ; operate in linear time
    (is (= :c (nth '(:a :b :c :d) 2)))
    (is (= [:d 4] (nth (seq (sorted-map :a 1 :b 2 :c 3 :d 4)) 3)))
    (is (= [:a 1] (nth (seq (sorted-map :a 1 :b 2 :c 3 :d 4)) 0)))
    (is (= \j (nth "Clojure" 3))))

  (testing "rand-nth"
    ; Sometimes, you just want a random value from a sequence, and ‘rand-nth’
    ; fits the need.  Unfortunately, it’s hard to test for random behaviour, so
    ; you should try out this function on the REPL.
    (is (integer? (rand-nth '(1 2 3 4 5))))
    (is (symbol? (rand-nth ['a 'b 'c 'd 'e])))
    (is (keyword? (rand-nth [:a :b :c :d])))
    (is (string? (rand-nth ["cd" "css" "se"])))))

(deftest manipulating-sequences
  ; Additionally, there are functions that can manipulate sequences.
  (testing "distinct"
    ; Sometimes, you want to remove duplicates from an arbitrary sequece.  For
    ; this, you can use ‘distinct’.
    (is (= '(1 3 4 2 5) (distinct '(1 1 3 4 1 2 3 5 2))))
    (is (= (\A \b \a \c \d \r \!) (distinct "Abacadabra!")))
    (is (= (4 5 2 6 7 8 1 9) (distinct [4 5 2 6 2 4 7 8 1 2 9 2 5 8 2 7 6 9 2])))
    (is (= () (distinct [])))
    (is (= '(3 1 2) (distinct '(3 1 1 2 3 2)))))

  (testing "sort"
    ; It is also possible to ‘sort’ the values of a sequence.  This relies on
    ; the natural ordering of the contents of the sequence.
    (is (= '(1 2 3 4 5) (sort [5 4 3 2 1])))
    (is (=  (\c \e \j \l \o \r \u) (sort "clojure")))
    (is (= (\a \a \a \a \a \b \b \c \d \r) (sort "abacadabra")))
    (is (= (b l m o s s y) (sort ['s 'y 'm 'b 'o 'l 's])))
    (is (= () (sort []))))


  ; Stopped here

  (testing "reverse"
    ; You can get the elements in a sequence in ‘reverse’ order.
    (is (= '(5 4 3 2 1) (reverse [1 2 3 4 5])))
    (is (= __ (reverse '(\a \b \c \d \e))))
    (is (= __ (reverse (sorted-map :a 1 :b 2 :c 3))))
    (is (= '(:c :b :a) (reverse __))))

  (testing "flatten"
    ; If an input sequence has nested sequences, it is possible to ‘flatten’ them.
    (is (= '(1 2 3 4 5) (flatten [1 [2 3] '([[4] 5])])))
    (is (= __ (flatten '(1 2 3 4))))
    (is (= __ (flatten '(1 [2 3] 4))))
    (is (= __ (flatten '([([])]))))
    (is (= __ (flatten nil))))

  (testing "shuffle"
    ; You can get a random reordering of a sequence by using ‘shuffle’.  Note
    ; that ‘shuffle’ always returns a vector.  Again, it‘s hard to test
    ; shuffle, so please try it out in the REPL.
    (is (vector? (shuffle [1 2 3 4 5])))
    (is (vector? (shuffle '(1 2 3 4 5))))))

(deftest generating-sequences
  ; Additionally, it is possible to generate larger, possibly infinite,
  ; sequences.  Here are a few handy functions that can do that.

  (testing "concat"
    ; Sometimes you wnat to merge sequences together, one after another, making
    ; a single sequence.  ‘concat’ is the way you do that.
    (is (= '(1 2 3 4 5 6) (concat [1 2 3] [4 5 6])))
    (is (= '(1 2 3 4 5 6) (concat [1 2] [3] [4] [5 6])))
    (is (= __ (concat '(1) [2] [3])))
    (is (= __ (concat '(1) nil [2] '() [3])))
    (is (= __ (concat))))

  (testing "interleave"
    ; It is also possible to mix sequences together.  Take ‘interleave’ as an
    ; example.  It will give you the first item from each collection, followed
    ; by the second item in each collection, etc.
    (is (= '(:a \a a :b \b b :c \c c) (interleave [:a :b :c]
                                                  [\a \b \c]
                                                  ['a 'b 'c])))
    (is (= __ (interleave (sorted-map :a 1 :b 2 :c 3))))
    (is (= __ (interleave (sorted-map :a 1 :b 2 :c 3)
                          (sorted-set 4 5 6))))
    (is (= __ (interleave [1 2 3]
                          "hello")))
    (is (= __ (interleave [] ""))))

  (testing "interpose"
    ; Sometimes, it is useful to create a new sequence by inserting a new item
    ; in between each item in an input sequence.  ‘interpose’ accomplishes this
    ; task.
    (is (= '(:a 1 :b 1 :c) (interpose 1 [:a :b :c])))
    (is (= __ (interpose \, "abcd")))
    (is (= __ (interpose :foo [])))
    (is (= __ (interpose nil [1 2 3])))
    (is (= __ (interpose [1 2 3] nil))))

  (testing "cycle"
    ; At other times, it is quite useful to generate an infinite sequcence from
    ; a finite sequence.  ‘cycle’ does this by repeating the contents of the list
    ; over and over again.  Note that we use ‘take’ because otherwise we would
    ; be tyring to get every item from an infinite sequence.
    (is (= '(1 2 3 1 2 3 1 2 3 1) (take 10 (cycle [1 2 3]))))
    (is (= __ (take 5 (cycle (sorted-set :a :b)))))
    (is (= __ (take 0 (cycle (sorted-set :a :b)))))
    (is (= __ (take 2 (cycle (sorted-map :a 1 :b 2 :c 3 :d 4)))))
    (is (= __ (take 6 (cycle "abc")))))

  (testing "range"
    ; Sometimes you need to ‘create’ a range of integers.  The no-argument
    ; version generates an infinite sequence of integers starting from zero.
    (is (= '(0 1 2 3 4) (take 5 (range))))
    (is (= __ (take 3 (drop 2 (range)))))

    ; The one-argument version defines the end of the sequence.
    (is (= '(0 1 2 3 4) (range 5)))
    (is (= '(0 1 2) (range __)))
    (is (= __ (range 10)))
    (is (= __ (range 0)))
    (is (= __ (range -2)))

    ; The two-argument version defines both a start and an end.
    (is (= '(1 2 3) (range 1 4)))
    (is (= __ (range 5 10)))
    (is (= '(10 11 12) (range __ __)))
    (is (= __ (range 10 0)))

    ; The three-argument version defines a start, a stop, and a step
    (is (= '(2 4 6) (range 2 7 2)))
    (is (= __ (range 0 10 3)))
    (is (= __ (range 0 10 30)))
    (is (= __ (range 10 0 30)))
    (is (= __ (range 10 0 -1)))
    (is (= '(15 10 5 0) (range __ __ __))))

  (testing "repeat"
    ; Occasionally, you will just need a sequence of the same constant over and
    ; over again.  ‘repeat’ is helpful here.  The no-argument version returns
    ; an infinite sequence.
    (is (= '(1 1 1 1) (take 4 (repeat 1))))
    (is (= __ (take 3 (repeat []))))

    ; There is a one-argument version that returns a finite sequence.
    (is (= '(\a \a \a) (repeat 3 \a)))
    (is (= __ (repeat 4 :foo)))
    (is (= '("a" "a" "a" "a") (repeat __ __)))))

(deftest creating-collections-from-sequences
  ; There are a number of ways you can consume a sequence to create a
  ; collection.  We will discuss two here.
  (testing "into"
    ; Sometimes, you want to take a sequence and put it into a given type of
    ; collection.  For this, you can use ‘into’.  Note that this function uses
    ; ‘conj’ for adding elements to a collections.
    (is (= [1 2 3] (into [] '(1 2 3))))
    (is (= __ (into '() [1 2 3])))
    (is (= __ (into '(:a :b) [])))
    (is (= __ (into (sorted-set :a :b) [:b :a :c])))
    (is (= __ (into {} [[:a 1] [:b 2]]))))

  (testing "zipmap"
    ; Finally, sometimes you have two sequences that you want to combine into a
    ; map. ‘zipmap’ is great way to do this.
    (is (= {:a 1 :b 2} (zipmap [:a :b] '(1 2))))
    (is (= __ (zipmap [1 2 3] [3 2 1])))
    (is (= __ (zipmap [] [:a :b])))
    (is (= __ (zipmap [1 2 3] [:a :b])))
    (is (= {:list '() :map {} :vector [] :set #{}} (zipmap __ __)))))

;;;
;;; Part 4: Functions and control structures
;;;
;;; In the previous part we saw some ways that sequences can be used.  However,
;;; there is a lot more that we can do.  However, in order to introduce these
;;; additional functions, we need to learn about two new concepts:
;;;
;;; 1. Defining and using functions.
;;; 2. Branching flow control
;;;

(deftest functions
  ; What is a functional language without functions?  In this section, you
  ; will learn to create anonymous functions.

  (testing "anonymous functions"
    ; You can create a function by invoking ‘fn’.  The first argument to ‘fn’ is
    ; an argument list, which must be a vector of symbols.  The remaining
    ; arguments are the body of the function where the last value is return value
    ; for the new function.  If left out, the functin will return nil.
    ;
    ; A such, the simplest possible function is (fn [])
    (is (function? (fn [])))
    ; We can invoke that function by wrapping in a set of parentheses.
    (is (nil? ((fn []))))
    ; However, a slightly more interesting function returns a value.
    (is (= "Hello" ((fn [] "Hello"))))

    ; Write some functions that return particular values
    (is (= 42 (__)))
    (is (= :foo (__)))
    (is (= [] (__)))

    ; That is all well and good, but what about functions that take arguments?
    ; The following function returns its first argument.
    (is (= 42 ((fn [arg] arg)
               42)))
    (is (= :a ((fn [arg] arg)
               :a)))

    ; Of course, you can do something in the body of a function.  The following
    ; returns the first item of its first argument.
    (is (= 1 ((fn [c] (first c))
              [1 2 3])))

    ; Write a function that adds 5 to its argument.
    (is (adds-5-function? __))

    ; Write a function that returns the third element in a sequence or nil if
    ; there is no such element
    (is (third-element-function? __))

    ; A function can take multiple arguments
    (is (= 36 ((fn [a b] (* a b)) 4 9)))

    ; Create function that takes two arguments an returns their sum
    (is (sums-two-args-function? __))

    ; How about a function that tkaes three arguments and returns the second one?
    (is (second-of-three-function? __)))

  (testing "functions as arguments"
    ; Additionally, being a functional language, we can take functions as
    ; arguments.  For example, the following function takes a function that
    ; prodduces an integer as an argument.  It will invoke the argument
    ; function and increment the result.  This is a higher-order function.
    (is (= 3 ((fn [input-fn]
                ; invoke the function and add one
                (+ (input-fn) 1))
              (fn [] 2)))) ; a function that returns 2

    ; Now create a function that takes a function (which produces a sequence)
    ; as an argument.  Your function should invoke that function and return and
    ; return the first element from the returned sequence.
    (is (higher-order-first-function? __))

    ; How about a function that takes a function that produces a number.   Upon
    ; invocation, your function will generate that integer and multiply by
    ; five.
    (is (higher-order-multiply-by-5-function? __)))

  (testing "functions as return values"
    ; Naturally, it is also possible to return functions as values.  For
    ; example, the following function returns a function that alwyas produces
    ; the argument to the outer function.  In addition to being an example of
    ; higher-order function, it also shows that the inner function closes over
    ; the argument to the inner function.
    (is (= 42 (((fn [x]
                  (fn [] x))))))

    ; Now, create a function that takes a number as an argument.  It should
    ; return a function that takes an input and multiplies that imput by the by
    ; the number that was passed into the outer function.
    ;
    ; Example:
    ;  (= 10 ((f 5) 2))
    (is (multiplier-function? __))

    ; For the last higher-order function, create a function that takes a
    ; sequence as an argument.  This outer function returns a function that
    ; takes a number n as an argument.  The returned function should return the
    ; first n elements of the sequence.
    ;
    ; Example:
    ;  (= '(1 2) ((f [1 2 3 4) 2))
    (is (first-n-function? __))))


(deftest branching-control-structures
  ; Every language has control structures, and Clojure is no different.

  (testing "branching with if"
    ; First up is ‘if’.  It works like you would expect it to.
    (is (= __ (if true :then :else)))
    (is (= __ (if false :then :else)))

    ; Additionally, you can leave out the else form.
    (is (= __ (if true :then)))
    (is (= __ (if false :then))))

  ; It is very important to understand what Clojure considers logically false.
  ; There are two logical false values in Clojure, false and nil.
  (testing "on logical truth"
    (is (= __ (if nil :then :else)))
    (is (= __ (if "string" :then :else)))
    (is (= __ (if 'symbol :then :else)))
    (is (= __ (if [] :then :else))))

  (testing "branching with cond"
    ; A Lisp classic is ‘cond’, which takes an even number of forms.  The first
    ; form in each pair is a test.  If that test evaluates to logical true,
    ; then it evaluates the second form of that pair and returns that value.
    ; Otherwise, it continues on to the next pair until there are no pairs
    ; left.
    (is (= :one (cond
                  true :one
                  true :two)))
    (is (= :two (cond
                  false :one
                  true :two)))
    (is (= nil (cond
                 false :one
                 false :two)))
    (is (= __ (cond
                (zero? 1) :zero
                (pos? 1) :postive
                :else :negative)))
    (is (= __ (cond
                (zero? 0) :zero
                (pos? 0) :postive
                :else :negative)))
    (is (= __ (cond
                (zero? -1) :zero
                (pos? -1) :postive
                :else :negative))))

  (testing "logical and"
    ; Clojure has a short-circuiting logical ‘and’ that returns the last
    ; logical true or false value.
    (is (= :b (and :a :b)))
    (is (= nil (and :a nil)))
    (is (= __ (and :a false)))
    (is (= __ (and nil :a)))
    (is (= __ (and false :a)))
    (is (= __ (and :a)))
    (is (= __ (and nil)))
    (is (= __ (and))))

  (testing "logical or"
    ; Clojure has a short-circuiting logical ‘or’ that returns the first
    ; logical true value.
    (is (= :a (or :a :b)))
    (is (= :a (or :a nil)))
    (is (= __ (or :a false)))
    (is (= __ (or nil :a)))
    (is (= __ (or false :a)))
    (is (= __ (or :a)))
    (is (= __ (or false)))
    (is (= __ (or))))

  (testing "logical not"
    ; Clojure has a logical ‘not’ that returns the true or false if the
    ; argument is logical false or logical true, respectively.
    (is (= true (not false)))
    (is (= false (not true)))
    (is (= __ (not nil)))
    (is (= __ (not :a)))
    (is (= __ (not 'sym)))))

;;;
;;; Part 5: map, filter, and reduce
;;;
;;; Now that we know a bit about functions, higher-order functions, and how
;;; Clojure treats logical values, we can delve into map, filter, and reduce.
;;; These three functions lie at the heart of how most functional languages
;;; manage collections of data instead of using iterative loops.
;;;

(deftest map-filter-reduce
  (testing "map"
    ; ‘map’ is a higher order function that takes function and applies it to
    ; every element in a sequence, returning a new sequence
    (is (= '(1 2 3 4) (map inc (range 4))))
    (is (= '(true false true false) (map even? (range 4))))
    (is (= __ (map (fn [x] (+ x 2))
                   [-1 1 3 5])))
    (is (= __ (map {:a 1 :b 2 :c 3 :d 4}
                   [:a :d :c :b])))
    (is (= __ (map #{:a} (sorted-set :a :b :c))))
    (is (= '(0 3 6 9) (map __ (range 4))))
    (is (= '(-2 0 2 4) (map (fn [x] (* 2 (dec x)))
                            __))))

  (testing "filter"
    ; ‘filter’ is a higher order function that takes a predicate, a function
    ; that takes an argument and returns a logical true or false value, and a
    ; sequence.  It returns a sequence that contains only the elements that
    ; match the predicate.
    (is (= '(0 2 4) (filter even? (range 6))))
    (is (= '(1 3 5) (filter odd? (range 6))))

    (is (= __ (filter keyword? [0 :a 1 \a "a" :b \b])))
    (is (= __ (filter symbol? [0 :a 1 \a "a" :b \b])))
    (is (= __ (filter char? [0 :a 1 \a "a" :b \b])))
    (is (= '(1 3 2 5) (filter __ [0 1 -2 3 -4 2 5])))
    (is (= '(-2 -4) (filter __ [0 1 -2 3 -4 2 5])))
    (is (= '(0 4 8 12 16) (filter (fn [x] (zero? (rem x 4))) __))))

  (testing "reduce"
    ; The last of the three functions is ‘reduce’, which is a higher-order
    ; function that accumulates a result over a sequence.  It takes a reducing
    ; function, an initial value, and a sequence (there is a version that does
    ; not take an initial value, but its behaviour is more complex).
    (is (= 10 (reduce + 0 (range 5))))
    (is (= 11 (reduce + 1 (range 5))))
    (is (= __ (reduce * 1 (range 1 6))))
    (is (= __ (reduce * 2 (range 1 6))))
    (is (= __ (reduce * 2 '())))
    (is (= 6 (reduce __ 0 [2 2 2])))
    (is (= 8 (reduce __ 1 [2 2 2])))
    (is (= [1 2 3] (reduce conj __ [1 2 3])))
    (is (= [4 1 2 3] (reduce conj [4 1] __))))

  (testing "putting it all together"
    ; Double all of the even numbers from 0 to 9
    (= '(0 4 8 12 16 18) (map __ (filter __ (range 10))))

    ; Multiply every number from 0 to 9 by 3 and get the sum
    (= 135 (reduce __ __ (map __ (range 10))))

    ; Add all the even numbers from 100 to 199
    (= 7450 (reduce __ __ (filter __ (range 100 200))))

    ; Take all of the even numbers between from -10 to 5, multiply them by 5,
    ; and sum them all up.
    (= -120 (__ __ __ (__ __ (__ __ (range -10 6)))))))
