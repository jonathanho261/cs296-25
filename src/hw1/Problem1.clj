(ns hw1.Problem1)


(defn sum-mults [n & mults]
      (let [pred (apply some-fn
                        (map #(fn [x] (zero? (mod x %))) mults))]
           (->> (range n) (filter pred) (reduce +))))

(println (sum-mults 1000 3 5))


(defn collatz-sequence [n]
      {:pre [(pos? n)]}
      (lazy-seq
        (cond (= n 1)   '(1)
              (even? n) (cons n (collatz-sequence (/ n 2)))
              :else     (cons n (collatz-sequence (+ (* n 3) 1))))))



(let [hseq (collatz-sequence 27)]
     (->  hseq count      (= 112)            assert)
     (->> hseq (take 4)   (= [27 82 41 124]) assert)
     (->> hseq (drop 108) (= [8 4 2 1])      assert))

(let [{max-i :num, max-len :len}
      (reduce #(max-key :len %1 %2)
              (for [i (range 1 100000)]
                   {:num i, :len (count (collatz-sequence i))}))]
     (println "Maximum length" max-len "was found for hailstone(" max-i ")."))


