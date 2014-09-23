(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [b-seq n]
                 (cond 
                   (= n 0)
                     nil

                   (= n 1)
                     (first b-seq)
                   
                   :else
                     (recur (rest b-seq) (dec n))))]
    (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2 n]
                 (cond
                   (not (= (count s1) (count s2)))
                      false

                   (and (empty? s1) (empty? s2))
                      true

                   (not (= (first s1) (first s2)))
                      false

                   :else
                      (recur (rest s1) (rest s2) (dec n))))]
    (helper seq1 seq2 (max (count seq1) (count seq2)))))

(defn find-first-index [pred a-seq]
  (loop [n 0 b-seq a-seq]
    (cond
      (empty? b-seq)
        nil

      (pred (first b-seq))
        n

      :else
        (recur (inc n) (rest b-seq)))))

(defn avg [a-seq]
  (loop [acc 0 n 0 b-seq a-seq]
    (cond
      (empty? a-seq)
        0

      (empty? b-seq)
        (/ acc n)

      :else
        (recur (+ acc (first b-seq)) (inc n) (rest b-seq)))))

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  (loop [down-from n total 1 total-prev 1]
    (cond
      (= n 0)
        0

      (= n 1)
        1

      (= down-from 1)
        total-prev

      :else
        (recur (dec down-from) (+ total total-prev) total))))

(defn cut-at-repetition [a-seq]
  (loop [seen-vec []
    (cond
      (contains? (set seen-vec) (first a-seq))
        seen-vec
     
      :else
        (recur (conj seen-vec (first a-seq))))))

