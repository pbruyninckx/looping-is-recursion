(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (cond
                   (== exp 0) acc
                   (even? exp) (recur (* base base) (/ exp 2) acc)
                   :else (recur base (dec exp) (* acc base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (if
    (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
    true
    (or (empty? seq1) (empty? seq2))
    false
    (== (first seq1) (first seq2))
    (recur (rest seq1) (rest seq2))
    :else
    false))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) ind
      :else (recur (inc ind) (rest s)))))

(defn avg [a-seq]
  (loop [len 0
         s a-seq
         acc 0]
    (if (empty? s)
      (/ acc len)
      (recur (inc len) (rest s) (+ (first s) acc)))))

(defn parity [a-seq]
  (loop [acc #{}
        s a-seq]
    (if (empty? s)
      acc
      (recur ((if (contains? acc (first s))
                disj
                conj) acc (first s))
             (rest s)))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 1
         ind 0]
    (if (= ind n)
      f1
      (recur f2 (+ f1 f2) (inc ind)))))

(defn cut-at-repetition [a-seq]
  (loop
    [acc []
     seen #{}
     r-seq a-seq]
    (cond
      (empty? r-seq)
      acc
      (contains? seen (first r-seq))
      acc
      :else
      (recur (conj acc (first r-seq))
             (conj seen (first r-seq))
             (rest r-seq)))))

