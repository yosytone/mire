(ns mire.count-map)

(defn cm-remove [count-map obj]
  "Remove from count-map"
  (let [new-count (dec (get count-map obj 1))]
      (if (= new-count 0) 
         (dissoc count-map obj) 
         (assoc count-map obj new-count))))
  
(defn cm-add [count-map obj]
 "Add to count-map"
 (let [new-count (inc (get count-map obj 0))]
     (assoc count-map obj new-count)))

(defn has? [count-map obj]
    (not (nil? (count-map obj))))
  
(defn to-count-map [coll]
    "Convert to count-map"
    (reduce cm-add {} coll))
