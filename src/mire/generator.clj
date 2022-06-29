(ns mire.generator)

(def room-descriptions ["A room"])
(def next-room-id (ref 0))

(def start-room {:id 0 
                 :desc "Start room"
                 :exits {}
                 :keys []
                 :chest nil
                 :notes []})
(def two-rooms [{:id -1 :desc "Room 1" :exits {} :keys []} {:id -2 :desc "Room 2" :exits {} :keys []}])

(defn rand-irange [a b] (+ a (rand-int (- b a))))

(defn get-random-room [] 
    (let [room-desc (rand-nth room-descriptions)
          room-id (dosync (alter next-room-id inc))]
        {:id room-id
         :desc room-desc
         :exits {}
         :keys []
         :chest nil
         :notes []}))

(def all-exits #{:north :south :east :west :up :down})
(defn reverse-direction [dir]
    (case dir
        :north :south
        :south :north
        :east :west
        :west :east
        :up :down
        :down :up))

(defn add-exits [rooms exits]
    (mapv (fn [room] 
             (let [relevant-exits 
                   (filter (fn [[dir to-id from-id]] (= (:id room) from-id)) exits)
                   exit-vect (mapcat (fn [[dir to-id from-id]] [dir to-id]) relevant-exits)]
                   ;_ (println "exit-vect=" exit-vect)]
                  (if (empty? exit-vect) 
                      room 
                      (assoc room :exits 
                       (apply assoc (:exits room) exit-vect)))))
         rooms))

(defn generate-passages [prev-layer new-layer]
    (let [exit-directions (map (fn [room] 
                                (shuffle (apply disj all-exits (keys (:exits room)))))
                           prev-layer)
          max-number-exits (map count exit-directions)
          exits (mapcat 
                       (fn [dirs room] 
                           (map (fn [dir] [dir (:id room)]) dirs)) 
                       exit-directions prev-layer)
          entrances-data (shuffle (map (fn [[dir id]] [(reverse-direction dir) id]) exits))
          required-entrances (map :id new-layer)
          max-random-count (- (count entrances-data) (count new-layer))
          random-entrances (take (rand-irange 1 max-random-count)
                            (repeatedly #(rand-nth required-entrances)))
          entrances-data-full (map 
                               (fn [new-id [dir old-id]] [dir old-id new-id]) 
                               (concat required-entrances random-entrances)
                               entrances-data)
          exits-data-full (map
                           (fn [[dir old-id new-id]] 
                            [(reverse-direction dir) new-id old-id]) 
                           entrances-data-full)]
        [(add-exits prev-layer exits-data-full) 
         (add-exits new-layer entrances-data-full) 
         exits-data-full]))

(def all-keys [:red-key :blue-key :green-key :white-key :black-key])
(def all-keys-with-star (conj all-keys :star-key))
(def all-keys-set (into #{} all-keys-with-star))

(defn generate-keys [exits-data]
    (let [keys-counts (take (count exits-data) (repeatedly #(rand-irange 1 4)))
          needed-keys (map (fn [n] (take n (repeatedly #(rand-nth all-keys)))) keys-counts)]
     [(map conj exits-data needed-keys) (apply concat needed-keys)]))

(defn place-keys [prev-layer keys-to-place]
    (let [room-count (count prev-layer)
          rooms-choice (map (fn [k] [(rand-irange 0 room-count) k]) keys-to-place)]
         (reduce (fn [layer [i k]]
                  (assoc-in layer [i :keys] 
                    (conj (get-in layer [i :keys]) k)))
                 (into [] prev-layer) rooms-choice)))

(defn add-free-passages [layer]
    (let [free-ways (mapcat (fn [room] 
                             (map #(conj [% (:id room)]) 
                                (apply disj all-exits 
                                        (keys (:exits room))))) 
                         layer)
          ;_ (println "free-ways=" free-ways)
          passage-count (rand-irange 
                         (quot (count free-ways) 6) 
                         (+ 1 (quot (count free-ways) 3)))
          ;_ (println "passage-count=" passage-count)
          passages (take passage-count (partition 2 2 (shuffle free-ways)))
          passages-without-loops (filter (fn [[[_ from] [_ to]]] (not= from to)) passages)
          passage-data (mapcat (fn 
                                [[[way-from from] [way-to to]]] 
                                [[way-from to from] [way-to from to]]) passages-without-loops)]
          ;_ (println "passage-data=" passage-data)]
     (add-exits layer passage-data)))

(defn add-chests [rooms note-list]
  (mapv (fn [r]
         (if (< (rand) 0.3) r
          (let [chest-money (* 50 (rand-irange 3 11))
                 chest-note-count (rand-irange 2 5)
                 chest-notes (into #{} 
                              (take chest-note-count 
                               (repeatedly #(rand-nth note-list))))
                 chest [chest-money chest-notes]]
               (assoc r :chest chest)))) rooms))

(defn add-notes [rooms note-list]
  (let [room-count (count rooms)
        note-count (count note-list)
        note-rooms-choice (mapv vector
                           (take note-count (repeatedly #(rand-int (count rooms)))) 
                           note-list)]
       (reduce (fn [l [i note]] 
                (assoc-in l [i :notes] 
                            (conj (get-in l [i :notes]) note)))
        rooms note-rooms-choice)))

(defn add-exit-doors [rooms]
  (let [room-count (count rooms)
        one-room (rooms (- room-count 1))
        two-room (rooms (- room-count 2))
        three-room (rooms (- room-count 3))
        one-room-exit (assoc one-room :exit-door 1)
        two-room-exit (assoc two-room :exit-door 2)
        three-room-exit (assoc three-room :exit-door 3)]
       (concat (subvec rooms 0 (- room-count 3)) [three-room-exit two-room-exit one-room-exit])))

(defn generate-more [prev-layer]
    (let [old-room-count (count prev-layer)
          new-room-count (if (= old-room-count 1) 3 (rand-irange 2 6))
          new-rooms (take new-room-count (repeatedly get-random-room))
          ;_ (println "new-rooms=" new-rooms)
          [old-with-passages new-with-passages exits-data-full] 
          (generate-passages prev-layer new-rooms)
          ;_ (println "old-with-passages=" old-with-passages)
          ;_ (println "new-with-passages=" new-with-passages)
          ;_ (println "exits-data-full=" exits-data-full)
          [exits-with-keys required-keys] (generate-keys exits-data-full)
          ;_ (println "exits-with-keys=" exits-with-keys)
          ;_ (println "required-keys=" required-keys)
          old-with-keys (place-keys old-with-passages required-keys)
          ;_ (println "old-with-keys=" old-with-keys)
          new-with-free-passages (add-free-passages new-with-passages)]
          ;_ (println "new-with-free-passages=" new-with-free-passages)]
          
         [old-with-keys new-with-free-passages exits-with-keys]))

(defn generate-all-layers []
    (let [values (take 5 (iterate 
                          (fn [[prev-layer new-layer exits-with-keys]] (generate-more new-layer)) 
                          [nil [start-room] nil]))]
        [(apply concat (concat (rest (map first values)) [(second (last values))]))
         (apply concat (concat (rest (map #(% 2) values)) ((last values) 2)))]))

(defn generate-note-list [n]
  (into #{} (map #(format "%05d" %) (take n (repeatedly #(rand-irange 0 100000))))))

(defn generate-full []
  (let [[rooms passages] (generate-all-layers)
        expected-note-count (* 2 (count rooms))
        note-list (doall (into [] (generate-note-list expected-note-count)))
        ;_ (println "rooms" rooms)
        ;_ (println "note-list" note-list)
        rooms-with-chests (add-chests (into [] rooms) note-list)
        rooms-with-notes (add-notes (into [] rooms-with-chests) note-list)
        rooms-with-exits (add-exit-doors 
                          (place-keys rooms-with-notes 
                            (repeat 4 :star-key)))]
    [rooms-with-exits passages]))
