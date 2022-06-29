(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [clojure.string :as string]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.generator :as generator]))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn is-game-over? []
  (and @rooms/game-over-time (> (System/currentTimeMillis) @rooms/game-over-time)))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "\nWhat is your name? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms 0))
              player/*inventory* (ref {})
              player/*gold* (ref 0)
              player/*exited* (ref false)
              player/*rob-cooldown* (ref 0)
              player/*rob-data* (ref nil)
              player/*rob-attempts-left* (ref 3)]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*)
       (commute player/gold-vaults assoc player/*name* player/*gold*))

      (println (commands/look)) (print player/prompt) (flush)

      (try (loop [input (read-line)]
             (when input
               (when (not= "" (string/trim input))
                (println (commands/execute input)))
               (.flush *err*)
               (print player/prompt) (flush)
               (if (is-game-over?)
                (println "Game is over!" @rooms/current-winner "won!")
                (recur (read-line)))))
           (finally (cleanup))))))

(defn -main
  ([port]
   (rooms/add-rooms)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port))
  ([] (-main 3333)))
