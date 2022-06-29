(ns mire.player (:require [mire.count-map :as cm] [mire.generator :as gen]))

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *gold*)
(def ^:dynamic *exited*)
(def ^:dynamic *rob-cooldown*)
(def ^:dynamic *rob-data*)
(def ^:dynamic *rob-attempts-left*)

(def prompt "> ")
(def streams (ref {}))
(def gold-vaults (ref {}))

(defn carrying? [thing]
  (cm/has? @*inventory* (keyword thing)))

(defn is-key? [thing]
  (gen/all-keys-set (keyword thing)))

(defn carrying-key? []
  (some #(cm/has? @*inventory* %) gen/all-keys-set))
