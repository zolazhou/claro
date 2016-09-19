(ns user
  (:require [claro.data :as data]
            [claro.engine :as engine]
            [claro.data.ops :as ops]
            [claro.projection :as projection]
            [claro.middleware.observe :refer [wrap-observe wrap-observe-by-class]]
            [claro.middleware.mock :refer [wrap-mock]]
            [manifold.deferred :as d]))

;; ## Records

(declare ->Collage ->Person)

(defrecord IsFollowing [follower-id id]
  data/Resolvable
  (resolve! [this _]
    (locking *out* (prn 'checking 'follower-status id '/ follower-id))
    (and follower-id
         id
         (not= follower-id id)
         (d/future
           #_(Thread/sleep 5)
           (= (mod follower-id 10) (mod id 10))))))

(defrecord Followers [person-id]
  data/Resolvable
  (resolve! [_ _]
    (d/future
      #_(Thread/sleep 5)
      (locking *out* (prn 'fetching 'followers person-id))
      (range 0 18 3)))

  data/Transform
  (transform [_ ids]
    (map ->Person ids)))

(defrecord Person [id]
  data/Resolvable
  data/BatchedResolvable
  (resolve-batch! [_ _ people]
    (d/future
      #_(Thread/sleep 5)
      (locking *out* (prn 'fetching 'people (map :id people)))
      (mapv
        (fn [{:keys [id]}]
          {:id        id
           :name      (str "Person #" id)
           :collage-ids [(quot id 5) (quot (* id 3) 5) (quot (* id 5) 5)]})
        people)))

  data/Transform
  (transform [_ {:keys [id collage-ids] :as person}]
    (-> person
        (assoc :collages  (map ->Collage collage-ids))
        (assoc :followers (->Followers id))
        (assoc :follows?  (map->IsFollowing {:follower-id id})))))

(defrecord Collage [id]
  data/Resolvable
  (resolve! [_ _]
    (let [person-id (* id 5)]
      (d/future
        #_(Thread/sleep 5)
        (locking *out* (prn 'fetching 'collage id))
        {:id               id
         :name             (str "Collage #" id)
         :random           (rand-nth [{:x 1} [{:x 1} {:x 2}] nil])
         :creator          (->Person person-id)
         :creator-follows? (map->IsFollowing {:follower-id person-id})}))))

;; ## Resolution

"
{
  collage(id: 1) {
    id,
    name,
    creator { name, collages { name } },
    creator-follows-me? : creator-follows?(id: 15),
    creator-follows-this-other-person? : creator-follows?(id: 12)
  }
}
"

(def projection
  (projection/union
    {:id      projection/leaf
     :name    projection/leaf
     :creator
     (projection/let [{:keys [id name]} {:id projection/leaf
                                         :name projection/leaf}]
       {:id projection/leaf
        :followers [(projection/let [{follower-id :id} {:id projection/leaf}]
                      {:id projection/leaf
                       :name projection/leaf
                       :creator-name (projection/value name projection/leaf)
                       :follows-creator?
                       (projection/value (->IsFollowing follower-id id))})]
        :collages [{(projection/alias :the-person :creator) (projection/extract :name)
                    (projection/alias :the-person-id :creator) (projection/extract :id)}]})}
    {(projection/alias :creator-follows-this-other-user? :creator-follows?)
     (projection/parameters {:id 20} projection/leaf)

     (projection/alias :creator-follows-me?  :creator-follows?)
     (projection/parameters {:id 15} projection/leaf)}))

(def engine-run!
  (-> (engine/engine
        {:selector (claro.engine.selector/exact-selector
                     (cycle
                       [[Collage]
                        [Person]
                        [Followers Collage]
                        [Person]]))})
      #_(wrap-mock
        Person (fn [{:keys [id]} _]
                 {:id id
                  :name "Me"
                  :collage-ids []
                  }))
      (wrap-observe-by-class
        [Person]
        #(prn '!!! %1 '-> %2))))

(defn resolve-it
  []
  @(engine-run!
    (-> (->Collage 1)
        (projection/apply projection))))

(defn resolve-with-timeout
  []
  (-> (->Person 1)
      (engine-run!)
      (d/timeout! 100)))

(defrecord Person [id]
  data/Resolvable
  (resolve! [_ env]
    (d/future
      {:id id
       :name (str "Person #" id)
       :friend-ids (range (inc id) (+ id 7) 3)}))

  data/Transform
  (transform [_ {:keys [friend-ids] :as person}]
    (-> person
        (assoc :friends (map ->Person friend-ids))
        (dissoc :friend-ids))))

(defn trace-resolution
  [input output]
  (locking *out*
    (prn input '-> output)))

(def run-engine
  (-> (engine/engine)
      (wrap-mock
        Person
        (fn [{:keys [id]} env]
          {:id         id
           :name       "Person"
           :friend-ids [(inc id)]}))
      (wrap-observe-by-class [Person] trace-resolution)))

(time
  (dotimes [_ 100]
    (-> [(->Person 1) (->Person 2)]
        (projection/apply [{:friends [{:name projection/leaf :friends [{:id projection/leaf}]}]}])
        (run-engine)
        (deref))))
