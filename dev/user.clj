(ns user
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.index :as index]))

(comment
  (clerk/serve! {:watch-paths ["src"] :port 8023 :browse? true})
  (clerk/clear-cache!)
  (clerk/halt!)

  (clerk/build! {:index "src/advent_of_clerk/index.clj"
                 :paths (index/build-index-paths)
                 :browse? true}))
