(ns cycle-b
  ;; first we require this ns, then add this dependency
  ;; to trigger a cycle
  #_
  (:require cycle-a))

(println "loading cycle-b")
