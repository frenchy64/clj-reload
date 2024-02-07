(ns cycle-a
  ;; first we require this ns, then add this dependency
  ;; to trigger a cycle
  #_
  (:require cycle-b))
