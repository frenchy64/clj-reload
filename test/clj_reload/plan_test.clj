(ns clj-reload.plan-test
  (:require
   [clj-reload.plan :as plan]
   [clojure.test :refer [is deftest testing]]))

(deftest linear-fj-plan-test
  (is (= '[{:before [{:op :unload, :ns i}],
            :forks
            [{:before [{:op :unload, :ns j}],
              :forks [],
              :after [{:op :load, :ns j}]}]}]
         (plan/linear-fj-plan '{:to-unload (i j), :to-load (j)})))
  (is (= '[{:before [{:op :unload, :ns a}],
            :forks
            [{:before [{:op :unload, :ns b}],
              :forks
              [{:before [{:op :unload, :ns c}],
                :forks
                [{:before [{:op :unload, :ns d}],
                  :forks
                  [{:before [{:op :unload, :ns e}],
                    :forks [],
                    :after [{:op :load, :ns e}]}],
                  :after [{:op :load, :ns d}]}],
                :after [{:op :load, :ns c}]}],
              :after [{:op :load, :ns b}]}],
            :after [{:op :load, :ns a}]}]
         (plan/linear-fj-plan {:to-unload '[a b c d e] :to-load (rseq '[a b c d e])})))
  (is (= '[{:before [{:op :unload, :ns a}],
            :forks
            [{:before [{:op :unload, :ns b}],
              :forks
              [{:before [{:op :unload, :ns c}],
                :forks
                [{:before [{:op :unload, :ns d}],
                  :forks
                  [{:before [{:op :unload, :ns e}],
                    :forks [],
                    :after [{:op :load, :ns a}]}],
                  :after [{:op :load, :ns b}]}],
                :after [{:op :load, :ns c}]}],
              :after [{:op :load, :ns d}]}],
            :after [{:op :load, :ns e}]}]
         (plan/linear-fj-plan {:to-unload '[a b c d e] :to-load '[a b c d e]}))))
