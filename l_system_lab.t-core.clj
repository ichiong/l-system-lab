(ns l_system_lab.t-core
  (:use midje.sweet)
  (:use [l_system_lab.core]))

(facts "about numners"
       (fact "Floating point is close enough sometimes."
             10.0 => (roughly 10.001)
             9.0 => (roughly 10.2)))