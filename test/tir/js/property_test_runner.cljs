(ns tir.js.property-test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [tir.js.property-test]))

(doo-tests 'tir.js.property.test)
