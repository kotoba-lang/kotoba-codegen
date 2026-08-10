(ns kotoba.codegen.mc-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.codegen.mc :as mc]))

(def program
  {:mc/version 2
   :mc/target :x86-64
   :mc/frame-slots 0
   :mc/instructions
   [{:mc/op :mc/instruction :mc/encoding :x86-64/argument
     :mir/dst :x86-64/rax :mir/index 0}
    {:mc/op :mc/instruction :mc/encoding :x86-64/constant
     :mir/dst :x86-64/rcx :mir/value 1}
    {:mc/op :mc/instruction :mc/encoding :x86-64/add
     :mir/dst :x86-64/rdx :mir/left :x86-64/rax :mir/right :x86-64/rcx}
    {:mc/op :mc/branch-zero :mc/test :x86-64/rdx :mc/target :test.label/zero}
    {:mc/op :mc/instruction :mc/encoding :x86-64/return :mir/value :x86-64/rdx}
    {:mir/op :mir/label :mir/id :test.label/zero}
    {:mc/op :mc/jump :mc/target :test.label/done}
    {:mir/op :mir/label :mir/id :test.label/done}
    {:mc/op :mc/instruction :mc/encoding :x86-64/return :mir/value :x86-64/rcx}]})

(def spilled-program
  {:mc/version 2
   :mc/target :x86-64
   :mc/frame-slots 1
   :mc/instructions
   [{:mc/op :mc/instruction :mc/encoding :x86-64/constant
     :mir/dst :x86-64/rax :mir/value 42}
    {:mc/op :mc/instruction :mc/encoding :x86-64/spill-store
     :mir/src :x86-64/rax :mir/slot 0}
    {:mc/op :mc/instruction :mc/encoding :x86-64/spill-load
     :mir/dst :x86-64/rax :mir/slot 0}
    {:mc/op :mc/instruction :mc/encoding :x86-64/return
     :mir/value :x86-64/rax}]})

(deftest canonical-allocated-program-is-admitted
  (is (= program (mc/validate! program)))
  (is (= spilled-program (mc/validate! spilled-program)))
  (let [moved (assoc program :mc/instructions
                     [{:mc/op :mc/instruction :mc/encoding :x86-64/move
                       :mir/dst :x86-64/rcx :mir/src :x86-64/rax}
                      {:mc/op :mc/instruction :mc/encoding :x86-64/return
                       :mir/value :x86-64/rcx}])]
    (is (= moved (mc/validate! moved)))))

(deftest selected-i64-scalar-family-is-admitted
  (doseq [operation [:subtract :multiply :quotient :bit-and :bit-or :bit-xor
                     :equal :less-than :greater-than :less-or-equal
                     :greater-or-equal]]
    (let [instruction {:mc/op :mc/instruction
                       :mc/encoding (keyword "x86-64" (name operation))
                       :mir/dst :x86-64/rdx :mir/left :x86-64/rax
                       :mir/right :x86-64/rcx}
          candidate (assoc program :mc/instructions
                           (conj (subvec (:mc/instructions program) 0 2)
                                 instruction
                                 {:mc/op :mc/instruction
                                  :mc/encoding :x86-64/return
                                  :mir/value :x86-64/rdx}))]
      (is (= candidate (mc/validate! candidate))))))

(deftest contract-fails-closed
  (testing "target and selected encoding must agree"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in program [:mc/instructions 0 :mc/encoding]
                                         :aarch64/argument)))))
  (testing "selected keysets are exact"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in program [:mc/instructions 0 :ambient/policy]
                                         true)))))
  (testing "move is canonical and target-specific"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate!
                  (assoc program :mc/instructions
                         [{:mc/op :mc/instruction :mc/encoding :x86-64/move
                           :mir/dst :x86-64/rax :mir/src :x86-64/rcx
                           :ambient/copy true}])))))
  (testing "physical register profile comes from MIR"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in program [:mc/instructions 0 :mir/dst]
                                         :aarch64/x0)))))
  (testing "control flow remains a closed graph"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in program [:mc/instructions 3 :mc/target]
                                         :test.label/missing)))))
  (testing "unknown operations never reach a backend"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in program [:mc/instructions 0 :mc/op]
                                         :mc/invented)))))
  (testing "spill slots stay inside the declared frame"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (assoc-in spilled-program
                                         [:mc/instructions 1 :mir/slot] 1)))))
  (testing "MC v1 cannot silently omit frame ownership"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate! (-> program
                                   (assoc :mc/version 1)
                                   (dissoc :mc/frame-slots)))))))

(def call-module
  {:mc/version 3
   :mc/target :x86-64
   :mc/entry 'main
   :mc/functions
   [{:mc/name 'add-one :mc/arity 1 :mc/frame-slots 0
     :mc/frame-policy :allocator
     :mc/instructions
     [{:mc/op :mc/instruction :mc/encoding :x86-64/argument
       :mir/dst :x86-64/rax :mir/index 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/return
       :mir/value :x86-64/rax}]}
    {:mc/name 'main :mc/arity 1 :mc/frame-slots 2
     :mc/frame-policy :all-vregs
     :mc/instructions
     [{:mc/op :mc/instruction :mc/encoding :x86-64/argument
       :mir/dst :x86-64/rax :mir/index 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-store
       :mir/src :x86-64/rax :mir/slot 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-load
       :mir/dst :x86-64/rdi :mir/slot 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/call
       :mir/dst :x86-64/rax :mir/callee 'add-one
       :mir/arguments [:x86-64/rdi]}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-store
       :mir/src :x86-64/rax :mir/slot 1}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-load
       :mir/dst :x86-64/rax :mir/slot 1}
      {:mc/op :mc/instruction :mc/encoding :x86-64/return
       :mir/value :x86-64/rax}]}]})

(deftest v3-mc-preserves-function-frame-and-call-encoding-ownership
  (is (= call-module (mc/validate! call-module)))
  (testing "target encodings and exact call keysets remain closed"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate!
                  (assoc-in call-module
                            [:mc/functions 1 :mc/instructions 3 :mc/encoding]
                            :aarch64/call))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate!
                  (assoc-in call-module
                            [:mc/functions 1 :mc/instructions 3 :ambient/policy]
                            true)))))
  (testing "MIR independently revalidates callee and frame policy"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate!
                  (assoc-in call-module
                            [:mc/functions 1 :mc/instructions 3 :mir/callee]
                            'missing))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (mc/validate!
                  (assoc-in call-module [:mc/functions 1 :mc/frame-policy]
                            :allocator))))))
