(ns kotoba.codegen.mc-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.codegen.mc :as mc]
            [kotoba.mir :as mir]))

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
                     :shift-left :shift-right-signed :shift-right-unsigned
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

(deftest selected-f64-family-is-admitted
  (doseq [target [:x86-64 :aarch64]
          operation [:f64-add :f64-subtract :f64-multiply :f64-divide
                     :f64-min :f64-max :f64-equal :f64-less-than
                     :f64-less-or-equal :f64-greater-than
                     :f64-greater-or-equal :f64-unordered]]
    (let [[dst left right] (if (= :x86-64 target)
                             [:x86-64/rdx :x86-64/rax :x86-64/rcx]
                             [:aarch64/x2 :aarch64/x0 :aarch64/x1])
          candidate {:mc/version 2 :mc/target target :mc/frame-slots 0
                     :mc/instructions
                     [{:mc/op :mc/instruction
                       :mc/encoding (keyword (name target) (name operation))
                       :mir/dst dst :mir/left left :mir/right right}
                      {:mc/op :mc/instruction
                       :mc/encoding (keyword (name target) "return")
                       :mir/value dst}]}]
      (is (= candidate (mc/validate! candidate)))))
  (doseq [[target register] [[:x86-64 :x86-64/rax]
                             [:aarch64 :aarch64/x0]]]
    (let [candidate {:mc/version 2 :mc/target target :mc/frame-slots 0
                     :mc/instructions
                     [{:mc/op :mc/instruction
                       :mc/encoding (keyword (name target) "f64-sqrt")
                       :mir/dst register :mir/input register}
                      {:mc/op :mc/instruction
                       :mc/encoding (keyword (name target) "return")
                       :mir/value register}]}]
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
       :mir/dst :x86-64/rdi :mir/index 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/move
       :mir/dst :x86-64/rax :mir/src :x86-64/rdi}
      {:mc/op :mc/instruction :mc/encoding :x86-64/return
       :mir/value :x86-64/rax}]}
    {:mc/name 'main :mc/arity 1 :mc/frame-slots 1
     :mc/frame-policy :call-live
     :mc/instructions
     [{:mc/op :mc/instruction :mc/encoding :x86-64/argument
       :mir/dst :x86-64/rdi :mir/index 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/move
       :mir/dst :x86-64/rax :mir/src :x86-64/rdi}
      {:mc/op :mc/instruction :mc/encoding :x86-64/constant
       :mir/dst :x86-64/rcx :mir/value 10}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-store
       :mir/src :x86-64/rcx :mir/slot 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/move
       :mir/dst :x86-64/rdi :mir/src :x86-64/rax}
      {:mc/op :mc/instruction :mc/encoding :x86-64/call
       :mir/dst :x86-64/rax :mir/callee 'add-one
       :mir/arguments [:x86-64/rdi]}
      {:mc/op :mc/instruction :mc/encoding :x86-64/spill-load
       :mir/dst :x86-64/rcx :mir/slot 0}
      {:mc/op :mc/instruction :mc/encoding :x86-64/add
       :mir/dst :x86-64/rdx :mir/left :x86-64/rcx :mir/right :x86-64/rax}
      {:mc/op :mc/instruction :mc/encoding :x86-64/return
       :mir/value :x86-64/rdx}]}]})

(deftest v3-mc-preserves-function-frame-and-call-encoding-ownership
  (is (= call-module (mc/validate! call-module)))
  (let [call-index (first
                    (keep-indexed
                     (fn [index instruction]
                       (when (= :x86-64/call (:mc/encoding instruction)) index))
                     (get-in call-module [:mc/functions 1 :mc/instructions])))]
    (testing "target encodings and exact call keysets remain closed"
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate!
                    (assoc-in call-module
                              [:mc/functions 1 :mc/instructions call-index :mc/encoding]
                              :aarch64/call))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate!
                    (assoc-in call-module
                              [:mc/functions 1 :mc/instructions call-index :ambient/policy]
                              true)))))
    (testing "MIR independently revalidates entry ABI, callee, and frame policy"
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate!
                    (assoc-in call-module
                              [:mc/functions 0 :mc/instructions 0 :mir/dst]
                              :x86-64/rax))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate!
                    (assoc-in call-module
                              [:mc/functions 1 :mc/instructions call-index :mir/callee]
                              'missing))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate!
                    (assoc-in call-module [:mc/functions 1 :mc/frame-policy]
                              :allocator)))))))

(deftest v3-mc-admits-bounded-fifth-entry-argument-spill
  (doseq [target [:x86-64 :aarch64]]
    (let [arguments (get mir/call-argument-registers target)
          [r0 r1 r2] (get mir/physical-registers target)
          fifth (arguments 4)
          instruction (fn [operation operands]
                        (merge {:mc/op :mc/instruction
                                :mc/encoding (keyword (name target)
                                                      (name operation))}
                               operands))
          module {:mc/version 3
                  :mc/target target
                  :mc/entry 'sum-five
                  :mc/functions
                  [{:mc/name 'sum-five
                    :mc/arity 5
                    :mc/frame-slots 1
                    :mc/frame-policy :allocator
                    :mc/instructions
                    (vec (concat
                          (map-indexed
                           (fn [index register]
                             (instruction :argument
                                          {:mir/dst register :mir/index index}))
                           arguments)
                          [(instruction :spill-store
                                        {:mir/src fifth :mir/slot 0})
                           (instruction :add
                                        {:mir/dst r0 :mir/left r0 :mir/right r1})
                           (instruction :spill-load
                                        {:mir/dst r2 :mir/slot 0})
                           (instruction :add
                                        {:mir/dst r0 :mir/left r0 :mir/right r2})
                           (instruction :return {:mir/value r0})]))}]}]
      (is (= module (mc/validate! module)) target)
      (is (thrown? clojure.lang.ExceptionInfo
                   (mc/validate! (assoc-in module
                                           [:mc/functions 0 :mc/frame-slots] 0)))
          target))))
