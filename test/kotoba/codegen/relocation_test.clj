(ns kotoba.codegen.relocation-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.codegen.relocation :as relocation]))

(def arm-request
  {:reloc/version 1 :reloc/target :aarch64 :reloc/section 1
   :reloc/offset 0 :reloc/type :aarch64/branch26 :reloc/symbol "_callee"})

(deftest canonical-relocations-are-target-explicit
  (is (= arm-request (relocation/validate! arm-request)))
  (is (= {:offset 0 :type :aarch64/branch26 :symbol "_callee"}
         (relocation/->macho arm-request)))
  (is (= :x86-64/branch
         (:reloc/type
          (relocation/validate!
           (assoc arm-request :reloc/target :x86-64
                              :reloc/type :x86-64/branch))))))

(deftest relocation-contract-fails-closed
  (testing "target and type namespaces cannot be mixed"
    (is (thrown? clojure.lang.ExceptionInfo
                 (relocation/validate!
                  (assoc arm-request :reloc/type :x86-64/branch)))))
  (testing "shape, section, offset, and symbol are bounded"
    (doseq [request [(assoc arm-request :ambient/policy true)
                     (assoc arm-request :reloc/section 0)
                     (assoc arm-request :reloc/offset -1)
                     (assoc arm-request :reloc/symbol "callee")]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (relocation/validate! request))))))
