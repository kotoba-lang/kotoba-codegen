(ns kotoba.codegen.layout-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.codegen.layout :as layout]))

(defn- le32 [n]
  (mapv #(bit-and (unsigned-bit-shift-right (long n) (* 8 %)) 0xff) (range 4)))

(defn- size-of [token]
  (or (layout/token-size token)
      (when (and (integer? token) (<= 0 token 255)) 1)))

(defn- encode-branch [{:mir/keys [encoding]} displacement]
  (case encoding
    :x86-64/jz-rel32 (into [0x0f 0x84] (le32 displacement))
    :x86-64/jmp-rel32 (into [0xe9] (le32 displacement))
    :x86-64/call-rel32 (into [0xe8] (le32 displacement))))

(defn- resolve-layout [tokens]
  (let [labels (layout/label-offsets tokens size-of)]
    (layout/resolve-tokens tokens size-of labels encode-branch
                           (fn [token _position] [token]))))

(deftest labels-are-zero-width-and-forward-branches-use-final-layout
  (let [target :test.label/else
        tokens [(layout/relative-branch :x86-64/jz-rel32 target)
                0xaa 0xbb
                (layout/label target)]]
    (is (= {target 8} (layout/label-offsets tokens size-of)))
    (is (= (vec (concat [0x0f 0x84] (le32 2) [0xaa 0xbb]))
           (resolve-layout tokens))))
  (testing "the same branch is recomputed after an optimization changes arm size"
    (let [target :test.label/after]
      (is (= (vec (concat [0x0f 0x84] (le32 7) (repeat 7 0x90)))
             (resolve-layout (concat [(layout/relative-branch :x86-64/jz-rel32 target)]
                                     (repeat 7 0x90)
                                     [(layout/label target)]))))
      (is (= (vec (concat [0x0f 0x84] (le32 1) [0x90]))
             (resolve-layout [(layout/relative-branch :x86-64/jz-rel32 target)
                              0x90
                              (layout/label target)]))))))

(deftest backward-branches-use-signed-relative-displacements
  (let [target :test.label/loop]
    (is (= (vec (concat [0xaa 0xbb 0xe9] (le32 -7)))
           (resolve-layout [(layout/label target)
                            0xaa 0xbb
                            (layout/relative-branch :x86-64/jmp-rel32 target)])))))

(deftest direct-calls-reserve-and-resolve-their-architectural-width
  (let [target :test.function/callee
        x86 [(layout/relative-branch :x86-64/call-rel32 target)
             0x90
             (layout/label target)]
        arm [(layout/relative-branch :aarch64/bl-imm26 target)
             (layout/label target)]]
    (is (= 5 (layout/token-size (first x86))))
    (is (= [0xe8 0x01 0x00 0x00 0x00 0x90] (resolve-layout x86)))
    (is (= 4 (layout/token-size (first arm))))
    (is (= {target 4} (layout/label-offsets arm size-of)))))

(deftest malformed-or-unresolved-layout-fails-closed
  (testing "duplicate labels"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"duplicate MIR label"
                          (layout/label-offsets [(layout/label :test.label/a)
                                                 (layout/label :test.label/a)]
                                                size-of))))
  (testing "labels and targets are canonical qualified keywords"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"qualified keyword"
                          (layout/label-offsets [(layout/label :local)] size-of)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"qualified keyword"
                          (layout/label-offsets
                           [(layout/relative-branch :x86-64/jmp-rel32 :local)] size-of))))
  (testing "unknown encodings and extra fields are rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unsupported"
                          (layout/label-offsets
                           [(layout/relative-branch :x86-64/jne-rel32 :test.label/a)] size-of)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-canonical"
                          (layout/label-offsets
                           [(assoc (layout/label :test.label/a) :extra true)] size-of))))
  (testing "AArch64 TBNZ operands are closed and range checked"
    (is (= 4 (layout/token-size
              (layout/relative-branch :aarch64/tbnz-imm14 :test.label/a [16 63]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"TBNZ requires"
                          (layout/label-offsets
                           [(layout/relative-branch :aarch64/tbnz-imm14
                                                    :test.label/a [32 0])]
                           size-of)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not accept operands"
                          (layout/label-offsets
                           [(layout/relative-branch :aarch64/b-imm26
                                                    :test.label/a [0])]
                           size-of))))
  (testing "unknown MIR operations never fall through as backend-owned maps"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown canonical MIR"
                          (layout/label-offsets [{:mir/op :mir/invented}] (constantly 1)))))
  (testing "all branch targets must exist"
    (let [tokens [(layout/relative-branch :x86-64/jmp-rel32 :test.label/missing)]
          labels (layout/label-offsets tokens size-of)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown label"
                            (layout/resolve-tokens tokens size-of labels encode-branch
                                                   (fn [token _] [token]))))))
  (testing "rel32 overflow is rejected instead of truncated"
    (let [target :test.label/far
          tokens [(layout/relative-branch :x86-64/jmp-rel32 target)]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"out of range"
                            (layout/resolve-tokens tokens size-of
                                                   {target 0x80000005}
                                                   encode-branch
                                                   (fn [token _] [token])))))))

(deftest branch-encoder-must-honor-the-width-reserved-by-layout
  (let [target :test.label/end
        tokens [(layout/relative-branch :x86-64/jmp-rel32 target)
                (layout/label target)]
        labels (layout/label-offsets tokens size-of)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reserved width"
                          (layout/resolve-tokens tokens size-of labels
                                                 (fn [_ _] [0xe9])
                                                 (fn [token _] [token]))))))
