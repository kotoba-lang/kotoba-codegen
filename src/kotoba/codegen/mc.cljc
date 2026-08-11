(ns kotoba.codegen.mc
  "Closed allocated machine-code program contract.

  MC retains MIR operands and bounded spill-frame ownership after target
  selection and allocation while making the target encoding choice explicit.
  Backends own the bytes for each encoding; this namespace owns the admitted
  data shape."
  (:require [kotoba.mir :as mir]))

(def version 3)

(def ^:private selected-keysets
  {:argument #{:mc/op :mc/encoding :mir/dst :mir/index}
   :constant #{:mc/op :mc/encoding :mir/dst :mir/value}
   :add #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :subtract #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :multiply #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :quotient #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :bit-and #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :bit-or #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :bit-xor #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :shift-left #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :shift-right-signed #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :shift-right-unsigned #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-add #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-subtract #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-multiply #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-divide #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-min #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-max #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-sqrt #{:mc/op :mc/encoding :mir/dst :mir/input}
   :f64-equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-less-than #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-less-or-equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-greater-than #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-greater-or-equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :f64-unordered #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :kernel-load-u8 #{:mc/op :mc/encoding :mir/dst :mir/base :mir/length
                     :mir/index :mir/maximum}
   :kernel-store-u8 #{:mc/op :mc/encoding :mir/dst :mir/base :mir/length
                      :mir/index :mir/stored :mir/maximum}
   :kernel-load-u32 #{:mc/op :mc/encoding :mir/dst :mir/base :mir/length
                      :mir/index :mir/maximum}
   :kernel-store-u32 #{:mc/op :mc/encoding :mir/dst :mir/base :mir/length
                       :mir/index :mir/stored :mir/maximum}
   :kernel-subregion #{:mc/op :mc/encoding :mir/dst :mir/base :mir/length
                       :mir/offset :mir/size}
   :equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :less-than #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :greater-than #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :less-or-equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :greater-or-equal #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :spill-load #{:mc/op :mc/encoding :mir/dst :mir/slot}
   :spill-store #{:mc/op :mc/encoding :mir/src :mir/slot}
   :move #{:mc/op :mc/encoding :mir/dst :mir/src}
   :call #{:mc/op :mc/encoding :mir/dst :mir/callee :mir/arguments}
   :runtime-call #{:mc/op :mc/encoding :mir/dst :mir/runtime
                   :mir/context-offset :mir/arguments}
   :return #{:mc/op :mc/encoding :mir/value}})

(def ^:private operation-keysets
  {:mc/branch-zero #{:mc/op :mc/test :mc/target}
   :mc/jump #{:mc/op :mc/target}})

(defn- reject! [problem instruction]
  (throw (ex-info (str "MC rejected: " (name problem))
                  {:phase :mc :problem problem :instruction instruction})))

(defn- selected-operation [target instruction]
  (let [encoding (:mc/encoding instruction)]
    (when-not (and (keyword? encoding) (= (name target) (namespace encoding)))
      (reject! :target-encoding-mismatch instruction))
    (let [operation (keyword (name encoding))
          keyset (get selected-keysets operation)]
      (when-not (= keyset (set (keys instruction)))
        (reject! :non-canonical-selected-instruction instruction))
      operation)))

(defn- ->mir-instruction [target instruction]
  (if (= :mir/label (:mir/op instruction))
    instruction
    (case (:mc/op instruction)
      :mc/instruction
      (let [operation (selected-operation target instruction)]
        (-> instruction
            (dissoc :mc/op :mc/encoding)
            (assoc :mir/op (keyword "mir" (name operation)))))

      :mc/branch-zero
      (do
        (when-not (= (get operation-keysets :mc/branch-zero)
                     (set (keys instruction)))
          (reject! :non-canonical-branch instruction))
        {:mir/op :mir/branch-zero :mir/test (:mc/test instruction)
         :mir/target (:mc/target instruction)})

      :mc/jump
      (do
        (when-not (= (get operation-keysets :mc/jump) (set (keys instruction)))
          (reject! :non-canonical-jump instruction))
        {:mir/op :mir/jump :mir/target (:mc/target instruction)})

      (reject! :unknown-operation instruction))))

(defn- validate-v2!
  [{:mc/keys [version target frame-slots instructions] :as program}]
  (when-not (and (map? program)
                 (= #{:mc/version :mc/target :mc/frame-slots :mc/instructions}
                    (set (keys program)))
                 (= 2 version)
                 (contains? mir/targets target)
                 (integer? frame-slots)
                 (<= 0 frame-slots 4095)
                 (vector? instructions))
    (reject! :non-canonical-program program))
  (let [mir-instructions (mapv #(->mir-instruction target %) instructions)]
    (mir/validate! {:mir/version 1 :mir/target target :mir/registers :physical
                    :mir/frame-slots frame-slots
                    :mir/instructions mir-instructions}))
  program)

(defn- validate-v3!
  [{:mc/keys [target entry functions] :as module}]
  (when-not (and (= #{:mc/version :mc/target :mc/entry :mc/functions}
                    (set (keys module)))
                 (= 3 (:mc/version module))
                 (contains? mir/targets target)
                 (vector? functions)
                 (seq functions))
    (reject! :non-canonical-module module))
  (let [mir-functions
        (mapv
         (fn [{:mc/keys [name arity frame-slots frame-policy instructions]
               :as function}]
           (when-not (and (= #{:mc/name :mc/arity :mc/frame-slots
                               :mc/frame-policy :mc/instructions}
                             (set (keys function)))
                          (vector? instructions))
             (reject! :non-canonical-function function))
           {:mir/name name
            :mir/arity arity
            :mir/frame-slots frame-slots
            :mir/frame-policy frame-policy
            :mir/instructions (mapv #(->mir-instruction target %) instructions)})
         functions)]
    (mir/validate! {:mir/version 3
                    :mir/target target
                    :mir/registers :physical
                    :mir/entry entry
                    :mir/functions mir-functions}))
  module)

(defn validate!
  "Validate and return a closed allocated MC program unchanged. v3 mirrors
  physical MIR function/frame ownership while retaining target encodings."
  [{:mc/keys [version] :as program}]
  (case version
    2 (validate-v2! program)
    3 (validate-v3! program)
    (reject! :unsupported-version program)))
