(ns kotoba.codegen.mc
  "Closed allocated machine-code program contract.

  MC retains MIR operands and bounded spill-frame ownership after target
  selection and allocation while making the target encoding choice explicit.
  Backends own the bytes for each encoding; this namespace owns the admitted
  data shape."
  (:require [kotoba.mir :as mir]))

(def version 2)

(def ^:private selected-keysets
  {:argument #{:mc/op :mc/encoding :mir/dst :mir/index}
   :constant #{:mc/op :mc/encoding :mir/dst :mir/value}
   :add #{:mc/op :mc/encoding :mir/dst :mir/left :mir/right}
   :spill-load #{:mc/op :mc/encoding :mir/dst :mir/slot}
   :spill-store #{:mc/op :mc/encoding :mir/src :mir/slot}
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

(defn validate!
  "Validate and return a closed allocated MC v2 program unchanged."
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
