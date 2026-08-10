(ns kotoba.codegen.relocation
  "Closed target-specific relocation request contract.

  Code generators own when a relocation is required. Object encoders own its
  container bits. This namespace fixes the data exchanged at that boundary."
  (:require [clojure.string :as str]))

(def version 1)

(def macho-types
  {:aarch64 #{:aarch64/unsigned :aarch64/branch26
              :aarch64/page21 :aarch64/pageoff12}
   :x86-64 #{:x86-64/unsigned :x86-64/signed :x86-64/branch
             :x86-64/got-load :x86-64/got :x86-64/tlv}})

(defn- reject! [problem request]
  (throw (ex-info (str "relocation rejected: " (name problem))
                  {:phase :relocation :problem problem :request request})))

(defn validate!
  "Validate and return one canonical external relocation request."
  [{:reloc/keys [version target section offset type symbol] :as request}]
  (when-not (and (map? request)
                 (= #{:reloc/version :reloc/target :reloc/section
                      :reloc/offset :reloc/type :reloc/symbol}
                    (set (keys request)))
                 (= 1 version)
                 (contains? macho-types target)
                 (integer? section) (pos? section)
                 (integer? offset) (<= 0 offset 0xffffffff)
                 (contains? (get macho-types target) type)
                 (string? symbol) (str/starts-with? symbol "_")
                 (<= 2 (count symbol) 255)
                 (every? #(<= 0x20 (int %) 0x7e) symbol))
    (reject! :non-canonical-request request))
  request)

(defn ->macho
  "Project a validated request into a Mach-O section relocation record."
  [request]
  (let [{:reloc/keys [offset type symbol]} (validate! request)]
    {:offset offset :type type :symbol symbol}))
