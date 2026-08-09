# kotoba-codegen

MIR-to-machine-code layout contracts for Kotoba.

The initial `kotoba.codegen.layout` contract performs deterministic two-pass
layout for x86-64 and AArch64 label/relative-branch tokens. Backends mix their
ordinary encoded bytes with these closed tokens; codegen assigns label offsets,
checks architectural ranges/alignment, and asks the backend to encode the final
displacement.

```clojure
(require '[kotoba.codegen.layout :as layout])

(def target :example.label/done)
(def tokens [(layout/relative-branch :x86-64/jmp-rel32 target)
             [0x90]
             (layout/label target)])
```

## Boundary

- owned here: canonical layout tokens, instruction widths, PC bias,
  architectural displacement range/alignment checks, deterministic two-pass
  resolution, and encoder-width verification
- owned by `kotoba-mir`: target MIR and register allocation
- owned by target backends: instruction selection, ordinary instruction bytes,
  branch opcode encoding, ABI/runtime policy
- owned by `kotoba-object`: object-container records

## Development

```sh
clojure -M:test
```
