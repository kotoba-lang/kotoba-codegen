# kotoba-codegen

MIR-to-machine-code contracts for Kotoba.

`kotoba.codegen.mc` owns the closed allocated MC v2/v3 program shapes between MIR
allocation and target byte encoders. It verifies target/encoding agreement,
exact instruction keysets, physical-register profiles, bounded spill frames,
closed control flow, and the physical-register `move` selected by MIR's
deterministic parallel-copy scheduler. MC v3 additionally retains independent
function frames and selected module-local scalar calls.

`kotoba.codegen.relocation` owns the closed target-specific relocation request
passed from instruction selection/layout to object encoders. Requests name the
target, one-based section, bounded offset, typed relocation kind, and external
symbol; container-specific bit packing remains in `kotoba-object`.

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

- owned here: canonical layout tokens, typed relocation requests, instruction
  widths, PC bias,
architectural displacement range/alignment checks, deterministic two-pass
resolution, direct-call widths, encoder-width verification, and the allocated
MC program schema
- owned by `kotoba-mir`: target MIR and register allocation
- owned by target backends: instruction selection, ordinary instruction bytes,
  branch opcode encoding, ABI/runtime policy
- owned by `kotoba-object`: object-container records

## Development

```sh
clojure -M:test
```
