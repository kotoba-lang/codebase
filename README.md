# codebase

Content-addressed semantic definition identity, verified local persistence, and
hash-native authoring and evaluation for Kotoba code.

This repository owns canonical semantic blocks, namespace commits, closure
transfer, and pure build-cache keys. It does not parse source, compile or
execute components, serve network traffic, or grant runtime authority.

## What is addressed by hash

| Namespace | Owns |
| --- | --- |
| `semantic-code` | lowering a checked definition to canonical DAG-CBOR and its CIDv1 |
| `store` | immutable verified block persistence, namespace heads, CAS, merge |
| `ir` | reading blocks back: link decoding, reference traversal, CID substitution |
| `evaluator` | evaluating a definition by hydrating its dependencies BY CID |
| `authoring` | scratch source in, namespace commit out, with update propagation |
| `render` | projecting a stored definition back to readable source (`view`) |
| `names` | name / hash / abbreviation lookup, dependents, dependencies |
| `fetch` | bounded, verified hydration of a closure from an injected transport |
| `typed-code` | definition identity computed from the compiler's checked KIR |
| `typed-eval` | executing that KIR through the language oracle, hydrated by CID |
| `value-runtime` | bounded run-local Handle ↔ ValueCID resolution over canonical immutable values |

Two identity layers exist and they are not interchangeable. `semantic-code`
hashes an IR this repository normalizes for itself; `typed-code` hashes the
**checked KIR** the compiler produces and the backends consume, and binds the
typed interface (parameter types, result, declared effects) alongside the body.
Prefer `typed-code` for anything that will also be compiled: it is the layer
where what a definition IS and what gets executed are the same object, and its
language coverage is the compiler's rather than a hand-maintained subset.

Alpha-normalization in `typed-code` is verified rather than assumed. KIR has
five binding forms, and a sixth added later would silently leave a
source-chosen name inside a hash — so a binder that survives renaming fails the
compile closed instead.

Three properties hold by construction and are covered by tests:

- **A definition runs from its hash alone.** `evaluator/invoke` reads the block,
  hydrates every dependency by CID, and never consults a file, a namespace, or
  a name. Dropping every binding that points at a definition does not change how
  it runs; a missing dependency fails closed instead of falling back to a name.
- **An update propagates by rewriting, not recompiling.** `authoring` compiles a
  scratch buffer against the names a namespace currently selects, then rewrites
  each dependent's stored IR so its dependency CID points at the new definition.
  Dependent *source* is never required to still exist.
- **Received bytes are checked before they are believed.** `fetch` verifies that
  bytes hash to the CID that was requested *and* that they are the canonical
  encoding of what they decode to, so an untrusted provider can be wrong but not
  convincing.

Names are a lookup layer, not identity. A rename changes the namespace commit
and no definition CID; an ambiguous hash abbreviation is rejected rather than
resolved to whichever candidate sorts first.

Values use the same separation at runtime: `ValueCID` is a portable logical
address, `Handle` is a run-local bounded slot, and `Cap` remains authority.
`value-runtime` interns or hydrates canonical `kotoba.value.v1` blocks, rejects
forged handles, never reuses a released handle, and keeps runtime collection
separate from persistent CAS retention. It is the reference host kernel; Wasm
and native callback ABI adoption is tracked separately and must preserve this
contract rather than expose CIDs on the hot path.

`value-runtime-abi` fixes the shared backend boundary as five synchronous
operations over canonical bytes, CID text, and scalar handles. Wasm
typed-`externref` imports and native context callbacks are transports for this
ABI; they may not reinterpret the bytes or derive authority from a CID.

## Test

```sh
clojure -M:test
clojure -M:lint
```
