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

## Test

```sh
clojure -M:test
clojure -M:lint
```
