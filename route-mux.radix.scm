(module (route-mux radix)
  (
   ;; constructors
   make-node
   make-edge
   ;; selectors
   node-value
   node-edges
   edge-label
   edge-node
   ;; predicates
   node?
   edge?
   ;; searching
   node-search
   ;; mutators
   node-insert!
   ;; internals
   sorted-insert
   sorted-edge-insert)

  (import (chicken base)
          (scheme)
          (only (chicken format) fprintf)
          (only (srfi 13) string< string-prefix-length substring/shared)
          (only matchable match)
          (only vector-lib vector-unfold))

  (include "lib/radix.scm"))
