(module (route-mux path)
  (
   ;; conversion
   route->path
   ;; searching
   path-search
   ;; mutation
   path-insert!)

  (import (chicken base)
          (scheme)
          (only (srfi 1) fold)
          (only (srfi 13) string-index string-copy substring/shared)
          (only matchable match)
          (only (route-mux radix)
                make-edge
                edge-label edge-node
                make-node
                node-value node-edges
                node-search node-insert-at!))

  (include "lib/path.scm"))
