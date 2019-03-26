(module (route-mux route)
  (
   ;; api
   route-uri)

  (import (chicken base)
          (scheme)
          (srfi 14)
          comparse
          (only matchable match))

  (include "lib/route.scm"))
