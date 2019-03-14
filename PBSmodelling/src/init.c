#include "fib.h"
#include "fib2.h"
#include "parser.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[]  = {
    {"fibonacci",  (DL_FUNC) &fibonacci, 3},        /* fib.c:      1 */
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"fibonacci2",    (DL_FUNC) &fibonacci2, 2},    /* fib2.c:     4 */
    {"strToList",     (DL_FUNC) &strToList, 4},     /* parser.c: 158 */
    {"strToVector",   (DL_FUNC) &strToVector, 4},   /* parser.c: 305 */
    {"stripComments", (DL_FUNC) &stripComments, 1}, /* parser.c: 396 */
    {NULL, NULL, 0}
};

void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_PBSmodelling(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

