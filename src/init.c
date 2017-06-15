#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void C_rss2Dloop(void *, void *, void *, void *, void *, void *, void *);
extern void C_rss3Dloop(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"C_rss2Dloop", (DL_FUNC) &C_rss2Dloop,  7},
    {"C_rss3Dloop", (DL_FUNC) &C_rss3Dloop, 11},
    {NULL, NULL, 0}
};

void R_init_runOPM(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
