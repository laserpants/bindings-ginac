#ifndef BINDINGS_GINAC_H
#define BINDINGS_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::ex *ginac_ex_new();
GiNaC::ex *ginac_ex_new_from_int(int i);
GiNaC::ex *ginac_ex_new_from_symbol(GiNaC::symbol *s);
void ginac_ex_free(GiNaC::ex *this_ex);
char *ginac_ex_to_str(GiNaC::ex *this_ex);
void ginac_ex_print(GiNaC::ex *this_ex);

#ifdef __cplusplus
}
#endif

#endif // BINDINGS_GINAC_H
