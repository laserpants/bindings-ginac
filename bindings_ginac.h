#ifndef BINDINGS_GINAC_H
#define BINDINGS_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::ex *ginac_ex_new();
GiNaC::ex *ginac_ex_new_from_basic(const GiNaC::basic &other);
GiNaC::ex *ginac_ex_new_from_int(int i);
void ginac_ex_free(GiNaC::ex *this_ex);
char *ginac_ex_to_str(const GiNaC::ex &this_ex);
void ginac_ex_print(const GiNaC::ex &this_ex);
GiNaC::symbol *ginac_symbol_new(const char *name);
void ginac_symbol_free(GiNaC::symbol *this_symbol);

#ifdef __cplusplus
}
#endif

#endif // BINDINGS_GINAC_H
