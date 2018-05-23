#ifndef BINDINGS_GINAC_H
#define BINDINGS_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::ex *ginac_ex_new();
GiNaC::ex *ginac_ex_new_from_basic(const GiNaC::basic &other);
GiNaC::ex *ginac_ex_new_from_int(int i);
GiNaC::ex *ginac_ex_subs(const GiNaC::ex &this_ex, const GiNaC::symbol &s, const GiNaC::ex &other);
GiNaC::ex *ginac_ex_subs_int(const GiNaC::ex &this_ex, const GiNaC::symbol &s, int i);
void ginac_ex_free(GiNaC::ex *this_ex);
char *ginac_ex_to_str(const GiNaC::ex &this_ex);
void ginac_ex_print(const GiNaC::ex &this_ex);
void ginac_basic_free(GiNaC::basic *this_basic);
GiNaC::symbol *ginac_symbol_new(const char *name);
GiNaC::ex *ginac_ex_neg(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_abs(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_signum(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_sqrt(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_add(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_mul(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_div(const GiNaC::ex &lh, const GiNaC::ex &rh);

#ifdef __cplusplus
}
#endif

#endif // BINDINGS_GINAC_H
