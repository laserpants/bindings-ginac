#ifndef BINDINGS_GINAC_H
#define BINDINGS_GINAC_H

#include <ginac/ginac.h>

#ifdef __cplusplus
extern "C" {
#endif

GiNaC::ex *ginac_ex_new();
GiNaC::ex *ginac_ex_new_from_basic(const GiNaC::basic &other);
GiNaC::ex *ginac_ex_new_from_int(const int i);
GiNaC::ex *ginac_ex_new_from_double(const double d);
GiNaC::ex *ginac_ex_new_from_relation_eq(const GiNaC::ex &lh, const GiNaC::ex &rh);

GiNaC::ex *ginac_ex_subs(const GiNaC::ex &this_ex, const GiNaC::relational &rel);
GiNaC::ex *ginac_ex_subs_int(const int i, const GiNaC::ex &this_ex, const GiNaC::symbol &s);

void ginac_ex_free(GiNaC::ex *this_ex);

bool ginac_ex_equal(const GiNaC::ex &this_ex, const GiNaC::ex &that_ex);
int ginac_ex_compare(const GiNaC::ex &this_ex, const GiNaC::ex &that_ex);
bool ginac_ex_is_numeric(const GiNaC::ex &this_ex);

double ginac_ex_to_double(const GiNaC::ex &this_ex);
int ginac_ex_to_int(const GiNaC::ex &this_ex);
char *ginac_ex_to_str(const GiNaC::ex &this_ex);
void ginac_ex_print(const GiNaC::ex &this_ex);

void ginac_basic_free(GiNaC::basic *this_basic);

GiNaC::symbol *ginac_symbol_new(const char *name);

GiNaC::ex *ginac_ex_neg(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_abs(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_signum(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_sqrt(const GiNaC::ex &this_ex);
GiNaC::ex *ginac_ex_add(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_ex_sub(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_ex_mul(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_ex_div(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::ex *ginac_ex_diff(int nth, const GiNaC::ex &this_ex, const GiNaC::symbol &x);
GiNaC::ex *ginac_ex_pow(const GiNaC::ex &b, const GiNaC::ex &p);
GiNaC::ex *ginac_ex_factorial(const int n);
GiNaC::ex *ginac_ex_series(const GiNaC::ex &this_ex, const GiNaC::relational &rel, const int n);
GiNaC::ex *ginac_ex_coeff(const GiNaC::ex &this_ex, const GiNaC::ex &s, const int n);

GiNaC::add *ginac_add(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::mul *ginac_mul(const GiNaC::ex &lh, const GiNaC::ex &rh);
GiNaC::power *ginac_pow(const GiNaC::ex &b, const GiNaC::ex &p);
GiNaC::function ginac_factorial(const int n);
GiNaC::relational *ginac_relation_eq_new(const GiNaC::ex &lh, const GiNaC::ex &rh);

GiNaC::numeric *ginac_numeric_new_from_int(const int i);
GiNaC::numeric *ginac_numeric_new_from_double(const double d);
GiNaC::numeric *ginac_numeric_new_from_str(const char *c);

#ifdef __cplusplus
}
#endif

#endif // BINDINGS_GINAC_H
