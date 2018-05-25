#include <iostream>
#include <sstream>
#include "bindings_ginac.h"

using namespace GiNaC;

ex *ginac_ex_new()
{
    return new ex;
}

ex *ginac_ex_new_from_basic(const basic &other)
{
    return new ex(other);
}

ex *ginac_ex_new_from_int(const int i)
{
    return new ex(i);
}

ex *ginac_ex_new_from_double(const double d)
{
    return new ex(d);
}

ex *ginac_ex_new_from_relation_eq(const ex &lh, const ex &rh)
{
    return new ex(relational(lh, rh));
}

ex *ginac_ex_subs(const ex &this_ex, const relational &r)
{
    try {
        return new ex(this_ex.subs(r));
    } catch (pole_error e) {
        return new ex(fail());
    }
}

ex *ginac_ex_subs_int(const int i, const ex &this_ex, const symbol &s)
{
    try {
        return new ex(this_ex.subs(s == i));
    } catch (pole_error e) {
        return new ex(fail());
    }
}

void ginac_ex_free(ex *this_ex)
{
    delete this_ex;
}

bool ginac_ex_equal(const ex &this_ex, const ex &that_ex)
{
    return (this_ex == that_ex);
}

int ginac_ex_compare(const ex &this_ex, const ex &that_ex)
{
    return this_ex.compare(that_ex);
}

bool ginac_ex_is_numeric(const ex &this_ex)
{
    return is_a<numeric>(this_ex);
}

double ginac_ex_to_double(const ex &this_ex)
{
    return ex_to<numeric>(this_ex).to_double();
}

int ginac_ex_to_int(const ex &this_ex)
{
    return ex_to<numeric>(this_ex).to_int();
}

char *ginac_ex_to_str(const ex &this_ex)
{
    std::ostringstream out;
    out << this_ex;
    return strdup(out.str().c_str());
}

void ginac_ex_print(const ex &this_ex)
{
    std::cout << this_ex << std::endl;
}

void ginac_basic_free(basic *this_basic)
{
    delete this_basic;
}

symbol *ginac_symbol_new(const char *name)
{
    return new symbol(name);
}

symbol *ginac_symbol_static()
{
    static symbol s;
    return &s;
}

ex *ginac_ex_neg(const ex &this_ex)
{
    return new ex(mul(-1, this_ex));
}

ex *ginac_ex_abs(const ex &this_ex)
{
    return new ex(abs(this_ex));
}

ex *ginac_ex_signum(const ex &this_ex)
{
    if (this_ex > 0) {
        return new ex(1);
    } else if (this_ex < 0) {
        return new ex(-1);
    } else {
        return new ex(0);
    }
}

ex *ginac_ex_sqrt(const ex &this_ex)
{
    return new ex(sqrt(this_ex));
}

ex *ginac_ex_add(const ex &lh, const ex &rh)
{
    return new ex(add(lh, rh));
}

ex *ginac_ex_mul(const ex &lh, const ex &rh)
{
    return new ex(mul(lh, rh));
}

ex *ginac_ex_div(const ex &lh, const ex &rh)
{
    try {
        return new ex(lh/rh);
    } catch (pole_error e) {
        return new ex(fail());
    }
}

ex *ginac_ex_diff(int nth, const ex &this_ex, const symbol &x)
{
    return new ex(this_ex.diff(x, nth));
}

ex *ginac_ex_pow(const ex &b, const ex &p)
{
    return new ex(pow(b, p));
}

ex *ginac_ex_factorial(const int n)
{
    return new ex(factorial(n));
}

ex *ginac_ex_series(const ex &this_ex, const relational &r, const int n)
{
    return new ex(this_ex.series(r, n));
}

ex *ginac_ex_coeff(const ex &this_ex, const ex &s, const int n)
{
    return new ex(this_ex.coeff(s, n));
}

ex *ginac_ex_coeff_symbol(const int n, const ex &this_ex, const symbol &s)
{
    return new ex(this_ex.coeff(s, n));
}

add *ginac_add(const ex &lh, const ex &rh)
{
    return new add(lh, rh);
}

mul *ginac_mul(const ex &lh, const ex &rh)
{
    return new mul(lh, rh);
}

power *ginac_pow(const ex &b, const ex &p)
{
    return new power(b, p);
}

function ginac_factorial(const int n)
{
    return factorial(n);
}

relational *ginac_relation_eq_new(const ex &lh, const ex &rh)
{
    return new relational(lh, rh);
}
