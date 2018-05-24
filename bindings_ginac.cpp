#include <iostream>
#include <sstream>
#include "bindings_ginac.h"

#ifdef DEBUG
#define DEBUG_PRINT(x) std::cout << x << std::endl
#else
#define DEBUG_PRINT(x)
#endif

using namespace GiNaC;

ex *ginac_ex_new()
{
    DEBUG_PRINT("ex +1");
    return new ex;
}

ex *ginac_ex_new_from_basic(const basic &other)
{
    DEBUG_PRINT("ex +1");
    return new ex(other);
}

ex *ginac_ex_new_from_int(int i)
{
    DEBUG_PRINT("ex +1");
    return new ex(i);
}

ex *ginac_ex_subs_int(int i, const ex &this_ex, const symbol &s)
{
    DEBUG_PRINT("ex +1");
    return new ex(this_ex.subs(s == i));
}

void ginac_ex_free(ex *this_ex)
{
    DEBUG_PRINT("ex -1");
    delete this_ex;
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
    DEBUG_PRINT("symbol -1");
    delete this_basic;
}

symbol *ginac_symbol_new(const char *name)
{
    DEBUG_PRINT("symbol +1");
    return new symbol(name);
}

symbol *ginac_symbol_static()
{
    static symbol *s = new symbol;
    return s;
}

GiNaC::ex *ginac_ex_neg(const GiNaC::ex &this_ex)
{
    DEBUG_PRINT("ex +1");
    return new ex(mul(-1, this_ex));
}

ex *ginac_ex_abs(const ex &this_ex)
{
    DEBUG_PRINT("ex +1");
    return new ex(abs(this_ex));
}

ex *ginac_ex_signum(const ex &this_ex)
{
    DEBUG_PRINT("ex +1");
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
    DEBUG_PRINT("ex +1");
    return new ex(sqrt(this_ex));
}

ex *ginac_add(const ex &lh, const ex &rh)
{
    DEBUG_PRINT("ex +1");
    return new ex(add(lh, rh));
}

ex *ginac_mul(const ex &lh, const ex &rh)
{
    DEBUG_PRINT("ex +1");
    return new ex(mul(lh, rh));
}

ex *ginac_div(const ex &lh, const ex &rh)
{
    DEBUG_PRINT("ex +1");
    return new ex(lh/rh);
}

ex *ginac_diff(int nth, const ex &this_ex, const symbol &x)
{
    DEBUG_PRINT("ex +1");
    return new ex(this_ex.diff(x, nth));
}

ex *ginac_pow(const ex &b, const ex &p)
{
    DEBUG_PRINT("ex +1");
    return new ex(pow(b, p));
}

ex *ginac_factorial(int n)
{
    DEBUG_PRINT("ex +1");
    return new ex(factorial(n));
}

relational *ginac_relation_eq_new(const ex &lh, const ex &rh)
{
    DEBUG_PRINT("relational +1");
    return new relational(lh, rh);
}
