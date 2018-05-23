#include <iostream>
#include <sstream>
#include "bindings_ginac.h"

#ifdef DEBUG
#define DEBUG_PRINT(x) std::cout << "GiNaC::" << x << std::endl
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

ex *ginac_ex_subs(const ex &this_ex, const symbol &s, const ex &other)
{
    DEBUG_PRINT("ex +1");
    return new ex(this_ex.subs(s == other));
}

ex *ginac_ex_subs_int(const ex &this_ex, const symbol &s, int i)
{
    DEBUG_PRINT("ex +1");
    return new ex(this_ex.subs(s == i));
}

void ginac_ex_free(ex *this_ex)
{
    DEBUG_PRINT("ex -1");
    delete this_ex;
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

ex *ginac_add(const ex &lh, const ex &rh)
{
    DEBUG_PRINT("ex +1");
    return new ex(add(lh, rh));
}
