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

ex *ginac_ex_new_from_int(int i)
{
    return new ex(i);
}

ex *ginac_ex_subs(const ex &this_ex, const symbol &s, const ex &other)
{
    return new ex(this_ex.subs(s == other));
}

ex *ginac_ex_subs_int(const ex &this_ex, const symbol &s, int i)
{
    return new ex(this_ex.subs(s == i));
}

void ginac_ex_free(ex *this_ex)
{
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
    delete this_basic;
}

symbol *ginac_symbol_new(const char *name)
{
    return new symbol(name);
}

add *ginac_add_new(const ex &lh, const ex &rh)
{
    return new add(lh, rh);
}
