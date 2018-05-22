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

symbol *ginac_symbol_new(const char *name)
{
    return new symbol(name);
}

void ginac_symbol_free(symbol *this_symbol)
{
    delete this_symbol;
}
