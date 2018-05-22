#include <iostream>
#include <sstream>
#include "bindings_ginac.h"

using namespace GiNaC;

ex *ginac_ex_new()
{
    return new ex;
}

ex *ginac_ex_new_from_int(int i)
{
    return new ex(i);
}

ex *ginac_ex_new_from_symbol(symbol *s)
{
    return new ex(*s);
}

void ginac_ex_free(ex *this_ex)
{
    delete this_ex;
}

char *ginac_ex_to_str(ex *this_ex)
{
    std::ostringstream out;
    out << *this_ex;
    return strdup(out.str().c_str());
}

void ginac_ex_print(ex *this_ex)
{
    std::cout << *this_ex << std::endl;
}
