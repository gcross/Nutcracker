
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_BLOCK_HPP_
#define BOOST_LOCAL_AUX_BLOCK_HPP_

#include "symbol.hpp"

// Users can exit a local block (and not enclosing function) using `return;`.
#define BOOST_LOCAL_AUX_BLOCK( \
        local_function_macro, parenthesized_binding) \
    { /* start new code block to reuse same local function name */ \
        local_function_macro( \
        (void) (BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME)( \
                parenthesized_binding \
            ) \
        ) /* user block code `{ ... }` here */

#endif // #include guard

