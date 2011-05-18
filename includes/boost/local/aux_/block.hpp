
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_BLOCK_HPP_
#define BOOST_LOCAL_AUX_BLOCK_HPP_

#include "symbol.hpp"

// Users can exit a local block (and not enclosing function) using `return;`.
#define BOOST_LOCAL_AUX_BLOCK( \
        local_function_params_macro, parenthesized_binding) \
    void local_function_params_macro(parenthesized_binding) \
    /* user block code `{ ... }` here */

#define BOOST_LOCAL_AUX_BLOCK_END(id) \
    /* user block code `{ ... }` here */ \
    /* ends the local function declaration */ \
    BOOST_LOCAL_FUNCTION_NAME(inline /* for optimization (never as tparam) */ \
            BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME(id)) \
    /* call local funciton and execute block's code imediately */ \
    BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME(id)(); \

#endif // #include guard

