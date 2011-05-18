
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_HPP_

#include "../../symbol.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use this lib's ScopeExit impl.
#include <boost/detail/preprocessor/keyword/this.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/control/iif.hpp>

// PUBLIC //

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
    BOOST_PP_CAT(se_this_type, id) // Using ScopeExit prefix `se_...`.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_NAME \
    se_this // Using ScopeExit prefix `se_...`.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_RENAME(param_name) \
    /* can't use `PP_KEYWORD_IS_THIS_FRONT()` because some `param_name` */ \
    /* might start with non-alphanumeric symbol `&` (but never for `this`) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_THIS_BACK(param_name), \
        BOOST_LOCAL_AUX_SYMBOL_THIS_PARAM_NAME \
    , \
        param_name \
    )

#endif // #include guard

