
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_HPP_

#include "parse_params_/is_void.hpp"
#include "parse_params_/seq.hpp"
#include "parsed_params_/nil.hpp"
#include <boost/preprocessor/control/iif.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_NONE_(unused) \
    BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_NIL

// PUBLIC //

// parenthesized_params: Empty ``, or `void`, or `(void)` for no params
//  Or, `{([auto | register] param_type param_name)[(default default_value)] 
//  | ([const] bind [&] bind_name}+` where `bind_name` can be `this` (but not
//  `&this` as usual in C++).
#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS(parenthesized_params) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_IS_VOID( \
            parenthesized_params), \
        BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_NONE_ \
    , \
        BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_SEQ \
    )(parenthesized_params)

#endif // #include guard

