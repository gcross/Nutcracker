
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_HPP_

#include "../../keyword/const_bind.hpp"
#include <boost/detail/preprocessor/keyword/this.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
// IS_UNARY is part of details because it does not work on BCC compiler.
#include <boost/preprocessor/detail/is_unary.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_STRIP_PAREN_(\
        type) type

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITH_TYPE_( \
        tokens) \
    BOOST_PP_EXPAND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_STRIP_PAREN_ \
            BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_FRONT(tokens)) \
    BOOST_PP_EMPTY /* always trail EMPTY because bind type is optional */

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_STRIP_( \
        type) /* must expand to nothing */

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITHOUT_TYPE_( \
        tokens) \
    BOOST_PP_EXPAND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_TYPE_STRIP_ \
            BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_FRONT(tokens))

// PUBLIC //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_HAS_TYPE(tokens) \
    BOOST_PP_IS_UNARY(BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_FRONT( \
            tokens))

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITH_TYPE(tokens) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_HAS_TYPE( \
            tokens), \
        BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITH_TYPE_ \
    , \
        BOOST_PP_EMPTY \
        BOOST_PP_TUPLE_EAT(1) \
    )(tokens)

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITHOUT_TYPE( \
        tokens) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_HAS_TYPE( \
            tokens), \
        BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITHOUT_TYPE_ \
    , \
        BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_FRONT \
    )(tokens)

// Private macro (but needs public macros above).
#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_THIS_TYPE_( \
        tokens) \
    BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_WITH_TYPE( \
            /* can't use `THIS_REMOVE_BACK` because `tokens` contains */ \
            /* multiple tokens (and not just one token) so `IS_THIS_BACK` */ \
            /* does not work -- but we know tokens ends with this if we */ \
            /* here so we can manually force the removal using `CAT` */ \
            BOOST_PP_CAT(tokens, _BOOST_DETAIL_PP_KEYWORD_THIS_REMOVE)) \
    /* do not append PP_EMPTY because ANY_BIND_WITH_TYPE macro above */ \
    /* already appends it */

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_THIS_TYPE(tokens) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_HAS_TYPE(\
            tokens), \
        BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS_ANY_BIND_THIS_TYPE_ \
    , \
        BOOST_PP_EMPTY \
        BOOST_PP_TUPLE_EAT(1) \
    )(tokens)

#endif // #include guard

