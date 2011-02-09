
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_NAME_HPP_
#define BOOST_DETAIL_PP_SIGN_NAME_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/keyword/is_operator.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/cat.hpp>

// Handle special name `( operator (symbol, word) )` for operators.
// These are NOT local macros -- do NOT #undef them! Mixed case
// names are necesseray because they must be prefixed by "operator".

#define BOOST_DETAIL_PP_SIGN_NAME_AS_WORD_operator(symbol, word) \
    /* use ## instead of PP_CAT here for proper macro expansion */ \
    operator_ ## word /* e.g., operator_equal */

#define BOOST_DETAIL_PP_SIGN_NAME_AS_SYMBOL_operator(symbol, word) \
    operator symbol /* e.g., operator== */

#define BOOST_DETAIL_PP_SIGN_NAME_WORD(name) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_OPERATOR(name), \
        /* expand to `PP_SIGN_NAME_AS_WORD_operator()` */ \
        BOOST_PP_CAT(BOOST_DETAIL_PP_SIGN_NAME_AS_WORD_, name) \
    , /* else */ \
        name \
    ) /* endif */

// Get name or symbol.

#define BOOST_DETAIL_PP_SIGN_NAME_(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_NAME_INDEX, sign) 

#define BOOST_DETAIL_PP_SIGN_NAME(sign) \
    BOOST_DETAIL_PP_SIGN_NAME_WORD(BOOST_DETAIL_PP_SIGN_NAME_(sign))
    
#define BOOST_DETAIL_PP_SIGN_NAME_IS_OPERATOR(sign) \
    BOOST_DETAIL_PP_KEYWORD_IS_OPERATOR(BOOST_DETAIL_PP_SIGN_NAME_(sign))

#define BOOST_DETAIL_PP_SIGN_NAME_SYMBOLIC_TOKEN(name_token) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_OPERATOR(name_token), \
        /* expand to `PP_SIGN_NAME_AS_SYMBOL_operator()` */ \
        BOOST_PP_CAT(BOOST_DETAIL_PP_SIGN_NAME_AS_SYMBOL_, name_token) \
    , /* else */ \
        name_token \
    ) /* endif */

#define BOOST_DETAIL_PP_SIGN_NAME_SYMBOLIC(sign) \
    BOOST_DETAIL_PP_SIGN_NAME_SYMBOLIC_TOKEN( \
            BOOST_DETAIL_PP_SIGN_NAME_(sign))

#endif // #include guard

