
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSER_ERR_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_HPP_

#include "../aux_/parser/indexes.hpp"
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/logical/bool.hpp>

// NOTE: I tried to use the tokens `[message] BOOST_PP_EMPTY` instead of the
// 2-tuple to represent errors (where there was an error iff message was
// present and therefore the expanded token expression was not empty). However,
// this caused macros expansion problems in MSVC. It is actually specified in
// Boost.Preprocessor documentation that the behavior of some BOOST_PP_...
// macros is undefined when BOOST_PP_EMPTY is explicitly passed as a macro
// parameter. Therefore, this use of BOOST_PP_EMPTY to represent errors must be
// avoided. Plus, it is probably more efficient to check if err_code != 0 than
// if the token expression is EMPTY to see if there was an error.

// Error is a 2-tuple `(code, message)` where there is an error iff code != 0.
#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_CODE_INDEX_  0
#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_MSG_INDEX_   1
#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_INDEX_MAX_   2

// Public API.

// This API could be made private (i.e., moved into "aux_/") because users
// should always access the error via the other macros below (code, message,
// etc). However, it is provided here for maximum flexibility to the users
// (and because it is not a secret that `seq_sign_err` has an error component).
#define BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_AUX_PARSER_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_INDEX, seq_sign_err)

#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_CODE(seq_sign_err) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_PARSER_ERR_INDEX_MAX_, \
            BOOST_DETAIL_PP_SIGN_PARSER_ERR_CODE_INDEX_, \
            BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err) \
    BOOST_PP_BOOL(BOOST_DETAIL_PP_SIGN_PARSER_ERR_CODE(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_PARSER_ERR_MESSAGE(seq_sign_err) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_PARSER_ERR_INDEX_MAX_, \
            BOOST_DETAIL_PP_SIGN_PARSER_ERR_MSG_INDEX_, \
            BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err))

#endif // #include guard

