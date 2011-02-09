
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_HPP_

#include "apply_i_.hpp" // To avoid reentrance issues (only IF uses APPLY_I*).
#include "return.hpp"
#include "../parser/seq_next.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

// IMPORTANT: Code repeated in "if_1.hpp", ... to avoid reentrance issues.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_DEFAULT_ERR_( \
        seq_sign_err, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_missing_one_or_more_tokens_when_matching_sequence)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_PRESENT_( \
        seq_sign_err, empty_check_then_else_data) \
    BOOST_PP_IIF( /* check_macro( next(sse) ) */ \
            BOOST_PP_TUPLE_ELEM(5, 1, empty_check_then_else_data) \
            ( BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) ), \
        BOOST_PP_TUPLE_ELEM(5, 2, empty_check_then_else_data) /* then_macro */ \
    , \
        BOOST_PP_TUPLE_ELEM(5, 3, empty_check_then_else_data) /* else_macro */ \
    )(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(5, 4, empty_check_then_else_data) /* data */)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY_( \
        seq_sign_err, empty_check_then_else_data) \
    /* empty_macro(sse , data) */ \
    BOOST_PP_TUPLE_ELEM(5, 0, empty_check_then_else_data) \
            ( seq_sign_err, \
              BOOST_PP_TUPLE_ELEM(5, 4, empty_check_then_else_data) /* data */ )

// Public API.

// If err, forward err; Else if empty, `empty_macro(sse, data)`;
// Else if `check_macro(next(sse))`, `then_macro(sse, data)`;
// Else `else_macro(sse, data)`.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY( \
        seq_sign_err, empty_macro, check_macro, then_macro, else_macro, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_I(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_PRESENT_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY_, \
            (empty_macro, check_macro, then_macro, else_macro, data) )

// If err, forward error; Else if empty, default error;
// Else if `check_macro(next(sse))`, `then_macro(sse, data)`;
// Else `else_macro(sse, data)`.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF( \
        seq_sign_err, check_macro, then_macro, else_macro, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_DEFAULT_ERR_, \
            check_macro, then_macro, else_macro, data)

#endif // #include guard
 
