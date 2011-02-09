
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_HPP_

#include "../parser/seq_next.hpp"
#include "../parser/seq_skip.hpp"
#include "return.hpp"
#include "apply_1.hpp" // To avoid reentrancy issues.
#include "apply_2.hpp" // To avoid reentrancy issues.
#include "if_1.hpp" // To avoid reentrancy issues.
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/control/iif.hpp>

// Parse required tagged tokens an in `(tag)( tokens )`.

// Called if empty seq, tag not found, empty tagged seq, etc.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_ERR_( \
        seq_sign_err, istag_isempty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(3, 2, istag_isempty_error) /* error */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_NONEMPTYSEQ_( \
        seq_sign_err, istag_isempty_error) \
    /* in (tag)( seq ) -- found tag and non-empty seq, return seq */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_RETURN_FWD(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_SOMESEQ_( \
        seq_sign_err, istag_isempty_error) \
    /* on C99, found seq could still be empty `()` */ \
    BOOST_PP_IIF( /* is_empty(next(sse)) */ \
            BOOST_PP_TUPLE_ELEM(3, 1, istag_isempty_error) \
            ( BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_ERR_ /* empty tokens */\
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_NONEMPTYSEQ_ \
    )(seq_sign_err, istag_isempty_error)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_YESTAG_( \
        seq_sign_err, istag_isempty_error) \
    /* in (tag)( seq ) -- tag found, now inspect seq */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_2( \
            /* need to APPLY again because of SKIP */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err), \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_SOMESEQ_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_ERR_, /*empty after*/\
            istag_isempty_error)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_( \
        seq_sign_err, istag_isempty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_1(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(3, 0, istag_isempty_error), /* is_tag_macro */ \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_YESTAG_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_ERR_ /* no tag found */ \
    , istag_isempty_error)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ(seq_sign_err, \
        is_tag_macro, is_empty_macro, error) \
    /* in (tag)( seq ) -- first look for tag */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ_ERR_, /* empty seq */ \
            ( is_tag_macro, is_empty_macro, error ) )

#endif // #include guard

