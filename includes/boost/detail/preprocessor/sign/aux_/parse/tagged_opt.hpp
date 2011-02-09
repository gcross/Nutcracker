
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_HPP_

#include "../parser/seq_next.hpp"
#include "../parser/seq_skip.hpp"
#include "return.hpp"
#include "apply_1.hpp" // To avoid reentrance issues.
#include "apply_2.hpp" // To avoid reentrance issues.
#include "if_1.hpp" // To avoid reentrance issues.
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/control/iif.hpp>

// Parse optional tagged token.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_NONEMPTYSEQ_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    /* in (tag)( seq ) -- found tag and non-empty seq, return seq */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, \
            BOOST_PP_IIF(BOOST_PP_TUPLE_ELEM(5, 4, \
                    istag_notag_isempty_onempty_append01), \
                /* append no_tag_seq to handle optional */ \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) \
                BOOST_PP_TUPLE_ELEM(5, 1, \
                        istag_notag_isempty_onempty_append01) \
            , \
                /* prepend no_tag_seq to handle empty seq */ \
                BOOST_PP_TUPLE_ELEM(5, 1, \
                        istag_notag_isempty_onempty_append01) \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) \
            ))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_EMPTYSEQ_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    /* in (tag)( seq ) -- found tag but seq is empty, on_empty(sse) */ \
    BOOST_PP_TUPLE_ELEM(5, 3, istag_notag_isempty_onempty_append01)( \
            seq_sign_err)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_SOMESEQ_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    /* on C99, found seq could still be empty `()` */ \
    BOOST_PP_IIF( /* is_empty(next(sse)) */ \
            BOOST_PP_TUPLE_ELEM(5, 2, istag_notag_isempty_onempty_append01) \
            ( BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_EMPTYSEQ_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_NONEMPTYSEQ_ \
    )(seq_sign_err, istag_notag_isempty_onempty_append01)

// Tag found.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_YESTAG_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    /* in (tag)( seq ) -- tag found, now inspect seq */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_2( \
            /* need to APPLY again because of SKIP */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err), \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_SOMESEQ_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_EMPTYSEQ_, \
            istag_notag_isempty_onempty_append01)

// Tag not found.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_NOTAG_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            /* no_tag_seq */ \
            BOOST_PP_TUPLE_ELEM(5, 1, istag_notag_isempty_onempty_append01))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_TAG_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_1(seq_sign_err, \
            /* is_tag_macro */ \
            BOOST_PP_TUPLE_ELEM(5, 0, istag_notag_isempty_onempty_append01), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_YESTAG_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_NOTAG_ \
    , istag_notag_isempty_onempty_append01)

// Empty tag.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_EMPTYTAG_( \
        seq_sign_err, istag_notag_isempty_onempty_append01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            /* no_tag_seq */ \
            BOOST_PP_TUPLE_ELEM(5, 1, istag_notag_isempty_onempty_append01))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_(seq_sign_err, \
        is_tag_macro, no_tag_seq, \
        is_empty_seq_macro, on_empty_seq_macro, append_empty_01) \
    /* in (tag)( seq ) -- first look for tag */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_TAG_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_EMPTYTAG_, \
            ( is_tag_macro, no_tag_seq, \
              is_empty_seq_macro, on_empty_seq_macro, append_empty_01 ) )

// Public API.

// By default on_empty_seq_macro is prepended to generate NILSEQ (NIL)...
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT(seq_sign_err, \
        is_tag_macro, no_tag_seq, \
        is_empty_seq_macro, on_empty_seq_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_(seq_sign_err, \
            is_tag_macro, no_tag_seq, \
            is_empty_seq_macro, on_empty_seq_macro, 0 /* prepend (default) */)
        
// This macro appends on_empty_seq_macro (above macro prepends it instead).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_APPEND_OPT(seq_sign_err, \
        is_tag_macro, no_tag_seq, \
        is_empty_seq_macro, on_empty_seq_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT_(seq_sign_err, \
            is_tag_macro, no_tag_seq, \
            is_empty_seq_macro, on_empty_seq_macro, 1 /* append */)

#endif // #include guard

