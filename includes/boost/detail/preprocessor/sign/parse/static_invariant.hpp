
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_HPP_

#include "../aux_/parser/sign_replace_back.hpp"
#include "../aux_/parser/sign_back.hpp"
#include "../aux_/parser/seq_skip.hpp"
#include "../aux_/parse/tagged_req.hpp"
#include "../aux_/parse/if.hpp"
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_IS_EMPTYSEQ_(seq) \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_REQ_PREPEND_NIL_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK(seq_sign_err, \
            (BOOST_PP_NIL) \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK(seq_sign_err)) \

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_REQ_( \
        seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_REQ_PREPEND_NIL_( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_REQ( \
                    /* skip (static) (tagged_req use APPLY to hanlde EMPTY) */ \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err), \
                    BOOST_DETAIL_PP_KEYWORD_IS_INVARIANT, \
                    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_IS_EMPTYSEQ_, \
                    ERROR_expected_static_class_invariants_after_static) \
            )

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_EMPTY_( \
        seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            (BOOST_PP_NIL) /* no static-invariant assertions */ )

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_EMPTY_, \
            BOOST_DETAIL_PP_KEYWORD_IS_STATIC, \
        BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_REQ_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_EMPTY_ \
    , ~)

#endif // #include guard

