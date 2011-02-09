
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_HPP_

#include "seq_next.hpp"
#include "../parse/apply.hpp"
#include "../../parser/err.hpp"
#include <boost/detail/preprocessor/keyword/is_public.hpp>
#include <boost/detail/preprocessor/keyword/is_protected.hpp>
#include <boost/detail/preprocessor/keyword/is_private.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/logical/or.hpp>

// Check token and not on seq_sign_err.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_ACCESS_TOKEN(token) \
    BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_PUBLIC(token) \
        , BOOST_PP_OR( \
              BOOST_DETAIL_PP_KEYWORD_IS_PROTECTED(token) \
            , BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE(token) \
        ) \
    )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_CHECK_( \
        seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_ACCESS_TOKEN( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_NOT_( \
        seq_sign_err, unused) \
    0 /* return not a member */

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_(seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_CHECK_, \
            /* if empty seq, it is NOT a member */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_NOT_, ~)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER(seq_sign_err) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err), \
        /* if error, it is NOT a member */ \
        BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_NOT_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER_ \
    )(seq_sign_err, ~)

#endif // #include guard

