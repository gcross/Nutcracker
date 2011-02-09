
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_HPP_

#include "params_/named.hpp"
#include "params_/positional.hpp"
#include "../apply.hpp"
#include "../return.hpp"
#include "../../parser/sign_nil.hpp"
#include "../../parser/seq_next.hpp"
#include "../../parser/seq_skip.hpp"
#include "../../../parser/seq.hpp"
#include "../../../parse/start.hpp"
#include <boost/detail/preprocessor/keyword/is_void.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/logical/and.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/seq/size.hpp>

// Handle named or positional.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_OR_POSITIONAL_D_( \
        d, seq_sign_err) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_IS_NAMED_TOKEN( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_D \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_POSITIONAL_D \
    )(d, seq_sign_err)

// Handle empty `()` (C99 only).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_EMPTY_(seq_sign_err, d) \
    /* no param to parse, append `EMPTY (NIL) (NIL) ` */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL( \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL( \
                seq_sign_err, \
                BOOST_PP_EMPTY /* no tag namespace */ \
            ), \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL /* no induced params */\
        ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL /* no deduced params */ \
    )

// Handle (void).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_VOID_D_(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_EMPTY_( \
            /* skip `(void)` */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err), d)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_HANDLE_VOID_(seq_sign_err, d) \
    /* it is void iff 2-elem seq (token)(NIL) and token == void */\
    BOOST_PP_IIF(BOOST_PP_AND( \
            BOOST_PP_EQUAL(BOOST_PP_SEQ_SIZE( \
                    BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), 2), \
            BOOST_DETAIL_PP_KEYWORD_IS_VOID( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                    seq_sign_err))), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_VOID_D_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_OR_POSITIONAL_D_ \
    )(d, seq_sign_err)

// Parse params.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_D_(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_HANDLE_VOID_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_EMPTY_, d)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_D(d, seq) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_D_(d, \
            BOOST_DETAIL_PP_SIGN_PARSE_START(seq))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS(seq) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_D(1, seq)

#endif // #include guard

