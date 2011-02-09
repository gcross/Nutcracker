
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_HPP_

#include "../parser/seq.hpp"
#include "../aux_/parse/until.hpp"
#include "../aux_/parse/return.hpp"
#include <boost/detail/preprocessor/keyword/contract/is_precondition.hpp>
#include <boost/detail/preprocessor/keyword/contract/is_postcondition.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/comparison/greater.hpp>
#include <boost/preprocessor/seq/size.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_IS_TOKEN_(token) \
    /* if token is (precondition) or (postcondition), quit until-loop */ \
    BOOST_PP_NOT(BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_PRECONDITION(token) \
        , BOOST_DETAIL_PP_KEYWORD_IS_POSTCONDITION(token)) \
    )

#define BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_P_(n, seq_sign_err) \
    /* body parsed first so either no-token or pre/post end initializers */ \
    BOOST_PP_IIF(BOOST_PP_GREATER(BOOST_PP_SEQ_SIZE( \
            /* size > 1 because (PP_NIL) always there */ \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), 1), \
        /* one or more tokens (beside PP_NIL) left, inspect next token */ \
        BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_IS_TOKEN_( \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)) \
    , \
        0 /* if no token left (beside PP_NIL), quit until-loop */ \
    ) /* do not delay macro expansion here (for MSVC) */

// Public API.

#define BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_P_)

#define BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            (BOOST_PP_NIL) /* empty initializers seq `(NIL)`*/ )

#endif // #include guard

