
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_HPP_

#include "aux_/parse/until.hpp"
#include "aux_/parse/return.hpp"
#include "aux_/parser/is_member.hpp" // For `IS_ACCESS_TOKEN()`.
#include "aux_/parser/is_ok.hpp"
#include "aux_/parser/seq.hpp"
#include "aux_/parser/seq_next.hpp"
#include <boost/detail/preprocessor/keyword/is_static.hpp>
#include <boost/detail/preprocessor/keyword/contract/is_invariant.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/comparison/greater.hpp>
#include <boost/preprocessor/seq/size.hpp>

// Predicate.

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_IS_TOKEN_(token) \
    /* if token is (static) or (invariant), quit until-loop */ \
    BOOST_PP_NOT(BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_STATIC(token) \
        , BOOST_DETAIL_PP_KEYWORD_IS_INVARIANT(token) \
    ))

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_P_(n, seq_sign_err) \
    BOOST_PP_IIF(BOOST_PP_NOT(BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_OK( \
            seq_sign_err)), \
        0 /* if error, quit until-loop */ \
    /* either no-token or static/invariant end base types */ \
    , BOOST_PP_IIF(BOOST_PP_GREATER(BOOST_PP_SEQ_SIZE( \
            /* size > 1 because (PP_NIL) always there */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ(seq_sign_err)), 1), \
        /* one or more tokens (beside PP_NIL) left, inspect next token */ \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_IS_TOKEN_( \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)) \
    , \
        0 /* if no token left (beside PP_NIL), quit until-loop */ \
    ))

// Operation.

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_ERR_( \
        n, seq_sign_err, error_message) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, error_message)

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_CONCAT_(seq_sign_err) \
    /* got here iff seq size > 2 for `(access)(base)... (NIL)` */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_SET(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK(seq_sign_err, \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK(seq_sign_err) \
                    /* seq append 2-tuple `( (access, base) )` */ \
                    (( \
                        /* seq next for `access` */ \
                        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                                seq_sign_err) \
                    , \
                        /* seq skip and next for `base` */ \
                        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
                            seq_sign_err \
                        )) \
                    )) \
            )))

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_CLASS_OK_( \
        n, seq_sign_err, unused) \
    /* got here iff sign size > 2 for `(publ|prot|priv)(base)... (NIL)` */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( /* skip `(base)` */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( /* skip `(access)` */ \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_CONCAT_(seq_sign_err) \
    ))

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_ACCESS_OK_( \
        n, seq_sign_err, unused) \
    /* check seq size > 2 for `(access)(base)... (NIL)` */ \
    BOOST_PP_IIF(BOOST_PP_GREATER(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ(seq_sign_err)), 2), \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_CLASS_OK_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_ERR_ \
    )(n, seq_sign_err, ERROR_missing_base_class_type)

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_(n, seq_sign_err) \
    /* until gets here iff seq size > 1 for `(token)... (NIL)` */ \
    /* check token is inheritance access public, protected, or private */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_ACCESS_TOKEN( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_ACCESS_OK_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_ERR_ \
    )(n, seq_sign_err, ERROR_missing_inheritance_access_level_public_protected_or_private)

// Public APIs.

#define BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_WITH_OP(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_P_, \
            BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_O_)

#endif // #include guard

