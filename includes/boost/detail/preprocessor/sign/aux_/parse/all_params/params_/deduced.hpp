
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_HPP_

#include "all.hpp"
#include "../../return.hpp"
#include "../../apply_1.hpp" // To avoid reentrancy issues.
#include "../../../parser/merge.hpp"
#include "../../../parser/sign_nil.hpp"
#include "../../../parser/seq_next.hpp"
#include "../../../parser/seq_skip.hpp"
#include "../../../../parse/start.hpp"
#include <boost/detail/preprocessor/keyword/parameter/is_deduced.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/seq/size.hpp>

// Deduced named params.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_NONE_OK_D_( \
        d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_UNEXPECTED_ERR_D_( \
        d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_unexpected_deduced_named_parameters)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_MISSING_ERR_D_( \
        seq_sign_err, d) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_missing_deduced_named_parameters)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_EMPTY_ERR_D_( \
        seq_sign_err, d) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_specified_empty_deduced_named_parameters)

// C99 only.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_MISSING_EMPTY_ERR_D_( \
        d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_empty_deduced_named_parameters)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_ALL_D_( \
        d, seq_sign_err) \
    /* take only 2nd elem because 1st is NIL */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE2(seq_sign_err, \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_D(d, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_O_, \
        BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY( \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err), \
                ERROR_missing_deduced_parameters) \
    ))
            
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_IF_EMPTY_D_( \
        seq_sign_err, d) \
    BOOST_PP_IF(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_ALL_D_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_EMPTY_ERR_D_ \
    )(d, seq_sign_err) 

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_YES_D_( \
        d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1( \
            /* skip (deduced) */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err), \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_IF_EMPTY_D_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_MISSING_ERR_D_, \
            d)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_NO(seq_sign_err) \
    /* next could be PP_NIL, or just not (deduced) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_DEDUCED( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_UNEXPECTED_ERR_D_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_NONE_OK_D_ \
    )(0, seq_sign_err)
        

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_D(d, seq_sign_err) \
    /* next could be PP_NIL, or just not (deduced) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_DEDUCED( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_YES_D_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_NONE_OK_D_ \
    )(d, seq_sign_err)

#endif // #include guard

