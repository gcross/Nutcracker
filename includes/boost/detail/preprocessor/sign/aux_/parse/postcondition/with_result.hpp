
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_HPP_

#include "../tagged_opt.hpp"
#include "../return.hpp"
#include "../if.hpp"
#include "../../parser/seq_next.hpp"
#include "../../../postcondition.hpp"
#include "../../../parse/finish.hpp"
#include <boost/detail/preprocessor/keyword/contract/is_postcondition.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/control/iif.hpp>

// First, parse result name token `(result)`.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_N_ONEMPTY_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_nonvoid_function_cannot_specify_postcondition_with_no_result)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_N_ISEMPTY_( \
        result_tok) \
    BOOST_PP_IS_EMPTY(result_tok)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_RESULTNAME_( \
        seq_sign_err) \
    /* use `TAGGED_APPEND` so `EMPTY` appended for opt result name */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_APPEND_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_POSTCONDITION, \
            BOOST_PP_EMPTY, /* if no (postcondition), `EMPTY` result */ \
            /* handle (postcondition)() empty result tok (on C99) */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_N_ISEMPTY_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_N_ONEMPTY_)

// Then, parse assertion sequence `( (post1) (post2) ... )`.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ONSIZEN_( \
        seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, \
            (BOOST_PP_NIL) /* to handle empty seq */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ONSIZE0_( \
        seq_sign_err, unused) \
    /* got here based on ISSIZE0 check below */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_postconditions_with_no_assertion)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ISSIZE0_( \
        seq) \
    /* handle (postcondition)(result)() (on C99) */ \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_EMPTY_( \
        seq_sign_err, unused) \
    /* handle (postcondition)(result)<NOTHING> */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_postconditions_with_missing_assertion)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_OK_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_EMPTY_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ISSIZE0_, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ONSIZE0_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_ONSIZEN_ \
    , ~)

// Handle postcondition not specified at all.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_PASS_( \
        seq_sign_err) \
    /* If got here, there was no error AND no result was specified. */ \
    /* This can only happen if `(postcondition)(result)( ... )` was */ \
    /* omitted all together. In case just return empty assertion */ \
    /* sequence `(NIL)`. */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            (BOOST_PP_NIL) )
            
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_HASRESULT_( \
        seq_sign_err, unused) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_POSTCONDITION_HAS_RESULT( \
            BOOST_DETAIL_PP_SIGN_PARSE_FINISH(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_OK_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_PASS_ \
    )(seq_sign_err)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_ASSERTIONS_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_HASRESULT_, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_A_HASRESULT_, \
        ~)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_ASSERTIONS_( \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT_RESULTNAME_( \
        seq_sign_err \
    ))

#endif // #include guard

