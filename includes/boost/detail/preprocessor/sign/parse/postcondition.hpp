
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDTION_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDTION_HPP_

#include "finish.hpp"
#include "../aux_/parse/postcondition/with_result.hpp"
#include "../aux_/parse/postcondition/without_result.hpp"
#include "../aux_/parse/return.hpp"
#include "../aux_/parse/tagged_not.hpp"
#include "../result_type.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
            
#define BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_OPT(seq_sign_err) \
    /* if got here, no sse err -- decide if result is void or not */ \
    /* LIMITATION: This code will detect a `void` result based just on */ \
    /* matching the `void` token itself. This does not check the type (at */ \
    /* compile time) but instead the token itself (at preprocessing time). */ \
    /* the token can be `myvoid` where `typedef void myvoid` so it will */ \
    /* not be detected as `void` even if it is void type -- in this case */ \
    /* it is possible to force void result by specifying `void` as the */ \
    /* result name in the postconditions. On the other end, `void*` is not */ \
    /* will be matched as a void result type even if it is not -- in this */ \
    /* case the user can use `void_ptr` instead of `void*` where */ \
    /* `typedef void* void_ptr`. */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_IS_VOID_RESULT( \
            BOOST_DETAIL_PP_SIGN_PARSE_FINISH(seq_sign_err)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITH_RESULT \
    )(seq_sign_err)
            
#define BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_NOT_RESULT_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_PP_EMPTY) /* no result-name token */

#define BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_NOT(seq_sign_err) \
    /* make sure there is no postcondition code block */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT( \
            BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_NOT_RESULT_( \
                    seq_sign_err), \
            BOOST_DETAIL_PP_KEYWORD_IS_POSTCONDITION, \
            (BOOST_PP_NIL), /* no postcondition seq */ \
            ERROR_unexpected_postconditions)

#endif // #include guard

