
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_HPP_

#include "../tagged_opt.hpp"
#include "../return.hpp"
#include <boost/detail/preprocessor/keyword/contract/is_postcondition.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>
            
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_ONEMPTY_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_postcondition_with_no_assertion)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_ISEMPTY_(seq) \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_POSTCONDITION, \
            (BOOST_PP_NIL), /* if no (postcondition), no assertions */ \
            /* handle (postcondition)() empty seq (on C99) */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_ISEMPTY_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_ONEMPTY_)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT(seq_sign_err) \
    /* then parse eventual postcondition assertion sequence */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_POST_WITHOUT_RESULT_( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
                    BOOST_PP_EMPTY)) /* append PP_EMPTY for no result name */

#endif // #include guard

