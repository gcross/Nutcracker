
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_HPP_

#include "../aux_/parse/tagged_opt.hpp"
#include "../aux_/parse/not.hpp"
#include "../aux_/parse/return.hpp"
#include <boost/detail/preprocessor/keyword/concept/is_requires.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>
            
#define BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT_ONEMPTY_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_requires_with_no_concept)

#define BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT_ISEMPTY_(seq) \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_REQUIRES, \
            (BOOST_PP_NIL), /* if no (requires), no concepts */ \
            /* handle (requires)() empty seq (on C99) */ \
            BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT_ISEMPTY_, \
            BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT_ONEMPTY_)

#define BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_WITH_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_REQUIRES, \
            (BOOST_PP_NIL), /* no requires and no concepts */ \
            ERROR_unexpected_requires_specifier)

#endif // #include guard

