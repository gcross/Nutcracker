
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_THROW_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_THROW_HPP_

#include "../aux_/parse/tagged_opt.hpp"
#include "../aux_/parse/return.hpp"
#include <boost/detail/preprocessor/keyword/is_throw.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>
            
#define BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT_EMPTYSEQ_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, (void) )

#define BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT_ISEMPTY_(seq) \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW, \
            (BOOST_PP_NIL), /* if no (throw), no execption specs */ \
            /* handle (throw)() as (throw)( (void) ) (on C99) */ \
            BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT_ISEMPTY_, \
            BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT_EMPTYSEQ_)

#define BOOST_DETAIL_PP_SIGN_PARSE_THROW_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_WITH_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW, \
            (BOOST_PP_NIL), /* no throw and no exception specs */ \
            ERROR_unexpected_throw_specifier)

#endif // #include guard

