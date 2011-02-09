
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_THROW_HPP_
#define BOOST_DETAIL_PP_SIGN_THROW_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/detail/preprocessor/keyword/is_void.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// Sequence of specificed exceptions (if has throw and is not throw nothing).
#define BOOST_DETAIL_PP_SIGN_THROW(sign) \
    BOOST_DETAIL_PP_NILSEQ_TO_SEQ( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_THROW_ID, sign))

// Expand to 1 iff exception specs present (could throw nothing or something).
#define BOOST_DETAIL_PP_SIGN_HAS_THROW(sign) \
    BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_THROW_INDEX, sign)))

#define BOOST_DETAIL_PP_SIGN_IS_THROW_NOTHING_(sign) \
    /* got here iff seq size == 2 for `(NIL) (elem0)` */ \
    BOOST_DETAIL_PP_KEYWORD_IS_VOID(BOOST_PP_SEQ_ELEM(1, \
            BOOST_DETAIL_PP_SIGN_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_PARSED_THROW_INDEX, sign)))

// Expand to 1 iff exception specs throw nothing `throw()`.
#define BOOST_DETAIL_PP_SIGN_IS_THROW_NOTHING(sign) \
    /* throw nothing iff `(NIL) (void)` */ \
    BOOST_PP_IIF(BOOST_PP_EQUAL(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_PARSED_THROW_INDEX, sign)), 2), \
        BOOST_DETAIL_PP_SIGN_IS_THROW_NOTHING_ \
    , \
        0 /* if size != 2, no throw specs (size 1) or throw some (size > 2) */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(sign)

#endif // #include guard

