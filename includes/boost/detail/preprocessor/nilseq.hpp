
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_NILSEQ_HPP_
#define BOOST_DETAIL_PP_NILSEQ_HPP_

#include <boost/preprocessor/seq.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/while.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/logical/not.hpp>

// Utilities to handle sequences starting with `(NIL)` (to support empty seq):
//     nilseq := (BOOST_PP_NIL) [(elem0) (elem1) ...]

// NOTE: `BOOST_PP_LIST` serves a similar purpose but it could not be used in
// order to reuse signature parsing code. For example, parsed parameters are
// represented as NILSEQ so parsers code used to parse sign sequence (wich
// operate on SEQ) can be reused. Similarly, for initializers, concepts, etc.

#define BOOST_DETAIL_PP_NILSEQ_SIZE(nilseq) \
    BOOST_PP_DEC(BOOST_PP_SEQ_SIZE(nilseq)) /* DEC for leading `(NIL)` */

#define BOOST_DETAIL_PP_NILSEQ_IS_NIL(nilseq) \
    BOOST_PP_EQUAL(BOOST_DETAIL_PP_NILSEQ_SIZE(nilseq), 0)

// Assume nilseq is not NIL and removes leading `(NIL)`.
#define BOOST_DETAIL_PP_NILSEQ_TO_SEQ(nilseq) \
    BOOST_PP_SEQ_TAIL(nilseq)

#define BOOST_DETAIL_PP_NILSEQ_ELEM(n, nilseq) \
    BOOST_PP_SEQ_ELEM(BOOST_PP_INC(n), nilseq) /* INC for leading `(NIL)` */

// FOR_EACH_I //

#define BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I_(op, data, nilseq) \
    /* got here iif nilseq size not nil, TAIL removes leading `(NIL)` */ \
    BOOST_PP_SEQ_FOR_EACH_I(op, data, BOOST_DETAIL_PP_NILSEQ_TO_SEQ(nilseq))

#define BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I(op, data, nilseq) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_NILSEQ_IS_NIL(nilseq), \
        BOOST_PP_TUPLE_EAT(3) \
    , \
        BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I_ \
    )(op, data, nilseq)

// FIND //

// Increments `index`.
#define BOOST_DETAIL_PP_NILSEQ_FIND_O_(d, index_nilseq_check) \
    ( \
        BOOST_PP_INC(BOOST_PP_TUPLE_ELEM(3, 0, index_nilseq_check)) \
    , \
        BOOST_PP_TUPLE_ELEM(3, 1, index_nilseq_check) \
    , \
        BOOST_PP_TUPLE_ELEM(3, 2, index_nilseq_check) \
    )

// Assume `index` within `nilseq` range and expand to 
// `check_macro(PP_SEQ_ELEM(index, nilseq))`.
#define BOOST_DETAIL_PP_NILSEQ_FIND_P_CHECK_NOT_(d, index_nilseq_check) \
    BOOST_PP_NOT( /* quit while-loop if `check_macro(elem)` is true */ \
    BOOST_PP_TUPLE_ELEM(3, 2, index_nilseq_check)( /* `check_macro(elem)` */ \
        BOOST_PP_SEQ_ELEM(BOOST_PP_TUPLE_ELEM(3, 0, index_nilseq_check), \
                BOOST_PP_TUPLE_ELEM(3, 1, index_nilseq_check)) \
    )) \

#define BOOST_DETAIL_PP_NILSEQ_FIND_P_(d, index_nilseq_check) \
    BOOST_PP_EXPAND( /* for proper macro expansion on MSVC */ \
    BOOST_PP_IIF(BOOST_PP_LESS(BOOST_PP_TUPLE_ELEM(3, 0, index_nilseq_check), \
            BOOST_PP_SEQ_SIZE(BOOST_PP_TUPLE_ELEM(3, 1, index_nilseq_check))), \
        BOOST_DETAIL_PP_NILSEQ_FIND_P_CHECK_NOT_ \
    , \
        0 /* quit while-loop because out of range */ \
        BOOST_PP_TUPLE_EAT(2) \
    )(d, index_nilseq_check) )

// Expand to `(index, nilseq, check)` where index is the one of the 1st elem in
// nil-seq for which `check(elem)` is true -- index is 0 if no match.
#define BOOST_DETAIL_PP_NILSEQ_FIND_INDEX_(nilseq, check_macro) \
    BOOST_PP_TUPLE_ELEM(3, 0, \
            BOOST_PP_WHILE(BOOST_DETAIL_PP_NILSEQ_FIND_P_, \
                    BOOST_DETAIL_PP_NILSEQ_FIND_O_, \
                    (1 /* index */, nilseq, check_macro) ))

#define BOOST_DETAIL_PP_NILSEQ_FIND_(nilseq, found_index) \
    BOOST_PP_EXPAND( /* for proper macro expansion on MSVC */ \
    BOOST_PP_IIF(BOOST_PP_LESS(found_index, BOOST_PP_SEQ_SIZE(nilseq)), \
        found_index /* found within range (or not found 0) */ \
    , \
        0 /* not found */ \
    ))

// Expand to index of 1st elem in nilseq for which `check_macro(elem)` is true,
// or to 0 if `check_macro(elem)` is false for all nilseq elems.
#define BOOST_DETAIL_PP_NILSEQ_FIND(nilseq, check_macro) \
    BOOST_DETAIL_PP_NILSEQ_FIND_(nilseq, \
            BOOST_DETAIL_PP_NILSEQ_FIND_INDEX_(nilseq, check_macro))

#endif // #include guard

