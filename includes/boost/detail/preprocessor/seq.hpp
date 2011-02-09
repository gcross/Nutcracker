
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SEQ_HPP_
#define BOOST_DETAIL_PP_SEQ_HPP_

#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/first_n.hpp>
#include <boost/preprocessor/seq/to_tuple.hpp>
#include <boost/preprocessor/seq/rest_n.hpp>
#include <boost/preprocessor/arithmetic/sub.hpp>

// Sequence manipulation in addition to Boost.Preprocessor ones.

// Return element in the back.
#define BOOST_DETAIL_PP_SEQ_BACK(seq) \
    BOOST_PP_SEQ_ELEM(BOOST_PP_SUB(BOOST_PP_SEQ_SIZE(seq), 1), seq)

// Return all elems but last: `(elem0) (elem1) ... (elem<N-2>)`.
#define BOOST_DETAIL_PP_SEQ_LEAD(seq) \
    BOOST_PP_SEQ_FIRST_N(BOOST_PP_DEC(BOOST_PP_SEQ_SIZE(seq)), seq)

// PARENTHESIZE //

#define BOOST_DETAIL_PP_SEQ_PARANTHESIZE_ELEM_(s, paren_seq, elem) \
    paren_seq /* seq concat */ ((elem)) /* add extra parenthesis */

// Used by some (Boost) code to handle commas within macro parameters.
#define BOOST_DETAIL_PP_SEQ_PARENTHESIZE_S(s, seq) \
    BOOST_PP_SEQ_TAIL( /* remove extra ((PP_EMPTY)) */ \
            BOOST_PP_SEQ_FOLD_LEFT_ ## s( \
                    BOOST_DETAIL_PP_SEQ_PARANTHESIZE_ELEM_, \
                    ((BOOST_PP_EMPTY)), /* handle empty macro arg */ \
                    seq))

#endif // #include guard

