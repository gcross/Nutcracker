
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_CLASS_BASES_HPP_
#define BOOST_DETAIL_PP_SIGN_CLASS_BASES_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/logical/not.hpp>

// Assume `HAS_BASES()` and return bases as a sequence `(base1)(base2)...`.
// Use `CLASS_BASE_XXX(base)` to inspect each base element.
#define BOOST_DETAIL_PP_SIGN_CLASS_BASES(sign) \
    BOOST_DETAIL_PP_NILSEQ_TO_SEQ(BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_CLASS_BASES_INDEX, sign))

// Need to use this instead of `PP_SEQ_SIZE(SIGN_CLASS_BASES(sign))` because
// this can be 0 while `PP_SEQ_SIZE()` cannot be 0 (it can be 0 only on C99).
#define BOOST_DETAIL_PP_SIGN_CLASS_BASES_SIZE(sign) \
    BOOST_DETAIL_PP_NILSEQ_SIZE(BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_BASES_INDEX, sign))

#define BOOST_DETAIL_PP_SIGN_CLASS_HAS_BASES(sign) \
    BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_BASES_INDEX, sign)))

#endif // #include guard

