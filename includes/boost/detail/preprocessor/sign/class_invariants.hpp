
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef CONTRACT_AUX_PP_SIGN_CLASS_INVARIANTS_HPP_
#define CONTRACT_AUX_PP_SIGN_CLASS_INVARIANTS_HPP_

#include "sign_/elem.hpp"
#include "../nilseq.hpp"
#include <boost/preprocessor/comparison/greater.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/logical/not.hpp>

// Non-static invariant.

#define CONTRACT_AUX_PP_SIGN_CLASS_INVARIANT(sign) \
    CONTRACT_AUX_PP_NILSEQ_TO_SEQ(CONTRACT_AUX_PP_SIGN_ELEM( \
            CONTRACT_AUX_PP_SIGN_CLASS_INVARIANT_ID, sign))

#define CONTRACT_AUX_PP_SIGN_CLASS_HAS_INVARIANT(sign) \
    BOOST_PP_NOT(CONTRACT_AUX_PP_NILSEQ_IS_NIL(CONTRACT_AUX_PP_SIGN_ELEM( \
            CONTRACT_AUX_PP_SIGN_CLASS_INVARIANT_ID, sign)))

// Static invariant.

#define CONTRACT_AUX_PP_SIGN_CLASS_STATIC_INVARIANT(sign) \
    CONTRACT_AUX_PP_NILSEQ_TO_SEQ(CONTRACT_AUX_PP_SIGN_ELEM( \
            CONTRACT_AUX_PP_SIGN_CLASS_STATIC_INVARIANT_ID, sign))

#define CONTRACT_AUX_PP_SIGN_CLASS_HAS_STATIC_INVARIANT(sign) \
    BOOST_PP_NOT(CONTRACT_AUX_PP_NILSEQ_IS_NIL(CONTRACT_AUX_PP_SIGN_ELEM( \
            CONTRACT_AUX_PP_SIGN_CLASS_STATIC_INVARIANT_ID, sign)))

#endif // #include guard

