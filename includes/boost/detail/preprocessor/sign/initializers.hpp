
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_INITIALIZERS_HPP_
#define BOOST_DETAIL_PP_SIGN_INITIALIZERS_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/logical/not.hpp>

#define BOOST_DETAIL_PP_SIGN_INITIALIZERS(sign) \
    BOOST_DETAIL_PP_NILSEQ_TO_SEQ(BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            CONTRACT_DETAOL_PP_SIGN_AUX_PARSED_INITIALIZERS_INDEX, sign))

#define BOOST_DETAIL_PP_SIGN_HAS_INITIALIZERS(sign) \
    BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL( \
            BOOST_DETAIL_PP_SIGN_ELEM( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSED_INITIALIZERS_INDEX, \
                    sign)))

#endif // #include guard

