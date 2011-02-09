
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include "aux_/params_indexes.hpp"

// Function parameters (positional or named).

#define BOOST_DETAIL_PP_SIGN_PARAMS(sign) \
    /* params represented as 3-array `(3, (tag, induced, deduced))` */ \
    (BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDEX_MAX, \
        ( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_TAG_INDEX, \
                    sign) \
        , \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_INDUCED_INDEX, \
                    sign) \
        , \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_DEDUCED_INDEX, \
                    sign) \
        ) \
    )

#endif // #include guard

