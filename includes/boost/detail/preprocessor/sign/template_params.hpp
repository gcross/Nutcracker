
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_TEMPLATE_PARAMS_HPP_
#define BOOST_DETAIL_PP_SIGN_TEMPLATE_PARAMS_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include "aux_/parsed/params_indexes.hpp"
#include <boost/preprocessor/facilities/empty.hpp>

// Function template parameters.

#define BOOST_DETAIL_PP_SIGN_TEMPLATE_PARAMS(sign) \
    /* params represented as 3-array `(3, (tag, induced, deduced))` */ \
    (BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_INDEX_MAX, \
        ( \
            BOOST_PP_EMPTY /* no tag for tparams */ \
        , \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( /* tparams as induced */ \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSED_TEMPLATE_PARAMS_INDEX, \
                    sign) \
        , \
            (BOOST_PP_NIL) /* no deduced for tparams */ \
        ) \
    )

#endif // #include guard

