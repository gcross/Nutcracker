
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM_HPP_

#include <boost/preprocessor/array/elem.hpp>

// Index as #defined in "indexes.hpp".
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM(index, sign) \
    /* parsed signature internally stored as array with indexed by ID */ \
    BOOST_PP_ARRAY_ELEM(index, sign)

#endif // #include guard

