
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_RANGE_HPP_
#define BOOST_DETAIL_PP_RANGE_HPP_

#include <boost/preprocessor/logical/and.hpp>
#include <boost/preprocessor/comparison/greater_equal.hpp>
#include <boost/preprocessor/comparison/less_equal.hpp>

// Expand to 1 iff `min <= value <= max` (equality included).
#define BOOST_DETAIL_PP_RANGE_IN(min, max, value) \
    BOOST_PP_AND( \
          BOOST_PP_GREATER_EQUAL(value, min) \
        , BOOST_PP_LESS_EQUAL(value, max) \
    )

#endif // #include guard

