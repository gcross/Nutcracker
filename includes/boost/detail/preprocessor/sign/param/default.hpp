
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAM_DEFAULT_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAM_DEFAULT_HPP_

#include "../aux_/param_indexes.hpp"
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAM_DEFAULT(param) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_AUX_PARAM_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_AUX_PARAM_DEFAULT_INDEX, param) \
            (/* expand empty */)

#define BOOST_DETAIL_PP_SIGN_PARAM_HAS_DEFAULT(param) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY( \
            BOOST_DETAIL_PP_SIGN_PARAM_DEFAULT(param)))

#endif // #include guard

