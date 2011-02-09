
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_CLASS_BASE_HPP_
#define BOOST_DETAIL_PP_SIGN_CLASS_BASE_HPP_

#include "aux_/class_base_indexes.hpp"
#include <boost/preprocessor/tuple/elem.hpp>

// Return base type.
#define BOOST_DETAIL_PP_SIGN_CLASS_BASE_TYPE(base) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_AUX_CLASS_BASE_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_AUX_CLASS_BASE_TYPE_INDEX, base)

// Return base inheritance access level -- public, protected, or private.
#define BOOST_DETAIL_PP_SIGN_CLASS_BASE_ACCESS(base) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_AUX_CLASS_BASE_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_AUX_CLASS_BASE_ACCESS_INDEX, base)

#endif // #include guard

