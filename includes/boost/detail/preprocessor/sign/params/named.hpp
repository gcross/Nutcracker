
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_NAMED_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_NAMED_HPP_

#include "tag_namespace.hpp"

#define BOOST_DETAIL_PP_SIGN_PARAMS_ARE_NAMED(params) \
    /* params are named iff the have non-empty tag namespace */ \
    BOOST_DETAIL_PP_SIGN_PARAMS_HAVE_TAG_NAMESPACE(params)

#endif // #include guard

