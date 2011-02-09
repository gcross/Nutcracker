
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_HPP_

#include <boost/detail/preprocessor/keyword/check.hpp>

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_bind (1)

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND(tokens) \
    BOOST_DETAIL_PP_KEYWORD_CHECK_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_BIND_)

#endif // #include guard

