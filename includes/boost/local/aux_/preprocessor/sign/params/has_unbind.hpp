
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_UNBIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_UNBIND_HPP_

#include "unbind.hpp"
#include <boost/detail/preprocessor/sign/params/size.hpp>
#include <boost/preprocessor/logical/bool.hpp>

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_UNBIND(params) \
    BOOST_PP_BOOL(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(params)))

#endif // #include guard

