
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ANY_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ANY_BIND_HPP_

#include "has_const_bind.hpp"
#include "has_bind.hpp"
#include <boost/preprocessor/logical/or.hpp>

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ANY_BIND(params) \
    BOOST_PP_OR( /* can't use `BITOR` for reentrancy on MSVC... */ \
          BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_CONST_BIND(params) \
        , BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND(params) \
    )

#endif // #include guard

