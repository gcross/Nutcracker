
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_VALIDATE_THIS_COUNT_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_VALIDATE_THIS_COUNT_HPP_

#include "../set_error.hpp"
#include "../../params_error.hpp"
#include "../../params_const_bind.hpp"
#include "../../params_bind.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/logical/bitand.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_VALIDATE_THIS_COUNT_(params) \
    BOOST_PP_IIF(BOOST_PP_BITAND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_CONST_BIND_THIS(params), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_BIND_THIS(params)), \
        BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_SET_ERROR \
    , \
        params BOOST_PP_TUPLE_EAT(2) \
    )(params, /* trailing `EMPTY` because error might not be present */ \
            ERROR_cannot_bind_object_this_multiple_times BOOST_PP_EMPTY)

// PUBLIC //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_VALIDATE_THIS_COUNT(params) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ERROR(params), \
        params BOOST_PP_TUPLE_EAT(1) /* fwd existing error */ \
    , \
        BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_VALIDATE_THIS_COUNT_ \
    )(params)

#endif // #include guard

