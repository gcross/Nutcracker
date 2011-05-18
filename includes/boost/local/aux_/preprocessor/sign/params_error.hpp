
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR_HPP_

#include "parsed_params_/index.hpp"
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>

// Expand to `EMPTY` if no error, to `ERROR_message_text EMPTY` otherwise.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR(params) \
    BOOST_PP_TUPLE_ELEM(BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_INDEX_MAX, \
            BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_INDEX_ERROR, \
            params)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR_MSG(params) \
    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR(params)(/* expand empty */)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ERROR(params) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR_MSG( \
            params)))

#endif // #include guard

