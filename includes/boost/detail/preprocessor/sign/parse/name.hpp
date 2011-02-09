
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_NAME_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_NAME_HPP_

#include "../aux_/parse/req.hpp"
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_NAME_ANY_TOKEN_(token) \
    /* any non-empty token can be a name */ \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY(token))

// Parse the operation name (for functions/operators, constructors, etc).
#define BOOST_DETAIL_PP_SIGN_PARSE_NAME_REQ(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_NOEMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_NAME_ANY_TOKEN_, \
            ERROR_missing_name_for_function_or_operator_or_constructor_or_destructor)

#endif // #include guard

