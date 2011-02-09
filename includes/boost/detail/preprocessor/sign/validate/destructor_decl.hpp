
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_VALIDATE_DESTRUCTOR_DECL_HPP_
#define BOOST_DETAIL_PP_SIGN_VALIDATE_DESTRUCTOR_DECL_HPP_

#include "basic.hpp"
#include "return.hpp"
#include "../function_params.hpp"
#include "../params/size.hpp"
#include "../parser/sign.hpp"
#include <boost/preprocessor/control/if.hpp>

#define BOOST_DETAIL_PP_SIGN_VALIDATE_DESTRUCTOR_DECL_(seq_sign_err) \
    /* got here iff `seq_sign_err` is OK (no error) */ \
    /* destructor must have no func params */ \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE( \
            BOOST_DETAIL_PP_SIGN_FUNCTION_PARAMS( \
                    BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err))), \
        BOOST_DETAIL_PP_SIGN_VALIDATE_RETURN_ERR(seq_sign_err, \
                ERROR_destructor_cannot_have_parameters) \
    , /* else, no error */ \
        BOOST_DETAIL_PP_SIGN_VALIDATE_RETURN_OK(seq_sign_err) \
    )

#define BOOST_DETAIL_PP_SIGN_VALIDATE_DESTRUCTOR_DECL(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_DECL(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_VALIDATE_DESTRUCTOR_DECL_)

#endif // #include guard

