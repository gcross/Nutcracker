
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef CONTRACT_AUX_PP_SIGN_VALIDATE_RETURN_HPP_
#define CONTRACT_AUX_PP_SIGN_VALIDATE_RETURN_HPP_

#include "../aux_/parse/return.hpp"

#define CONTRACT_AUX_PP_SIGN_VALIDATE_RETURN_OK(seq_sign_err) \
    seq_sign_err /* ok (no error, simply return seq_sign_err) */

#define CONTRACT_AUX_PP_SIGN_VALIDATE_RETURN_ERR(seq_sign_err, error_message) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, error_message)

#endif // #include guard

