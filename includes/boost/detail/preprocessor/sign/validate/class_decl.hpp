
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef CONTRACT_DECL_PP_SIGN_VALIDATE_CLASS_DECL_HPP_
#define CONTRACT_DECL_PP_SIGN_VALIDATE_CLASS_DECL_HPP_

#include "basic.hpp"
#include "return.hpp"
#include "../aux_/parsed/indexes.hpp"

#define BOOST_DETAIL_PP_SIGN_VALIDATE_CLASS_DECL(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_CLASS_DECL(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_VALIDATE_RETURN_OK)

#endif // #include guard

