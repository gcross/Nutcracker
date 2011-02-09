
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_APPEND_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_APPEND_HPP_

#include "../../parser/seq.hpp"
#include "../../parser/sign.hpp"
#include "../../parser/err.hpp"
#include <boost/preprocessor/array/push_back.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_APPEND( \
        seq_sign_err, sign_elem_to_append) \
    ( \
        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err) \
        /* append using seq concatenation */ \
        (sign_elem_to_append) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    )

#endif // #include guard

