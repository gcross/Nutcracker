
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK_HPP_

#include "../../parser/seq.hpp"
#include "../../parser/sign.hpp"
#include "../../parser/err.hpp"
#include <boost/detail/preprocessor/seq.hpp>
#include <boost/preprocessor/array/replace.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/array/size.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK( \
        seq_sign_err, sign_new_back_elem) \
    ( \
        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SEQ_LEAD( \
                BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN(seq_sign_err)) \
        (sign_new_back_elem) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    )

#endif // #include guard

