
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK_HPP_

#include "../../parser/sign.hpp"
#include <boost/detail/preprocessor/seq.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/array/size.hpp>
#include <boost/preprocessor/array/elem.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK(seq_sign_err) \
    BOOST_DETAIL_PP_SEQ_BACK(BOOST_DETAIL_PP_SIGN_PARSER_SIGN( \
            seq_sign_err))

#endif // #include guard

