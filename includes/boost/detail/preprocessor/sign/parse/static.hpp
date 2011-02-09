
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_STATIC_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_HPP_

#include "../aux_/parse/opt.hpp"
#include "../aux_/parse/not.hpp"
#include <boost/detail/preprocessor/keyword/is_static.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_STATIC)

#define BOOST_DETAIL_PP_SIGN_PARSE_STATIC_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_STATIC, \
            ERROR_unexpected_static_specifier)

#endif // #include guard

