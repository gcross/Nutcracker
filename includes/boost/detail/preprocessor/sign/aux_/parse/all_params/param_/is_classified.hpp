
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_CLASSIFIED_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_CLASSIFIED_HPP_

#include <boost/detail/preprocessor/keyword/parameter/is_in.hpp>
#include <boost/detail/preprocessor/keyword/parameter/is_out.hpp>
#include <boost/detail/preprocessor/keyword/parameter/is_inout.hpp>
#include <boost/detail/preprocessor/keyword/is_register.hpp>
#include <boost/detail/preprocessor/keyword/is_auto.hpp>
#include <boost/preprocessor/logical/or.hpp>

// I/O classified by in/out/inout (named params only).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_IO_TOKEN(token) \
    BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_IN(token) \
        , BOOST_PP_OR( \
              BOOST_DETAIL_PP_KEYWORD_IS_OUT(token) \
            , BOOST_DETAIL_PP_KEYWORD_IS_INOUT(token) \
        ) \
    )

// Storage classified by auto/register (positional params only).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_STORAGE_TOKEN(token) \
    BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_REGISTER(token) \
        , BOOST_DETAIL_PP_KEYWORD_IS_AUTO(token) \
    )

#endif // #include guard

