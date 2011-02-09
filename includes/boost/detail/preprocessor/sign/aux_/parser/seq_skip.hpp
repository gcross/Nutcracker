
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP_HPP_

#include "../../parser/seq.hpp"
#include "../../parser/sign.hpp"
#include "../../parser/err.hpp"
#include <boost/preprocessor/seq/seq.hpp> // For PP_SEQ_TAIL.

// Assume 2 or more elem in seq.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP(seq_sign_err) \
    ( \
        /* skip head in seq left to be parsed */ \
        BOOST_PP_SEQ_TAIL(BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    )

#endif // #include guard

