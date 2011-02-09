
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_HPP_

#include "return.hpp"
#include "if.hpp"
#include "../parser/seq_next.hpp"
#include <boost/preprocessor/facilities/empty.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_NO_(seq_sign_err, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_PP_EMPTY)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_YES_(seq_sign_err, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) \
            BOOST_PP_EMPTY)
    
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT(seq_sign_err, check_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_NO_, \
            check_macro, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_YES_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPT_NO_ \
    , ~)

#endif // #include guard

