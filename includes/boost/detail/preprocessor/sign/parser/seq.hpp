
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSER_SEQ_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSER_SEQ_HPP_

#include "../aux_/parser/indexes.hpp"
#include <boost/preprocessor/tuple/elem.hpp>

// This API could be made private (i.e., moved into "aux_/") but it is provided
// here for maximum flexibility to the users which might want to show the
// parenthsized syntax sequence left to be parsed for example in case of an
// error.
#define BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
    BOOST_PP_TUPLE_ELEM(BOOST_DETAIL_PP_SIGN_AUX_PARSER_INDEX_MAX, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_INDEX, seq_sign_err)

#endif // #include guard

