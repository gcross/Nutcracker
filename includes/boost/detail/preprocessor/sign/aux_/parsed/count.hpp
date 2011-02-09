
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSED_COUNT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_COUNT_HPP_

#include <boost/preprocessor/array/size.hpp>

// Return the size of parsed sign plus 1 (because of the extra `NIL` token 
// internally used to handle empty sequences, the "plus 1" is the reason why
// this is called "count" instead of "size").
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_COUNT(seq_sign_err) \
    BOOST_PP_ARRAY_SIZE(BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err))

#endif // #include guard

