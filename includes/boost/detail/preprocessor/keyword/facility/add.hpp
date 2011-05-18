
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_HPP_
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_HPP_

#include <boost/preprocessor/control/expr_iif.hpp>
#include <boost/preprocessor/logical/not.hpp>

// `is_front_macro(tokens)` is 1 iff `tokens` start with `keyword` to add.
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT( \
        tokens, is_front_macro, keyword) \
    BOOST_PP_EXPR_IIF(BOOST_PP_NOT(is_front_macro(tokens)), keyword) tokens

// `is_back_macro(tokens)` is 1 iff `tokens` end with `keyword` to add.
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
        is_back_macro, keyword) \
    tokens BOOST_PP_EXPR_IIF(BOOST_PP_NOT(is_back_macro(tokens)), keyword)

#endif // #include guard

