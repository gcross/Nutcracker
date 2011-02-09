
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_CHECK_HPP_
#define BOOST_DETAIL_PP_KEYWORD_CHECK_HPP_

// Boost.Preprocessor author P. Mensodines confirmed on an Boost email thread
// (subject ``check if a token is a keyword (was "BOOST_PP_IS_UNARY()")'')
// that it is OK to used `PP_IS_UNARY()` to check if tokens match predefined
// "keyword" as it is done by the macros below (even if `PP_IS_UNARY()` is
// technically only part of Boost.Preprocessor private API).
#include <boost/preprocessor/detail/is_unary.hpp>
#include <boost/preprocessor/cat.hpp>

// Expand to true iff keyword_prefix ## token is #defined, e.g.:
//  #define KWD_VOID_void (1) // Can be 1 or any non-empty token.
//  #define IS_VOID(tok) BOOST_DETAIL_PP_KEYWORD_CHECK(KWD_VOID_, tok)
//  IS_VOID(void) // Expand to 1.
//  IS_VOID(abcd) // Expand to 0.
#define BOOST_DETAIL_PP_KEYWORD_CHECK_FRONT(tokens, keyword_prefix) \
    BOOST_PP_IS_UNARY(BOOST_PP_CAT(keyword_prefix, tokens))

// This allow for special symbols in from of the check tokens because it checks
// from the "trail" of tokens. However, it only works iff there is only one
// single valid token in tokens. For example, `&this` can be checked to be
// `this` instead `& const this` will not check to be `this`.
#define BOOST_DETAIL_PP_KEYWORD_CHECK_BACK(tokens, keyword_postfix) \
    BOOST_PP_IS_UNARY(BOOST_PP_CAT(tokens, keyword_postfix))

#endif // #include guard

