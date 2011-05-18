
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_HPP_
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_HPP_

// Boost.Preprocessor author P. Mensodines confirmed on an Boost email thread
// (subject ``check if a token is a keyword (was "BOOST_PP_IS_UNARY()")'')
// that it is OK to used `PP_IS_UNARY()` to check if tokens match predefined
// "keyword" as it is done by the macros below (even if `PP_IS_UNARY()` is
// technically only part of Boost.Preprocessor private API).
#include <boost/preprocessor/detail/is_unary.hpp>
#include <boost/preprocessor/cat.hpp>

// `chekcing_prefix ## tokens` expand to unary (e.g., `(1)`) iff `tokens` start
// with keyword to check.
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, checking_prefix) \
    BOOST_PP_IS_UNARY(BOOST_PP_CAT(checking_prefix, tokens))

// `token ## chekcing_postfix` expand to unary (e.g., `(1)`) iff `token` is the
// keyword to check. This check only works if `token` is a single token, it
// will always expand to 0 if token is multiple tokens (e.g., `const *this`).
// This check will expand to 0 with no error if `token` starts with a
// non-alphanumeric symbol (e.g., `*this`).
#define BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, checking_postfix) \
    BOOST_PP_IS_UNARY(BOOST_PP_CAT(token, checking_postfix))

#endif // #include guard

