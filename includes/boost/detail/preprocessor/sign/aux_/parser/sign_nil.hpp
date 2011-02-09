
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL_HPP_

// Use this instead of `(NIL)` directly to hide implementation detail that
// signature are represented by sequences while being parsed.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL \
    (BOOST_PP_NIL) /* handle empty seq */

#endif // #include guard

