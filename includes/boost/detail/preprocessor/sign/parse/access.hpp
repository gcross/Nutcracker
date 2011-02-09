
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_HPP_

#include "../aux_/parser/is_member.hpp" // For IS_ACCESS_TOKEN().
#include "../aux_/parse/req.hpp"
#include "../aux_/parse/not.hpp"

#define BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_REQ(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_ACCESS_TOKEN, \
            ERROR_missing_public_protected_or_private_specifier)

#define BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_ACCESS_TOKEN, \
            ERROR_unexpected_public_or_protected_or_private_specifier)

#endif // #include guard

