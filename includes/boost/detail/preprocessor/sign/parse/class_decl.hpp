
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_CLASS_DECL_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_CLASS_DECL_HPP_

#include "../validate/class_decl.hpp"
#include "finish.hpp"
#include "invariant.hpp"
#include "static_invariant.hpp"
#include "base_types.hpp"
#include "class_type.hpp"
#include "start.hpp"

#define BOOST_DETAIL_PP_SIGN_PARSE_CLASS_DECL(parenthesized_syntax_seq) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_CLASS_DECL( \
    BOOST_DETAIL_PP_SIGN_PARSE_FINISH( \
    BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_INVARIANT_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_BASE_TYPES_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_CLASS_TYPE_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY( \
        parenthesized_syntax_seq, \
        ERROR_missing_class_signature \
    )))))))

#endif // #include guard

