
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_CONSTRUCTOR_DECL_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_CONSTRUCTOR_DECL_HPP_

#include "../validate/constructor_decl.hpp"
#include "finish.hpp"
#include "postcondition.hpp"
#include "ition.hpp"
#include "initializers.hpp"
#include "throw.hpp"
#include "copyable.hpp"
#include "volatile.hpp"
#include "const.hpp"
#include "function_params.hpp"
#include "name.hpp"
#include "result_type.hpp"
#include "virtual.hpp"
#include "static.hpp"
#include "extern.hpp"
#include "inline.hpp"
#include "explicit.hpp"
#include "requires.hpp"
#include "template.hpp"
#include "export.hpp"
#include "access.hpp"
#include "body.hpp"
#include "start.hpp"

#define BOOST_DETAIL_PP_SIGN_PARSE_CONSTRUCTOR_DECL( \
        parenthesized_syntax_seq) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_CONSTRUCTOR_DECL( \
    BOOST_DETAIL_PP_SIGN_PARSE_FINISH( \
    BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_PRECONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_COPYABLE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_VOLATILE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_CONST_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_PARAMS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_NAME_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_VIRTUAL_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXTERN_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INLINE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPLICIT_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPORT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY( \
        parenthesized_syntax_seq, \
        ERROR_missing_constructor_signature \
    )))))))))))))))))))))))

#endif // #include guard

