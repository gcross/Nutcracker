
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_HPP_

#include "../validate/member_decl.hpp"
#include "../validate/nonmember_decl.hpp"
#include "finish.hpp"
#include "postcondition.hpp"
#include "precondition.hpp"
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
#include "../aux_/parser/is_member.hpp"
#include <boost/preprocessor/control/iif.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_MEMBER_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_MEMBER_DECL( \
    BOOST_DETAIL_PP_SIGN_PARSE_FINISH( \
    BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_PRECONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_COPYABLE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_VOLATILE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_CONST_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_PARAMS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_NAME_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_VIRTUAL_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXTERN_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INLINE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPLICIT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPORT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_REQ( \
        seq_sign_err \
    ))))))))))))))))))))))

#define BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_NONMEMBER_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_NONMEMBER_DECL( \
    BOOST_DETAIL_PP_SIGN_PARSE_FINISH( \
    BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_PRECONDITION_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_THROW_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_COPYABLE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_VOLATILE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_CONST_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_PARAMS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_NAME_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_VIRTUAL_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXTERN_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INLINE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPLICIT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPORT_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_REQ( \
        seq_sign_err \
    ))))))))))))))))))))))

#define BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_(seq_sign_err) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_AUX_PARSER_IS_MEMBER(seq_sign_err), \
        BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_MEMBER_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_NONMEMBER_ \
    )(seq_sign_err)

#define BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL(parenthesized_syntax_seq) \
    BOOST_DETAIL_PP_SIGN_PARSE_FUNCTION_DECL_( \
    BOOST_DETAIL_PP_SIGN_PARSER_START( \
        parenthesized_syntax_seq, \
        ERROR_missing_function_signature \
    ))

#endif // #include guard

