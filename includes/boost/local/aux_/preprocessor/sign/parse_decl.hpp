
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSE_DECL_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_DECL_HPP_

#include "validate_.hpp"
#include <boost/detail/preprocessor/sign/parse/finish.hpp>
#include <boost/detail/preprocessor/sign/parse/postcondition.hpp>
#include <boost/detail/preprocessor/sign/parse/precondition.hpp>
#include <boost/detail/preprocessor/sign/parse/initializers.hpp>
#include <boost/detail/preprocessor/sign/parse/throw.hpp>
#include <boost/detail/preprocessor/sign/parse/copyable.hpp>
#include <boost/detail/preprocessor/sign/parse/volatile.hpp>
#include <boost/detail/preprocessor/sign/parse/const.hpp>
#include <boost/detail/preprocessor/sign/parse/params.hpp>
#include <boost/detail/preprocessor/sign/parse/name.hpp>
#include <boost/detail/preprocessor/sign/parse/result_type.hpp>
#include <boost/detail/preprocessor/sign/parse/virtual.hpp>
#include <boost/detail/preprocessor/sign/parse/static.hpp>
#include <boost/detail/preprocessor/sign/parse/extern.hpp>
#include <boost/detail/preprocessor/sign/parse/inline.hpp>
#include <boost/detail/preprocessor/sign/parse/explicit.hpp>
#include <boost/detail/preprocessor/sign/parse/requires.hpp>
#include <boost/detail/preprocessor/sign/parse/template.hpp>
#include <boost/detail/preprocessor/sign/parse/export.hpp>
#include <boost/detail/preprocessor/sign/parse/access.hpp>
#include <boost/detail/preprocessor/sign/parse/body.hpp>
#include <boost/detail/preprocessor/sign/parse/start.hpp>

#define BOOST_LOCAL_AUX_PP_SIGN_PARSE_DECL(parenthesized_syntax_seq) \
    BOOST_LOCAL_AUX_PP_SIGN_VALIDATE( \
    BOOST_DETAIL_PP_SIGN_PARSE_FINISH( \
    BOOST_DETAIL_PP_SIGN_PARSE_POSTCONDITION_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_PRECONDITION_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INITIALIZERS_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_THROW_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_COPYABLE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_VOLATILE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_CONST_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_NAME_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_REQ( \
    BOOST_DETAIL_PP_SIGN_PARSE_VIRTUAL_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_STATIC_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXTERN_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_INLINE_OPT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPLICIT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_REQUIRES_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_EXPORT_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_ACCESS_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_NOT( \
    BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY( \
        parenthesized_syntax_seq, \
        ERROR_missing_local_function_signature \
    )))))))))))))))))))))))

#endif // #include guard

