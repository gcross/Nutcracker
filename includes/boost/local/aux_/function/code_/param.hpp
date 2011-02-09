
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_HPP_

#include "../../symbol.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use Boost.ScopeExit internal impl.
#include <boost/detail/preprocessor/keyword/is_this.hpp>
#include <boost/detail/preprocessor/sign/param/classifier.hpp>
#include <boost/detail/preprocessor/sign/param/type.hpp>
#include <boost/detail/preprocessor/sign/param/name.hpp>
#include <boost/detail/preprocessor/sign/param/default.hpp>
#include <boost/detail/type_traits/add_pointed_const.hpp>
#include <boost/type_traits.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/control/expr_if.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/cat.hpp>

// Param named `this`.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_TYPE_(id) \
    BOOST_PP_CAT(se_this_type, id)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_NAME_ \
    se_this

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_RENAME_THIS_( \
        param_name) \
    /* can't use `PP_KEYWORD_IS_THIS()` because some `param_name` might */ \
    /* prefixed by `&` symbol (which can't be `PP_CAT()`) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS( \
            param_name), \
        BOOST_LOCAL_AUX_SYMBOL_THIS_PARAM_NAME \
    , \
        param_name \
    )

// Tag param.

// Adapted from `BOOST_SCOPE_EXIT_AUX_TAG_DECL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_TAG_DECL(r, id, i, var) \
    typedef void (*BOOST_SCOPE_EXIT_AUX_TAG(id,i))(int \
            BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_RENAME_THIS_(var) );

// Bound param (const and non-const).

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_TYPE( \
        id_typename_offset, i, var) \
    BOOST_SCOPE_EXIT_AUX_PARAMS_T( \
            BOOST_PP_TUPLE_ELEM(3, 0, id_typename_offset)):: \
    BOOST_SCOPE_EXIT_AUX_PARAM_T( \
            BOOST_PP_TUPLE_ELEM(3, 0, id_typename_offset), \
            BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(3, 2, id_typename_offset)),\
            var)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_VALUE( \
        id_offset, i, var) \
    BOOST_LOCAL_AUX_SYMBOL_PARAMS_VARIABLE_NAME-> \
            BOOST_SCOPE_EXIT_AUX_PARAM( \
                    BOOST_PP_TUPLE_ELEM(2, 0, id_offset), \
                    BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(2, 1, id_offset)), \
                    var).value

// Adapted from `BOOST_SCOPE_EXIT_AUX_ARG_DECL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_DECL_( \
        r, id_typename_offset, i, var, const01) \
    BOOST_PP_COMMA_IF(i) \
    BOOST_PP_TUPLE_ELEM(3, 1, id_typename_offset) /* eventual typename */ \
    BOOST_PP_EXPR_IF(const01, \
        BOOST_PP_IIF( \
                BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS(var), \
            ::contract::detail::add_pointed_const< /* pointed obj const */ \
        , \
            ::boost::add_const< /* outer type const */ \
        ) \
        BOOST_PP_TUPLE_ELEM(3, 1, id_typename_offset) /* eventual typename */ \
    ) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_TYPE( \
            id_typename_offset, i, var) \
    BOOST_PP_EXPR_IF(const01, >::type) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_RENAME_THIS_(var)
    
#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_CONST_BIND_DECL( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_DECL_( \
            r, id_typename_offset, i, var, 1 /* force const */)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_BIND_DECL( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_DECL_( \
            r, id_typename_offset, i, var, 0 /* do not force const */)

// Adapted from `BOOST_SCOPE_EXIT_AUX_ARG()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND( \
        r, id_offset, i, var) \
    BOOST_PP_COMMA_IF(i) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND_VALUE( \
            id_offset, i, var)

// Unbind param.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_DECL( \
        z, param, unused) \
    BOOST_DETAIL_PP_SIGN_PARAM_CLASSIFIER(param) /* opt auto/register */ \
    BOOST_DETAIL_PP_SIGN_PARAM_TYPE(param) \
    BOOST_DETAIL_PP_SIGN_PARAM_NAME(param)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_DECL_WITH_DEFAULT( \
        z, param, unused) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_DECL( \
            z, param, unused) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARAM_HAS_DEFAULT(param), \
        = BOOST_DETAIL_PP_SIGN_PARAM_DEFAULT \
    , \
        BOOST_PP_TUPLE_EAT(1) \
    )(param)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_NAME( \
        z, param, unused) \
    BOOST_DETAIL_PP_SIGN_PARAM_NAME(param)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_TYPE( \
        z, param, unused) \
    BOOST_DETAIL_PP_SIGN_PARAM_TYPE(param)

#endif // #include guard

