
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_DECL_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_DECL_HPP_

#include "code_/binding.hpp"
#include "code_/functor.hpp"
#include "../symbol.hpp"
#include "../preprocessor/sign/parse_decl.hpp"
#include <boost/detail/preprocessor/sign/params.hpp>
#include <boost/detail/preprocessor/sign/parser/apply.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/scope_exit.hpp> // Use Boost.ScopeExit internal implementation.

// Undefine local function bound args global variable. Actual declaration of
// this variable is made using SFINAE mechanisms by each local function macro.
extern boost::scope_exit::aux::undeclared
        BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME;

#define BOOST_LOCAL_AUX_FUNCTION_DECL_( \
        sign, params, id, typename_keyword) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING( \
            sign, id, typename_keyword) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR( \
            sign, params, id, typename_keyword)

#define BOOST_LOCAL_AUX_FUNCTION_DECL_OK_(sign, typename_keyword) \
    BOOST_LOCAL_AUX_FUNCTION_DECL_(sign, \
            BOOST_DETAIL_PP_SIGN_PARAMS(sign), \
            /* use `name ## __LINE__` so functions can appear on same line */ \
            BOOST_PP_CAT(BOOST_DETAIL_PP_SIGN_NAME(sign), __LINE__), \
            typename_keyword)

#define BOOST_LOCAL_AUX_FUNCTION_DECL_ERR_(message, unused) \
    ; /* close eventaul previous staments, otherwise no effect */ \
    BOOST_MPL_ASSERT_MSG(false /* assertion always fails */, message, ()) \
    ; /* must close ASSERT macro for eventual use within class scope */ 

// Public API.

// Must be followed by function body definition and closing class brakets
// `{ ... } LOCAL_FUNCTION_END ` (so to avoid wrapping function body code
// within macro param and retain original compiler error line numbers for
// body code).
#define BOOST_LOCAL_AUX_FUNCTION_DECL( \
        parenthesized_syntax_seq, typename_keyword) \
    BOOST_DETAIL_PP_SIGN_PARSER_APPLY( \
            BOOST_LOCAL_AUX_PP_SIGN_PARSE_DECL( \
                    parenthesized_syntax_seq), \
            BOOST_LOCAL_AUX_FUNCTION_DECL_OK_, \
            BOOST_LOCAL_AUX_FUNCTION_DECL_ERR_, \
            typename_keyword)

#endif // #include guard

