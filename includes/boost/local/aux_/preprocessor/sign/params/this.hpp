
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_HPP_

#include <boost/detail/preprocessor/keyword/is_this.hpp>
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/logical/bool.hpp>
#include <boost/preprocessor/seq/remove.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_FIND_( \
        params) \
    BOOST_DETAIL_PP_NILSEQ_FIND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_NAMES( \
                    params), \
            BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_YES_( \
        bind_names_nilseq, this_index) \
    BOOST_PP_SEQ_REMOVE(bind_names_nilseq, this_index)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_NO_( \
        bind_names_nilseq, this_index) \
    bind_names_nilseq

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_( \
        bind_names_nilseq, this_index) \
    BOOST_PP_IF(this_index, \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_YES_ \
    , \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_NO_ \
    )(bind_names_nilseq, this_index)

// Public API.

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_CONST_BIND_THIS(params) \
    BOOST_PP_BOOL(BOOST_DETAIL_PP_NILSEQ_FIND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_NAMES( \
                    params), \
            BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS))

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND_THIS(params) \
    BOOST_PP_BOOL(BOOST_DETAIL_PP_NILSEQ_FIND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES(params), \
            BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS))

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ALL_BIND_THIS(params) \
    BOOST_PP_BOOL( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_FIND_( \
                    params))

// Expand this macro iff `HAS_ALL_BIND_THIS()` is true (otherwise pp-error).
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_SEQ_INDEX( \
        params) \
    BOOST_PP_DEC( /* decrement to skip leading `PP_NIL` of nil-seq */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_FIND_( \
            params))

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_NAME_REMOVE( \
        bind_names_nilseq) \
    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_REMOVE_THIS_( \
            bind_names_nilseq, \
            BOOST_DETAIL_PP_NILSEQ_FIND(bind_names_nilseq, \
                    BOOST_DETAIL_PP_KEYWORD_IS_TRAILING_THIS))

#endif // #include guard

