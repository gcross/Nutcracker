
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_HPP_

#include "../../../parse/tagged_opt.hpp"
#include "../../../parse/tagged_not.hpp"
#include "../../../parse/return.hpp"
#include <boost/detail/preprocessor/keyword/is_namespace.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_ONEMPTY_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_an_empty_parameter_tag_namespace)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_APPEND_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE, \
            /* if no (namespace), Boost.Parameter default is `tag` */ \
            BOOST_PP_EMPTY BOOST_PP_EMPTY, /* 2 PP_EMPTY for default */ \
            /* handle (namespace)() empty seq (on C99) */ \
            BOOST_PP_IS_EMPTY, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_ONEMPTY_) 

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_NOT( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE, \
            BOOST_PP_EMPTY, /* 1 PP_EMPTY for none */ \
            ERROR_unexpected_parameter_tag_namespace)

#endif // #include guard

