
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_HPP_

#include "../parser/seq.hpp"
#include "../aux_/parser/sign_nil.hpp"
#include "../aux_/parser/merge.hpp"
#include "../aux_/parser/seq_next.hpp"
#include "../aux_/parser/seq_skip.hpp"
#include "../aux_/parse/if.hpp"
#include "../aux_/parse/not.hpp"
#include "../aux_/parse/all_params/tparams.hpp"
#include <boost/detail/preprocessor/keyword/is_template.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_NO_(seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL /* no tparams */ )

#define BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_YES_(seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE(seq_sign_err, \
            /* after (template), parse template params */ \
            BOOST_DETAIL_PP_SIGN_PARSE_TPARAMS( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                    /* skip already parsed (template) */ \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
                    seq_sign_err))), \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                    /* skip already parsed (template)( tparam-seq ) */ \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
                    seq_sign_err))) \
    )

#define BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE, \
        BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_YES_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_NO_ \
    , ~)

#define BOOST_DETAIL_PP_SIGN_PARSE_TEMPLATE_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_WITH_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL /* no tparams */, \
            ERROR_unexpected_template_specifier)

#endif // #include guard

