
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_HPP_

#include "../aux_/parse/all_params/params.hpp"
#include "../aux_/parse/req.hpp"
#include "../aux_/parser/merge.hpp"
#include "../aux_/parser/seq_next.hpp"
#include "../aux_/parser/seq_skip.hpp"
#include "../parser/seq.hpp"

// Parse operation params (for func/operator, constr, destr, etc).

#define BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_OP_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                    seq_sign_err)), \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                    /* skip already parsed ( param-seq ) */ \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
                    seq_sign_err)))

#define BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_ANY_TOKEN_(token) \
    1 /* accept any token (even empty) as param seq to be parsed */

#define BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_REQ(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OP(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_ANY_TOKEN_, \
            BOOST_DETAIL_PP_SIGN_PARSE_PARAMS_OP_, \
            ERROR_missing_function_parameters)

#endif // #include guard

