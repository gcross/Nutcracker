
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_HPP_

#include "../../../parser/merge.hpp"
#include "../../../parser/seq_next.hpp"
#include "../../../parser/sign_nil.hpp"
#include "../../../../parser/seq.hpp"
#include "../../../../parser/sign.hpp"
#include "../../../../parser/err.hpp"
#include <boost/detail/preprocessor/keyword/parameter/is_deduced.hpp>
#include <boost/preprocessor/logical/and.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/comparison/greater.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/control/while.hpp>

// All params (while-loop).

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_P_(n, seq_sign_err) \
    BOOST_PP_AND( \
          BOOST_PP_NOT(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err)) \
        , BOOST_PP_AND( \
              BOOST_PP_NOT(BOOST_DETAIL_PP_KEYWORD_IS_DEDUCED( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                    seq_sign_err))) \
            , BOOST_PP_GREATER(BOOST_PP_SEQ_SIZE( \
                    BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), 1) \
        ) \
    ) 

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_WHILE_D_( \
        d, op_macro, seq_sign_err) \
    BOOST_PP_WHILE_ ## d(BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_P_, \
            op_macro, seq_sign_err)

// Wraps params sign into a nil-seq `( seq, (NIL) (sign), err )`.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_SIGNSEQ_(d, seq_sign_err) \
    ( \
        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL /* handle empty seq */ \
        ( BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err) ) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_D( \
        d, op_macro, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE2(seq_sign_err, \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_SIGNSEQ_(d, \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_WHILE_D_(d, op_macro, \
        ( \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
        , \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL \
        , \
            BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
        ) \
    )))

#endif // #include guard

