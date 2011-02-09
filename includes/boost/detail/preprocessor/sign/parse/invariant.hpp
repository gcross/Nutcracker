
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_HPP_

#include "../aux_/parse/tagged_opt.hpp"
#include "../aux_/parse/return.hpp"
#include <boost/detail/preprocessor/keyword/contract/is_invariant.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
            
#define BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT_ONEMPTY_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_PARSER_RETURN_ERR(seq_sign_err, \
            ERROR_cannot_specify_invariant_with_no_assertion)

#define BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT_ISEMPTY_(seq) \
    BOOST_PP_NOT(BOOST_PP_SEQ_SIZE(seq))

#define BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_OPT(seq_sign_err, \
            BOOST_DETAIL_PP_KEYWORD_IS_INVARIANT, \
            (BOOST_PP_NIL), /* if no (invariant), no assertions */ \
            /* handle (invariant)() empty seq (on C99) */ \
            BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT_ISEMPTY_, \
            BOOST_DETAIL_PP_SIGN_PARSE_INVARIANT_OPT_ONEMPTY_)

#endif // #include guard

