
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_FINISH_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_FINISH_HPP_

#include "../parser/seq.hpp"
#include "../parser/sign.hpp"
#include "../parser/err.hpp"
#include <boost/preprocessor/seq/to_tuple.hpp>
#include <boost/preprocessor/seq/seq.hpp> // For `PP_SEQ_TAIL()`.
#include <boost/preprocessor/seq/to_array.hpp>

// Signature is represented as a seq during parsing (for efficient push-back
// operations) but it is represented as an array after parsing (for efficient
// random access of signature traits). These convert signature seq to array.
// NOTE: The array is used instead of a typle because sometimes the random
// access is needed before the signature has been fully parsed. In these cases
// the total size is not known a-priory so the array conversion will just use
// the number of sign elems parsed so far and still allow for random access.

// NOTE: These macro intentionally do not check for errors. Make sure
// seq_sign_err is validated before it is passed to these macros.

// Convert and return just sign. Err should be 0.
#define BOOST_DETAIL_PP_SIGN_PARSE_FINISH_SIGN_(seq_sign_err) \
    BOOST_PP_SEQ_TO_ARRAY(BOOST_PP_SEQ_TAIL( \
            BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err) \
            /* terminate array with NIL to avoid EMPTY terminated arrays */ \
            (BOOST_PP_NIL) \
            ))

// Convert sign and return (seq, converted-sign, err). If err != 0 it will be
// returned as such but sign convertion will still be done.
#define BOOST_DETAIL_PP_SIGN_PARSE_FINISH(seq_sign_err) \
    ( \
        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSE_FINISH_SIGN_(seq_sign_err) \
    , \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR(seq_sign_err) \
    )

#endif // #include guard

