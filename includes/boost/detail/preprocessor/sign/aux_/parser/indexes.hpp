
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_INDEXES_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_INDEXES_HPP_

// Parser's data is a 3-tuple `(seq, sign, err)` where:
// 0) seq -- sequence left still to be parsed (or `(NIL)`).
// 1) sign -- signature parsed so far (or `(NIL)`).
// 2) err -- if there is an error or not (with related error message).

#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_INDEX    0
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_INDEX   1
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_INDEX    2
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_INDEX_MAX    3

#endif // #include guard

