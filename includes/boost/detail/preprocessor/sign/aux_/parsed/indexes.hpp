
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSED_INDEXES_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_INDEXES_HPP_

// NOTE: NIL always terminates sign to avoid arrays with last element
// terminated by `PP_EMPTY` (which cause expansion problems on MSVC...).

// IMPORTANT: The ..._INDEX_MAX cannot be larger than BOOST_PP_ARRAY max size
// which is 25. If it ever needs to eceed this limit, some elements could be 
// combined in sub-arrays or tuple (e.g., the params namespace, induced, and
// deduced could be represented as a 3-tuple using 1 signle array element
// instead of 3).

// Operations (functions, constructors, etc).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_BODY_INDEX                        0
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_ACCESS_INDEX                      1
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_EXPORT_INDEX                      2
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_TEMPLATE_PARAMS_INDEX             3
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_REQUIRES_INDEX                    4
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_EXPLICIT_INDEX                    5
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_INLINE_INDEX                      6
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_EXTERN_INDEX                      7
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_STATIC_INDEX                      8
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_VIRTUAL_INDEX                     9
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_RESULT_TYPE_INDEX                10
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_NAME_INDEX                       11
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_TAG_INDEX                 12
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_INDUCED_INDEX             13
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_PARAMS_DEDUCED_INDEX             14
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CONST_INDEX                      15
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_VOLATILE_INDEX                   16
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_COPYABLE_INDEX                   17
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_THROW_INDEX                      18
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_INITIALIZERS_INDEX               19
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_PRECONDITION_INDEX               20
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_POSTCONDITION_RESULT_INDEX       21
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_POSTCONDITION_INDEX              22
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_NIL_INDEX                        23
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_INDEX_MAX                        24

// Class.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_TYPE_INDEX                  0
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_BASES_INDEX                 1
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_STATIC_INVARIANT_INDEX      2
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_INVARIANT_INDEX             3
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_NIL_INDEX                   4
#define BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_INDEX_MAX                   5

#endif // #include guard

