
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_VALIDATE_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_VALIDATE_HPP_

// FUTURE: Implement this macro to validate parsed local function signature:
// no copyable params, no named params, name cannot be operator, etc.
// VALIDATE: If there is a `const bound` param type then
// 1) There must be only 1.
// 2) It must be the last one or the 2nd last one if the last is `bound`.
// 3) It must have no storage classifier or copyable.
// 4) It must have no default.
// VALIDATE: If there is a `bound` param type then
// 1) There must be only 1.
// 2) It must be the last one.
// 3) It must have no storage classifier or compyable.
// 4) It must have not default.
#define BOOST_LOCAL_AUX_PP_SIGN_VALIDATE(seq_sign_err) \
    seq_sign_err

#endif // #include guard

