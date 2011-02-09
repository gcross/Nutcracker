
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_AUX_IDENTITY_HPP_
#define BOOST_AUX_IDENTITY_HPP_

namespace boost { namespace aux {

// Identity for type expressions.
template<typename F> struct identity_type {}; // Empty so cannot be used.
template<typename T> struct identity_type< void (T) > { typedef T type; };

// Identity for value expressions (call overhead should be optimized away).
template<typename T> inline T const& identity_value(T const& value)
    { return value; }

}} // namespace boost::aux

#endif // #include guard

