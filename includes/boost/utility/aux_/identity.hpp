
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_AUX_IDENTITY_HPP_
#define BOOST_AUX_IDENTITY_HPP_

#include <boost/type_traits/add_reference.hpp>

namespace boost { namespace aux {

// Identity for value expressions (call overhead should be optimized away).
template<typename T>
inline typename boost::add_reference<T>::type identity_value(
        typename boost::add_reference<T>::type value) {
    return value;
}

}} // namespace boost::aux

#endif // #include guard

