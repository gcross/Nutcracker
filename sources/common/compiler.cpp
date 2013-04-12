#include <boost/container/set.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/for_each.hpp>
#include <boost/range/algorithm/fill.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/unordered_set.hpp>

#include "nutcracker/compiler.hpp"

namespace Nutcracker {

using boost::container::map;
using boost::container::set;
using boost::fill;
namespace lambda = boost::lambda;
using boost::make_shared;
using boost::make_tuple;
using boost::irange;
using boost::scoped_array;
using boost::sort;
using boost::transform;
using boost::unordered_set;

using std::iterator_traits;
using std::make_pair;

Operator OperatorBuilder::compile(bool optimize, bool add_start_and_end_loops) {
    OperatorSpecification source = generateSpecification(add_start_and_end_loops);
    if(optimize) source.optimize();
    return source.compile();
}
OperatorSpecification OperatorBuilder::generateSpecification(bool add_start_and_end_loops) {
    OperatorSpecification specification = Base::generateSpecification();
    if(add_start_and_end_loops) {
        BOOST_FOREACH(unsigned int const site_number, irange((size_t)0u,connections.size())) {
            specification.connect(site_number,specification.getStartSignal(),specification.getStartSignal(),specification.lookupIdOfIdentityWithDimension(sites[site_number]));
            specification.connect(site_number,specification.getEndSignal(),  specification.getEndSignal(),  specification.lookupIdOfIdentityWithDimension(sites[site_number]));
        }
    }
    return boost::move(specification);
}
State StateBuilder::compile(bool optimize) {
    StateSpecification source = generateSpecification();
    if(optimize) source.optimize();
    return source.compile();
}
StateSpecification StateBuilder::generateSpecification() {
    return Base::generateSpecification();
}
SignalTable::SignalTable()
  : next_free_signal(3)
{}
unsigned int SignalTable::allocateSignal() {
    return next_free_signal++;
}
void SignalTable::reserveSignalsBelow(unsigned int exclusive_upper_bound) {
    next_free_signal = std::max(next_free_signal,exclusive_upper_bound);
}

}
