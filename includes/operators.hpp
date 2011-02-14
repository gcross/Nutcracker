//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1758: * @thin operators.hpp
//@@language cplusplus

#ifndef NUTCRACKER_OPERATORS_HPP
#define NUTCRACKER_OPERATORS_HPP

//@+<< Includes >>
//@+node:gcross.20110206185121.1759: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/container/vector.hpp>
#include <boost/move/move.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <complex>

#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1760: ** << Usings >>
using boost::assign::list_of;
using boost::container::vector;
using boost::copy;
using boost::numeric::ublas::matrix;
using boost::reverse_copy;
using boost::shared_ptr;
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1937: ** Tensors
//@+node:gcross.20110214164734.1938: *3* OperatorSite
class OperatorSite : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OperatorSite)

    unsigned int number_of_matrices;
    uint32_t* index_data;
public:
    OperatorSite()
      : number_of_matrices(0)
      , index_data(NULL)
    { }

    ~OperatorSite() { if(index_data) delete[] index_data; }

    OperatorSite(BOOST_RV_REF(OperatorSite) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
      , number_of_matrices(copyAndReset(other.number_of_matrices))
      , index_data(copyAndReset(other.index_data))
    { }

    OperatorSite(
          unsigned int const number_of_matrices
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,number_of_matrices*(*physical_dimension)*(*physical_dimension)
        )
      , number_of_matrices(number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    { }

    OperatorSite(
          CopyFrom<OperatorSite const> const other
    ) : SiteBaseTensor(other)
      , number_of_matrices(other->number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    {
        copy(other->index_data,other->index_data+2*number_of_matrices,index_data);
    }

    template<typename G1, typename G2> OperatorSite(
          unsigned int const number_of_matrices
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G1> const index_generator
        , FillWithGenerator<G2> const matrix_generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,number_of_matrices*(*physical_dimension)*(*physical_dimension)
            ,matrix_generator
        )
      , number_of_matrices(number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    {
        BOOST_CONCEPT_ASSERT(( Generator<G1,uint32_t> ));
        uint32_t* index = index_data;
        REPEAT(number_of_matrices) {
            uint32_t left_index = (*index_generator)()
                   , right_index = (*index_generator)()
                   ;
            assert(left_index >= 1 && left_index <= *leftDimension());
            assert(right_index >= 1 && right_index <= *rightDimension());
            *index++ = left_index;
            *index++ = right_index;
        }
        //generate_n(index_data.get(),size,*index_generator);
    }

    template<typename Range1, typename Range2> OperatorSite(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range1> const index_init
        , FillWithRange<Range2> const matrix_init
    ) : SiteBaseTensor(
             PhysicalDimension((unsigned int)sqrt(matrix_init->size()/(index_init->size()/2)))
            ,left_dimension
            ,right_dimension
            ,matrix_init
        )
      , number_of_matrices(index_init->size()/2)
      , index_data(new uint32_t[index_init->size()])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range1> ));
        copy(*index_init,index_data);
    }

    OperatorSite(MakeTrivial const make_trivial)
      : SiteBaseTensor(make_trivial)
      , number_of_matrices(1)
      , index_data(new uint32_t[2])
    {
        fill_n(index_data,2,1);
    }

    OperatorSite& operator=(BOOST_RV_REF(OperatorSite) other) {
        if(this == &other) return *this;
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
        number_of_matrices = copyAndReset(other.number_of_matrices);
        moveArrayToFrom(index_data,other.index_data); 
        return *this;
    }

    void swap(OperatorSite& other) {
        if(this == &other) return;
        SiteBaseTensor::swap(other);
        std::swap(number_of_matrices,other.number_of_matrices);
        std::swap(index_data,other.index_data);
    }

    unsigned int numberOfMatrices() const { return number_of_matrices; }

    operator uint32_t*() { return index_data; }
    operator uint32_t const*() const { return index_data; }

    static OperatorSite const trivial;
};
//@+node:gcross.20110207005827.1775: ** Type aliases
typedef matrix<complex<double> > Matrix;
typedef vector<shared_ptr<OperatorSite const> > Operators;
//@+node:gcross.20110214164734.1939: ** Classes
//@+node:gcross.20110206185121.1761: *3* OperatorLink
struct OperatorLink {
    unsigned int from, to;
    Matrix matrix;

    OperatorLink(
          unsigned int const from
        , unsigned int const to
        , Matrix const& matrix
    ) : from(from)
      , to(to)
      , matrix(matrix)
    { }

    OperatorLink(OperatorLink const& other)
      : from(other.from)
      , to(other.to)
      , matrix(other.matrix)
    { }

    OperatorLink(BOOST_RV_REF(OperatorLink) other)
      : from(copyAndReset(other.from))
      , to(copyAndReset(other.to))
      , matrix()
    { matrix.swap(other.matrix); }

    OperatorLink& operator=(OperatorLink const& other) {
        from = other.from;
        to = other.to;
        matrix = other.matrix;
        return *this;
    }    

    OperatorLink& operator=(BOOST_RV_REF(OperatorLink) other) {
        from = copyAndReset(other.from);
        to = copyAndReset(other.to);
        matrix.resize(0,false);
        matrix.swap(other.matrix);
        return *this;
    }
};
//@+node:gcross.20110206185121.1771: ** Functions
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorLink> const& links
);

Operators constructExternalFieldOperators(
      unsigned int const number_of_operators
    , Matrix const& matrix
);

Operators constructTransverseIsingModelOperators(
      unsigned int const number_of_operators
    , double spin_coupling_strength
);

Matrix identityMatrix(unsigned int const n);

//@+others
//@+node:gcross.20110207005827.1774: *3* diagonalMatrix
template<typename T> Matrix diagonalMatrix(T const& data) {
    unsigned int const n = data.size();
    Matrix matrix(n,n);
    matrix.clear();
    unsigned int i = 0; 
    typename T::const_iterator px = data.begin();
    while(px != data.end()) { matrix(i,i) = *px; ++i; ++px; }
    return matrix;
}
//@+node:gcross.20110207005827.1771: *3* squareMatrix
template<typename T> Matrix squareMatrix(T const& data) {
    unsigned int const n = (unsigned int)sqrt(data.size());
    assert(n*n == data.size());
    Matrix matrix(n,n);
    copy(data,matrix.data().begin());
    return matrix;
}
//@-others
//@+node:gcross.20110207005827.1772: ** Values
namespace Pauli {
    Matrix const
         I = identityMatrix(2)
        ,X = squareMatrix(list_of
                (0)(1)
                (1)(0)
             )
        ,Y = squareMatrix(list_of
                (c(0,0))(c(0,-1))
                (c(0,1))(c(0,0))
             )
        ,Z = diagonalMatrix(list_of(1)(-1))
        ;
}
//@-others

}

#endif
//@-leo
