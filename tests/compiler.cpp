//@+leo-ver=5-thin
//@+node:gcross.20110805222031.4665: * @file compiler.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110805222031.4666: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

//@+<< Includes >>
//@+node:gcross.20110805222031.4667: ** << Includes >>
#include "compiler.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

#include <boost/lambda/lambda.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/adaptor/uniqued.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>

using boost::adaptors::map_keys;
using boost::adaptors::map_values;
using boost::adaptors::uniqued;
using boost::container::set;
using boost::generate;
using boost::make_shared;
using boost::numeric::ublas::zero_matrix;

using std::make_pair;

using namespace Pauli;
//@-<< Includes >>

//@+others
//@+node:gcross.20110817224742.2489: ** Classes
class TestingOperatorSpecification : public OperatorSpecification {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(TestingOperatorSpecification)
public:
    TestingOperatorSpecification() {}
    TestingOperatorSpecification(BOOST_RV_REF(OperatorSpecification) other) : OperatorSpecification(other) {}
    vector<SiteConnections> const& getConnections() { return connections; }
};
//@+node:gcross.20110805222031.4668: ** Tests
TEST_SUITE(Compiler) {

//@+others
//@+node:gcross.20110827215622.2603: *3* DataTable specializations
TEST_SUITE(DataTable_specializations) {

//@+others
//@+node:gcross.20110805222031.4670: *4* MatrixTable
TEST_SUITE(MatrixTable) {

//@+others
//@+node:gcross.20110805222031.4671: *5* initial matrices
TEST_SUITE(initial_matrices) {

//@+others
//@+node:gcross.20110805222031.4672: *6* Null
TEST_CASE(Null) {
    EXPECT_EQ_VAL(MatrixTable().lookupIdOf(make_shared<Matrix>(2,2,c(0,0))),0);
}
//@+node:gcross.20110805222031.4674: *6* I
TEST_SUITE(I) {
//@+others
//@+node:gcross.20110815001337.2461: *7* correct add
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(I),matrix_table.getIMatrixId());
}
//@+node:gcross.20110815001337.2454: *7* correct lookup
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getIMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),I->data()))
}
//@-others
}
//@+node:gcross.20110805222031.4676: *6* X
TEST_SUITE(X) {
//@+others
//@+node:gcross.20110815001337.2463: *7* correct add
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(X),matrix_table.getXMatrixId());
}
//@+node:gcross.20110815001337.2456: *7* correct lookup
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getXMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),X->data()))
}
//@-others
}
//@+node:gcross.20110805222031.4680: *6* Y
TEST_SUITE(Y) {
//@+others
//@+node:gcross.20110815001337.2465: *7* correct add
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(Y),matrix_table.getYMatrixId());
}
//@+node:gcross.20110815001337.2458: *7* correct lookup
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getYMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),Y->data()))
}
//@-others
}
//@+node:gcross.20110805222031.4678: *6* Z
TEST_SUITE(Z) {
//@+others
//@+node:gcross.20110815001337.2467: *7* correct add
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(Z),matrix_table.getZMatrixId());
}
//@+node:gcross.20110815001337.2460: *7* correct lookup
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getZMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),Z->data()))
}
//@-others
}
//@-others

}
//@+node:gcross.20110805222031.4681: *5* lookupIdOf
TEST_SUITE(lookupIdOf) {

//@+others
//@+node:gcross.20110805222031.4682: *6* correct for new matrix
TEST_CASE(correct_for_new_matrix) {
    MatrixTable matrix_table;
    RNG random;
    typedef vector<pair<unsigned int,shared_ptr<Matrix const> > > PreviousMatrices;
    PreviousMatrices previous_matrices;
    REPEAT(100) {
        unsigned int const dimension = random;
        shared_ptr<Matrix> matrix(new Matrix(dimension,dimension));
        generate(matrix->data(),random.randomComplexDouble);

        unsigned int const matrix_id = matrix_table.lookupIdOf(matrix);
        ASSERT_EQ(matrix_table.getSizeOf(matrix_id),dimension);
        ASSERT_TRUE(boost::equal(matrix_table.get(matrix_id)->data(),matrix->data()));

        BOOST_FOREACH(PreviousMatrices::const_reference p, previous_matrices) {
            ASSERT_TRUE(boost::equal(matrix_table.get(p.first)->data(),p.second->data()));
        }

        previous_matrices.emplace_back(matrix_id,matrix);
    }
}
//@+node:gcross.20110805222031.4684: *6* correct for repeated matrix
TEST_CASE(correct_for_repeated_matrix) {
    MatrixTable matrix_table;
    RNG random;
    typedef vector<pair<unsigned int,shared_ptr<Matrix const> > > PreviousMatrices;
    PreviousMatrices previous_matrices;
    REPEAT(100) {
        unsigned int const dimension = random;
        shared_ptr<Matrix> matrix(new Matrix(dimension,dimension));
        generate(matrix->data(),random.randomComplexDouble);

        unsigned int const matrix_id = matrix_table.lookupIdOf(matrix);
        ASSERT_EQ_VAL(matrix_table.lookupIdOf(matrix),matrix_id)

        BOOST_FOREACH(PreviousMatrices::reference p, previous_matrices) {
            ASSERT_EQ(matrix_table.lookupIdOf(p.second),p.first);
        }

        previous_matrices.emplace_back(matrix_id,matrix);
    }
}
//@-others

}
//@+node:gcross.20110805222031.4702: *5* lookupIdOfIdentityWithDimension
TEST_SUITE(lookupIdOfIdentityWithDimension) {

//@+others
//@+node:gcross.20110805222031.4703: *6* returns_I_for_dimension_2
TEST_CASE(returns_I_for_dimension_2) {
    MatrixTable matrix_table;
    EXPECT_EQ(matrix_table.lookupIdOfIdentityWithDimension(2),matrix_table.getIMatrixId())
}
//@-others

}
//@-others

}
//@+node:gcross.20110827215622.2583: *4* VectorTable
TEST_SUITE(VectorTable) {

//@+others
//@+node:gcross.20110827215622.2611: *5* lookupIdOf
TEST_SUITE(lookupIdOf) {

//@+others
//@+node:gcross.20110827215622.2612: *6* correct for new vector
TEST_CASE(correct_for_new_vector) {
    VectorTable table;
    RNG random;
    typedef vector<pair<unsigned int,VectorConstPtr> > PreviousVectors;
    PreviousVectors previous_vectors;
    REPEAT(100) {
        unsigned int const dimension = random;
        VectorConstPtr vector = random.randomVector(dimension);

        unsigned int const id = table.lookupIdOf(vector);
        ASSERT_EQ(table.getSizeOf(id),dimension);
        ASSERT_TRUE(boost::equal(*table.get(id),*vector));

        BOOST_FOREACH(PreviousVectors::const_reference p, previous_vectors) {
            ASSERT_TRUE(boost::equal(*table.get(p.first),*p.second));
        }

        previous_vectors.emplace_back(id,vector);
    }
}
//@+node:gcross.20110827215622.2613: *6* correct for repeated vector
TEST_CASE(correct_for_repeated_vector) {
    VectorTable table;
    RNG random;
    typedef vector<pair<unsigned int,VectorTable::DataConstPtr> > PreviousVectors;
    PreviousVectors previous_vectors;
    REPEAT(100) {
        unsigned int const dimension = random;
        VectorTable::DataConstPtr vector = random.randomVector(dimension);

        unsigned int const id = table.lookupIdOf(vector);
        ASSERT_EQ_VAL(table.lookupIdOf(vector),id)

        BOOST_FOREACH(PreviousVectors::reference p, previous_vectors) {
            ASSERT_EQ(table.lookupIdOf(p.second),p.first);
        }

        previous_vectors.emplace_back(id,vector);
    }
}
//@-others

}
//@+node:gcross.20110827215622.2606: *5* lookupIdOfObservation
TEST_CASE(lookupIdOfObservation) {
    RNG random;
    REPEAT(100) {
        unsigned int const
            dimension = random(1,10),
            observation = random(0,dimension-1);
        VectorTable table;
        unsigned int id = table.lookupIdOfObservation(observation,dimension);
        Vector const& data = *table.get(id);
        ASSERT_EQ_VAL(data.size(),dimension);
        Vector correct_data(dimension,c(0,0));
        correct_data[observation] = c(1,0);
        ASSERT_TRUE(boost::equal(correct_data,data));
        ASSERT_EQ_VAL(table.lookupIdOfObservation(observation,dimension),id);
        ASSERT_EQ_VAL(table.lookupIdOf(make_shared<Vector>(correct_data)),id);
    }
}
//@+node:gcross.20110827215622.2585: *5* null
TEST_CASE(null) {
    EXPECT_EQ_VAL(VectorTable().lookupIdOfRange(vector<complex<double> >()),0);
}
//@-others

}
//@-others

}
//@+node:gcross.20110821165641.2510: *3* OperatorBuilder
TEST_SUITE(OperatorBuilder) {

//@+others
//@+node:gcross.20110822214054.2517: *4* addGlobalExternalField
TEST_CASE(addGlobalExternalField) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(1u,5u)) {
        checkOperatorsEqual(
            OperatorBuilder()
                .addSites(number_of_sites,PhysicalDimension(2u))
                .addGlobalExternalField(OperatorBuilder::getZMatrixId())
                .compile()
            ,
            constructExternalFieldOperator(number_of_sites,Pauli::Z)
        );
    }
}
//@+node:gcross.20110822214054.2521: *4* addGlobalNeighborCouplingField
TEST_CASE(addGlobalNeighborCouplingField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        Operator op =
            OperatorBuilder()
                .addSites(number_of_sites,PhysicalDimension(2u))
                .addGlobalExternalField(OperatorBuilder::getZMatrixId())
                .addGlobalNeighborCouplingField(OperatorBuilder::getXMatrixId(),OperatorBuilder::getXMatrixId(),0.5)
                .compile();
        ASSERT_EQ_VAL(op[0]->numberOfMatrices(),3u);
        BOOST_FOREACH(unsigned int const site_number,irange(1u,number_of_sites-1)) {
            ASSERT_EQ_VAL(op[site_number]->numberOfMatrices(),5u);
        }
        ASSERT_EQ_VAL(op[number_of_sites-1]->numberOfMatrices(),3u);
        checkOperatorsEquivalent(
            op,
            constructTransverseIsingModelOperator(number_of_sites,0.5),
            random
        );
    }
}
//@+node:gcross.20110822214054.2519: *4* addLocalExternalField
TEST_CASE(addLocalExternalField) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(1u,5u)) {
        OperatorBuilder builder;
        builder.addSites(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
            builder.addLocalExternalField(site_number,builder.getZMatrixId());
        }
        checkOperatorsEqual(
            builder.compile(),
            constructExternalFieldOperator(number_of_sites,Pauli::Z)
        );
    }
}
//@+node:gcross.20110822214054.2523: *4* addLocalNeighborCouplingField
TEST_CASE(addLocalNeighborCouplingField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        OperatorBuilder builder;
        builder.addSites(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
            builder.addLocalExternalField(site_number,OperatorBuilder::getZMatrixId());
            if(site_number+1 < number_of_sites) {
                builder.addLocalNeighborCouplingField(site_number,builder.getXMatrixId(),builder.getXMatrixId(),0.5);
            }
        }
        Operator op = builder.compile();
        ASSERT_EQ_VAL(op[0]->numberOfMatrices(),3u);
        BOOST_FOREACH(unsigned int const site_number,irange(1u,number_of_sites-1)) {
            ASSERT_EQ_VAL(op[site_number]->numberOfMatrices(),5u);
        }
        ASSERT_EQ_VAL(op[number_of_sites-1]->numberOfMatrices(),3u);
        checkOperatorsEquivalent(
            op,
            constructTransverseIsingModelOperator(number_of_sites,0.5),
            random
        );
    }
}
//@+node:gcross.20110826085250.2532: *4* addTerm
TEST_CASE(addTerm) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        OperatorBuilder builder;
        builder.addSites(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            vector<unsigned int> components;
            REPEAT(site_number) { components.push_back(builder.getIMatrixId()); }
            components.push_back(builder.getZMatrixId());
            REPEAT(number_of_sites-site_number-1) { components.push_back(builder.getIMatrixId()); }
            builder.addTerm(components);
        }
        Operator op = builder.compile();
        ASSERT_EQ_VAL(op[0]->numberOfMatrices(),2u);
        BOOST_FOREACH(unsigned int const site_number,irange(1u,number_of_sites-1)) {
            ASSERT_EQ_VAL(op[site_number]->numberOfMatrices(),3u);
        }
        ASSERT_EQ_VAL(op[number_of_sites-1]->numberOfMatrices(),2u);
        checkOperatorsEquivalent(
            op,
            constructExternalFieldOperator(number_of_sites,Pauli::Z),
            random
        );
    }
}
//@+node:gcross.20110821165641.2511: *4* generateSpecification
TEST_SUITE(generateSpecification) {

//@+others
//@+node:gcross.20110821165641.2513: *5* single connections
TEST_CASE(single_connections) {
    OperatorBuilder builder;
    builder.addSite(2);
    builder.connect(0u,0u,0u,builder.getIMatrixId());
    builder.connect(0u,0u,1u,builder.getXMatrixId());
    builder.connect(0u,0u,2u,builder.getYMatrixId());
    builder.connect(0u,0u,3u,builder.getZMatrixId());
    TestingOperatorSpecification spec(builder.generateSpecification(false));
    SiteConnections correct_connections;
    correct_connections[make_pair(0u,0u)] = spec.getIMatrixId();
    correct_connections[make_pair(0u,1u)] = spec.getXMatrixId();
    correct_connections[make_pair(0u,2u)] = spec.getYMatrixId();
    correct_connections[make_pair(0u,3u)] = spec.getZMatrixId();
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),4u);
    EXPECT_TRUE(spec.getConnections()[0] == correct_connections);
}
//@+node:gcross.20110821165641.2515: *5* multi-connections
TEST_CASE(multiconnections) {
    OperatorBuilder builder;
    builder.addSite(2);
    builder.connect(0u,0u,0u,builder.getIMatrixId());
    builder.connect(0u,0u,0u,builder.getXMatrixId());
    builder.connect(0u,0u,0u,builder.getYMatrixId());
    builder.connect(0u,0u,0u,builder.getZMatrixId());
    TestingOperatorSpecification spec(builder.generateSpecification(false));
    SiteConnections correct_connections;
    MatrixConstPtr matrix = squareMatrix(list_of(c(2,0))(c(1,-1))(c(1,1))(c(0,0)));
    correct_connections[make_pair(0u,0u)] = spec.lookupIdOf(matrix);
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u);
    EXPECT_TRUE(spec.getConnections()[0] == correct_connections);
}
//@+node:gcross.20110821165641.2512: *5* standard loops
TEST_CASE(standard_loops) {
    OperatorBuilder builder;
    REPEAT(5) {
        builder.addSite(2);
    }
    TestingOperatorSpecification spec(builder.generateSpecification());
    SiteConnections correct_connections;
    correct_connections[make_pair(spec.getStartSignal(),spec.getStartSignal())] = spec.getIMatrixId();
    correct_connections[make_pair(spec.getEndSignal(),spec.getEndSignal())] = spec.getIMatrixId();
    BOOST_FOREACH(SiteConnections const& connections, spec.getConnections()) {
        ASSERT_TRUE(connections == correct_connections);
    }
}
//@-others

}
//@-others

}
//@+node:gcross.20110817224742.2470: *3* OperatorSpecification
TEST_SUITE(OperatorSpecification) {

//@+others
//@+node:gcross.20110817224742.2485: *4* compile
TEST_SUITE(compile) {
//@+others
//@+node:gcross.20110817224742.2472: *5* 1 site
TEST_SUITE(_1_site) {
//@+others
//@+node:gcross.20110817224742.2475: *6* bad left signal
TEST_CASE(bad_left_signal) {
    OperatorSpecification spec;
    spec.connect(0,10,spec.getEndSignal(),0);
    try {
        spec.compile();
    } catch(NeighborSignalConflict const& e) {
        EXPECT_EQ_VAL(e.right_site_number,0u)
        EXPECT_TRUE(boost::equal(e.left_site_right_signals,list_of(1u)))
        EXPECT_TRUE(boost::equal(e.right_site_left_signals,list_of(10u)))
    }
}
//@+node:gcross.20110817224742.2477: *6* bad right signal
TEST_CASE(bad_right_signal) {
    OperatorSpecification spec;
    spec.connect(0,spec.getStartSignal(),10,spec.getIMatrixId());
    try {
        spec.compile();
    } catch(NeighborSignalConflict const& e) {
        EXPECT_EQ_VAL(e.right_site_number,1u)
        EXPECT_TRUE(boost::equal(e.left_site_right_signals,list_of(10u)))
        EXPECT_TRUE(boost::equal(e.right_site_left_signals,list_of(2u)))
    }
}
//@+node:gcross.20110817224742.2474: *6* non-trivial link
TEST_CASE(nontrivial_link) {
    RNG random;

    REPEAT(100) {
        unsigned int dimension = random;
        MatrixPtr const matrix = random.randomSquareMatrix(dimension);
        OperatorSpecification spec;
        spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),spec.lookupIdOf(matrix));
        checkOperatorSitesEqual(
            *spec.compile()[0],
            constructOperatorSite(
                PhysicalDimension(dimension),
                LeftDimension(1),
                RightDimension(1),
                list_of(OperatorSiteLink(1,1,matrix))
            )
        );
    }
}
//@+node:gcross.20110817224742.2471: *6* trivial link
TEST_CASE(trivial_link) {
    OperatorSpecification spec;
    spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),spec.lookupIdOfIdentityWithDimension(2));
    checkOperatorSitesEqual(
        *spec.compile()[0],
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(1),
            RightDimension(1),
            list_of(OperatorSiteLink(1,1,I))
        )
    );
}
//@-others
}
//@+node:gcross.20110817224742.2478: *5* 2 sites
TEST_SUITE(_2_sites) {
//@+others
//@+node:gcross.20110817224742.2483: *6* bad middle signal
TEST_CASE(bad_middle_signal) {
    RNG random;

    REPEAT(100) {
        unsigned int signal1 = random, signal2 = signal1 + random;
        OperatorSpecification spec;
        spec.connect(0,spec.getStartSignal(),signal1,spec.getIMatrixId());
        spec.connect(1,signal2,spec.getEndSignal(),spec.getIMatrixId());
        try {
            spec.compile();
        } catch(NeighborSignalConflict const& e) {
            EXPECT_EQ_VAL(e.right_site_number,1u)
            EXPECT_TRUE(boost::equal(e.left_site_right_signals,list_of(signal1)))
            EXPECT_TRUE(boost::equal(e.right_site_left_signals,list_of(signal2)))
        }
    }
}
//@+node:gcross.20110817224742.2482: *6* non trivial signal links
TEST_CASE(non_trivial_single_links) {
    RNG random;

    REPEAT(100) {
        unsigned int dimension1 = random, dimension2 = random;
        MatrixPtr
            matrix1 = random.randomSquareMatrix(dimension1),
            matrix2 = random.randomSquareMatrix(dimension2);
        OperatorSpecification spec;
        spec.connect(0,spec.getStartSignal(),10,spec.lookupIdOf(matrix1));
        spec.connect(1,10,spec.getEndSignal(),spec.lookupIdOf(matrix2));
        Operator op = spec.compile();
        EXPECT_EQ_VAL(op.size(),2)
        checkOperatorSitesEqual(
            *op[0],
            constructOperatorSite(
                PhysicalDimension(dimension1),
                LeftDimension(1),
                RightDimension(1),
                list_of(OperatorSiteLink(1,1,matrix1))
            )
        );
        checkOperatorSitesEqual(
            *op[1],
            constructOperatorSite(
                PhysicalDimension(dimension2),
                LeftDimension(1),
                RightDimension(1),
                list_of(OperatorSiteLink(1,1,matrix2))
            )
        );
    }
}
//@+node:gcross.20110817224742.2484: *6* random links
TEST_CASE(random_links) {
    RNG random;

    REPEAT(100) {
        unsigned int const
            dimension1 = random,
            dimension2 = random,
            number_of_signals = random;
        vector<OperatorSiteLink> links1, links2;
        OperatorSpecification spec;
        BOOST_FOREACH(unsigned int current_signal, irange(1u,number_of_signals+1)) {
            MatrixPtr const
                matrix1 = random.randomSquareMatrix(dimension1),
                matrix2 = random.randomSquareMatrix(dimension2);

            links1.emplace_back(spec.getStartSignal(),current_signal,matrix1);
            spec.connect(0,spec.getStartSignal(),current_signal,spec.lookupIdOf(matrix1));

            links2.emplace_back(current_signal,1,matrix2);
            spec.connect(1,current_signal,spec.getEndSignal(),spec.lookupIdOf(matrix2));
        }
        Operator op = spec.compile();
        checkOperatorSitesEqual(
            *op[0],
            constructOperatorSite(
                PhysicalDimension(dimension1),
                LeftDimension(1),
                RightDimension(number_of_signals),
                links1
            )
        );
        checkOperatorSitesEqual(
            *op[1],
            constructOperatorSite(
                PhysicalDimension(dimension2),
                LeftDimension(number_of_signals),
                RightDimension(1),
                links2
            )
        );
    }
}
//@+node:gcross.20110817224742.2480: *6* trivial links
TEST_CASE(trivial_links) {
    OperatorSpecification spec;
    spec.connect(0,spec.getStartSignal(),10,spec.lookupIdOfIdentityWithDimension(2));
    spec.connect(1,10,spec.getEndSignal(),spec.lookupIdOfIdentityWithDimension(2));
    Operator op = spec.compile();
    EXPECT_EQ_VAL(op.size(),2)
    checkOperatorSitesEqual(
        *op[0],
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(1),
            RightDimension(1),
            list_of(OperatorSiteLink(1,1,I))
        )
    );
    checkOperatorSitesEqual(
        *op[1],
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(1),
            RightDimension(1),
            list_of(OperatorSiteLink(1,1,I))
        )
    );
    EXPECT_EQ(op[0],op[1]);
}
//@-others
}
//@-others
}
//@+node:gcross.20110817224742.2490: *4* eliminateDeadLeftSignals
TEST_SUITE(eliminateDeadLeftSignals) {
//@+others
//@+node:gcross.20110817224742.2491: *5* 1 site
TEST_CASE(_1_site) {
    RNG random;

    REPEAT(100) {
        vector<unsigned int> remaining_signals;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            spec.connect(0u,signal,spec.getEndSignal(),0);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),1u)
        EXPECT_EQ(spec.getConnections()[0].size(),number_of_signals)
        spec.eliminateDeadLeftSignals();
        ASSERT_EQ_VAL(spec.getConnections()[0].size(),1)
        ASSERT_EQ(spec.getConnections()[0].begin()->first.first,1)
        ASSERT_EQ(spec.getConnections()[0].begin()->first.second,2)
    }
}
//@+node:gcross.20110817224742.2497: *5* 2 sites
TEST_CASE(_2_sites) {
    RNG random;

    REPEAT(100) {
        vector<unsigned int> left_signals;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            spec.connect(1u,signal,spec.getEndSignal(),spec.getIMatrixId());
            if(signal == 1u || random.randomBoolean()) {
                spec.connect(0u,spec.getStartSignal(),signal,spec.getIMatrixId());
                left_signals.push_back(signal);
            }
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ(spec.getConnections()[0].size(),left_signals.size())
        EXPECT_EQ(spec.getConnections()[1].size(),number_of_signals)
        spec.eliminateDeadLeftSignals();
        EXPECT_EQ(spec.getConnections()[0].size(),left_signals.size())
        EXPECT_EQ(spec.getConnections()[1].size(),left_signals.size())
        ASSERT_TRUE(boost::equal(spec.getConnections()[1] | map_keys | map_keys,left_signals))
    }
}
//@-others
}
//@+node:gcross.20110817224742.2494: *4* eliminateDeadRightSignals
TEST_SUITE(eliminateDeadRightSignals) {
//@+others
//@+node:gcross.20110817224742.2495: *5* 1 site
TEST_CASE(_1_site) {
    RNG random;

    REPEAT(100) {
        vector<unsigned int> remaining_signals;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            spec.connect(0u,spec.getStartSignal(),signal,0);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),1u)
        EXPECT_EQ(spec.getConnections()[0].size(),number_of_signals)
        spec.eliminateDeadRightSignals();
        ASSERT_EQ_VAL(spec.getConnections()[0].size(),1)
        ASSERT_EQ(spec.getConnections()[0].begin()->first.first,1)
        ASSERT_EQ(spec.getConnections()[0].begin()->first.second,2)
    }
}
//@+node:gcross.20110817224742.2499: *5* 2 sites
TEST_CASE(_2_sites) {
    RNG random;

    REPEAT(100) {
        vector<unsigned int> right_signals;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            spec.connect(0u,spec.getStartSignal(),signal,spec.getIMatrixId());
            if(signal == 1u || random.randomBoolean()) {
                spec.connect(1u,signal,spec.getEndSignal(),spec.getIMatrixId());
                right_signals.push_back(signal);
            }
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ(spec.getConnections()[0].size(),number_of_signals)
        EXPECT_EQ(spec.getConnections()[1].size(),right_signals.size())
        spec.eliminateDeadRightSignals();
        EXPECT_EQ(spec.getConnections()[0].size(),right_signals.size())
        EXPECT_EQ(spec.getConnections()[1].size(),right_signals.size())
        ASSERT_TRUE(boost::equal(spec.getConnections()[1] | map_keys | map_keys,right_signals))
    }
}
//@-others
}
//@+node:gcross.20110817224742.2486: *4* eliminateNullMatrices
TEST_SUITE(eliminateNullMatrices) {
//@+others
//@+node:gcross.20110817224742.2487: *5* 1 site
TEST_CASE(_1_site) {
    TestingOperatorSpecification spec;
    spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),0);
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u)
    spec.eliminateNullMatrices();
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),0u);
    spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),spec.getIMatrixId());
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u);
    spec.eliminateNullMatrices();
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u);
}
//@+node:gcross.20110817224742.2488: *5* 2 sites
TEST_CASE(_2_sites) {
    RNG random;

    REPEAT(100) {
        vector<unsigned int> remaining_signals;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            unsigned int matrix_id;
            if(random.randomBoolean()) {
                matrix_id = 0;
            } else {
                remaining_signals.push_back(signal);
                matrix_id = spec.getIMatrixId();
            }
            spec.connect(0u,spec.getStartSignal(),signal,matrix_id);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),1u)
        EXPECT_EQ(spec.getConnections()[0].size(),number_of_signals)
        spec.eliminateNullMatrices();
        EXPECT_EQ(spec.getConnections()[0].size(),remaining_signals.size())
        ASSERT_TRUE(boost::equal(spec.getConnections()[0] | map_keys | map_values,remaining_signals))
    }
}
//@-others
}
//@+node:gcross.20110817224742.2500: *4* mergeLeftSignals
TEST_SUITE(mergeLeftSignals) {
//@+others
//@+node:gcross.20110818221240.2508: *5* mergable signal sets
TEST_CASE(mergable_signal_sets) {
    RNG random;

    REPEAT(100) {
        unsigned int const
            number_of_signals = random,
            dimension1 = random+1,
            dimension2 = random+1;
        TestingOperatorSpecification spec;
        unsigned int const
            matrix1_id1 = spec.lookupIdOf(random.randomSquareMatrix(dimension1)),
            matrix1_id2 = spec.lookupIdOf(random.randomSquareMatrix(dimension1));
        MatrixPtr const merged_matrix(new Matrix(dimension2,dimension2,c(0,0)));
        REPEAT(number_of_signals) {
            unsigned int const signal = spec.allocateSignal();
            MatrixPtr const matrix2 = random.randomSquareMatrix(dimension2);
            *merged_matrix += *matrix2;
            unsigned int const matrix2_id = spec.lookupIdOf(matrix2);
            spec.connect(0u,1u,signal,matrix1_id1);
            spec.connect(0u,2u,signal,matrix1_id2);
            spec.connect(1u,signal,spec.getEndSignal(),matrix2_id);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),2*number_of_signals)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),number_of_signals)
        OperatorSpecification old_spec = spec;
        spec.mergeLeftSignals();
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[0].begin()->first.first,1u)
        EXPECT_EQ_VAL((++spec.getConnections()[0].begin())->first.first,2u)
        EXPECT_EQ(
            spec.getConnections()[0].begin()->first.second,
            spec.getConnections()[1].begin()->first.first
        )
        EXPECT_EQ(
            (++spec.getConnections()[0].begin())->first.second,
            spec.getConnections()[1].begin()->first.first
        )
        EXPECT_EQ_VAL(spec.getConnections()[1].begin()->first.second,spec.getEndSignal())
        EXPECT_EQ(spec.getConnections()[0].begin()->second,matrix1_id1)
        EXPECT_EQ((++spec.getConnections()[0].begin())->second,matrix1_id2)
        EXPECT_EQ(spec.getConnections()[1].begin()->second,spec.lookupIdOf(merged_matrix))
    }
}
//@+node:gcross.20110818221240.2500: *5* mergable signals
TEST_CASE(mergable_signals) {
    RNG random;

    REPEAT(100) {
        unsigned int const
            number_of_signals = random,
            dimension1 = random+1,
            dimension2 = random+1;
        TestingOperatorSpecification spec;
        unsigned int const matrix1_id = spec.lookupIdOf(random.randomSquareMatrix(dimension1));
        MatrixPtr const merged_matrix(new Matrix(dimension2,dimension2,c(0,0)));
        REPEAT(number_of_signals) {
            unsigned int const signal = spec.allocateSignal();
            MatrixPtr const matrix2 = random.randomSquareMatrix(dimension2);
            *merged_matrix += *matrix2;
            unsigned int const matrix2_id = spec.lookupIdOf(matrix2);
            spec.connect(0u,spec.getStartSignal(),signal,matrix1_id);
            spec.connect(1u,signal,spec.getEndSignal(),matrix2_id);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),number_of_signals)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),number_of_signals)
        OperatorSpecification old_spec = spec;
        spec.mergeLeftSignals();
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[0].begin()->first.first,spec.getStartSignal())
        EXPECT_EQ(
            spec.getConnections()[0].begin()->first.second,
            spec.getConnections()[1].begin()->first.first
        )
        EXPECT_EQ_VAL(spec.getConnections()[1].begin()->first.second,spec.getEndSignal())
        EXPECT_EQ(spec.getConnections()[0].begin()->second,matrix1_id)
        EXPECT_EQ(spec.getConnections()[1].begin()->second,spec.lookupIdOf(merged_matrix))
        checkOperatorsEquivalent(old_spec.compile(),spec.compile(),random);
    }
}
//@+node:gcross.20110818221240.2512: *5* unmergable signal sets
TEST_CASE(unmergable_signal_sets) {
    RNG random;

    REPEAT(100) {
        unsigned int const dimension = random+1;
        unsigned int last_matrix_id = 0;
        TestingOperatorSpecification spec;
        unsigned int identical_matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
        unsigned int const number_of_signals = random+1;
        vector<unsigned int> left_signals;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            unsigned int random_matrix_id = 0;
            while(random_matrix_id <= last_matrix_id) {
                random_matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
            }
            last_matrix_id = random_matrix_id;
            spec.connect(0u,signal,1u,random_matrix_id);
            spec.connect(0u,signal,2u,identical_matrix_id);
            left_signals.push_back(signal);
        }
        vector<SiteConnections> old_connections = spec.getConnections();
        spec.mergeLeftSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
        spec.mergeRightSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
    }
}
//@+node:gcross.20110817224742.2502: *5* unmergable signals
TEST_CASE(unmergable_signals) {
    RNG random;

    REPEAT(100) {
        unsigned int const dimension = random+1;
        unsigned int last_matrix_id = 0;
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        vector<unsigned int> left_signals;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            unsigned int matrix_id = 0;
            while(matrix_id <= last_matrix_id) {
                matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
            }
            last_matrix_id = matrix_id;
            spec.connect(0u,signal,spec.getEndSignal(),matrix_id);
            left_signals.push_back(signal);
        }
        vector<SiteConnections> old_connections = spec.getConnections();
        spec.mergeLeftSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
        spec.mergeRightSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
    }
}
//@-others
}
//@+node:gcross.20110817224742.2513: *4* mergeRightSignals
TEST_SUITE(mergeRightSignals) {
//@+others
//@+node:gcross.20110818221240.2510: *5* mergable signal sets
TEST_CASE(mergable_signal_sets) {
    RNG random;

    REPEAT(100) {
        unsigned int const
            number_of_signals = random,
            dimension1 = random+1,
            dimension2 = random+1;
        TestingOperatorSpecification spec;
        unsigned int const
            matrix1_id1 = spec.lookupIdOf(random.randomSquareMatrix(dimension1)),
            matrix1_id2 = spec.lookupIdOf(random.randomSquareMatrix(dimension1));
        MatrixPtr const merged_matrix(new Matrix(dimension2,dimension2,c(0,0)));
        REPEAT(number_of_signals) {
            unsigned int const signal = spec.allocateSignal();
            MatrixPtr const matrix2 = random.randomSquareMatrix(dimension2);
            *merged_matrix += *matrix2;
            unsigned int const matrix2_id = spec.lookupIdOf(matrix2);
            spec.connect(1u,signal,1u,matrix1_id1);
            spec.connect(1u,signal,2u,matrix1_id2);
            spec.connect(0u,spec.getStartSignal(),signal,matrix2_id);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),2*number_of_signals)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),number_of_signals)
        OperatorSpecification old_spec = spec;
        spec.mergeRightSignals();
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[1].begin()->first.second,1u)
        EXPECT_EQ_VAL((++spec.getConnections()[1].begin())->first.second,2u)
        EXPECT_EQ(
            spec.getConnections()[1].begin()->first.first,
            spec.getConnections()[0].begin()->first.second
        )
        EXPECT_EQ(
            (++spec.getConnections()[1].begin())->first.first,
            spec.getConnections()[0].begin()->first.second
        )
        EXPECT_EQ_VAL(spec.getConnections()[0].begin()->first.first,spec.getStartSignal())
        EXPECT_EQ(spec.getConnections()[1].begin()->second,matrix1_id1)
        EXPECT_EQ((++spec.getConnections()[1].begin())->second,matrix1_id2)
        EXPECT_EQ(spec.getConnections()[0].begin()->second,spec.lookupIdOf(merged_matrix))
    }
}
//@+node:gcross.20110817224742.2515: *5* mergable signals
TEST_CASE(mergable_signals) {
    RNG random;

    REPEAT(100) {
        unsigned int const
            number_of_signals = random,
            dimension1 = random+1,
            dimension2 = random+1;
        TestingOperatorSpecification spec;
        unsigned int const matrix2_id = spec.lookupIdOf(random.randomSquareMatrix(dimension2));
        MatrixPtr const merged_matrix(new Matrix(dimension1,dimension1,c(0,0)));
        REPEAT(number_of_signals) {
            unsigned int const signal = spec.allocateSignal();
            MatrixPtr const matrix1 = random.randomSquareMatrix(dimension1);
            *merged_matrix += *matrix1;
            unsigned int const matrix1_id = spec.lookupIdOf(matrix1);
            spec.connect(0u,spec.getStartSignal(),signal,matrix1_id);
            spec.connect(1u,signal,spec.getEndSignal(),matrix2_id);
        }
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),number_of_signals)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),number_of_signals)
        OperatorSpecification old_spec = spec;
        spec.mergeRightSignals();
        EXPECT_EQ_VAL(spec.getConnections().size(),2u)
        EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[1].size(),1u)
        EXPECT_EQ_VAL(spec.getConnections()[0].begin()->first.first,spec.getStartSignal())
        EXPECT_EQ(
            spec.getConnections()[0].begin()->first.second,
            spec.getConnections()[1].begin()->first.first
        )
        EXPECT_EQ_VAL(spec.getConnections()[1].begin()->first.second,spec.getEndSignal())
        EXPECT_EQ(spec.getConnections()[0].begin()->second,spec.lookupIdOf(merged_matrix))
        EXPECT_EQ(spec.getConnections()[1].begin()->second,matrix2_id)
        checkOperatorsEquivalent(old_spec.compile(),spec.compile(),random);
    }
}
//@+node:gcross.20110818221240.2516: *5* unmergable signal sets
TEST_CASE(unmergable_signal_sets) {
    RNG random;

    REPEAT(100) {
        unsigned int const dimension = random+1;
        unsigned int last_matrix_id = 0;
        TestingOperatorSpecification spec;
        unsigned int identical_matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
        unsigned int const number_of_signals = random+1;
        vector<unsigned int> left_signals;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            unsigned int random_matrix_id = 0;
            while(random_matrix_id <= last_matrix_id) {
                random_matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
            }
            last_matrix_id = random_matrix_id;
            spec.connect(0u,1u,signal,random_matrix_id);
            spec.connect(0u,2u,signal,identical_matrix_id);
            left_signals.push_back(signal);
        }
        vector<SiteConnections> old_connections = spec.getConnections();
        spec.mergeRightSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
        spec.mergeLeftSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
    }
}
//@+node:gcross.20110817224742.2514: *5* unmergable signals
TEST_CASE(unmergable_signals) {
    RNG random;

    REPEAT(100) {
        unsigned int const dimension = random+1;
        unsigned int last_matrix_id = 0;        
        TestingOperatorSpecification spec;
        unsigned int const number_of_signals = random+1;
        vector<unsigned int> right_signals;
        BOOST_FOREACH(unsigned int signal, irange(1u,number_of_signals+1u)) {
            unsigned int matrix_id = 0;
            while(matrix_id <= last_matrix_id) {
                matrix_id = spec.lookupIdOf(random.randomSquareMatrix(dimension));
            }
            last_matrix_id = matrix_id;
            spec.connect(0u,spec.getStartSignal(),signal,matrix_id);
            right_signals.push_back(signal);
        }
        vector<SiteConnections> old_connections = spec.getConnections();
        spec.mergeRightSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
        spec.mergeLeftSignals();
        ASSERT_TRUE(spec.getConnections() == old_connections);
    }
}
//@-others
}
//@+node:gcross.20110818221240.2502: *4* optimize
TEST_SUITE(optimize) {
//@+others
//@+node:gcross.20110818221240.2503: *5* external field
TEST_CASE(external_field) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites,irange(2u,5u+1u)) {
        TestingOperatorSpecification spec;
        BOOST_FOREACH(unsigned int const active_site,irange(0u,number_of_sites)) {
            unsigned int middle_signal = spec.allocateSignal();
            BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
                unsigned int const
                    left_signal = site_number == 0u ? spec.getStartSignal() : middle_signal,
                    right_signal = site_number == (number_of_sites-1) ? spec.getEndSignal() : middle_signal,
                    matrix_id = site_number == active_site ? spec.getZMatrixId() : spec.getIMatrixId();
                spec.connect(site_number,left_signal,right_signal,matrix_id);
            }
        }
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            ASSERT_EQ(spec.getConnections()[site_number].size(),number_of_sites);
        }
        Operator correct_operator = constructExternalFieldOperator(number_of_sites,Pauli::Z);
        checkOperatorsEquivalent(spec.compile(),correct_operator,random);
        spec.optimize();
        checkOperatorsEquivalent(spec.compile(),correct_operator,random);
        ASSERT_EQ(spec.getConnections()[0u].size(),2u);
        BOOST_FOREACH(unsigned int const site_number,irange(1u,number_of_sites-1)) {
            ASSERT_EQ(spec.getConnections()[site_number].size(),3u);
        }
        ASSERT_EQ(spec.getConnections()[number_of_sites-1].size(),2u);
    }
}
//@+node:gcross.20110818221240.2519: *5* transverse ising
TEST_CASE(transverse_ising) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites,irange(2u,5u+1u)) {
        TestingOperatorSpecification spec;
        BOOST_FOREACH(unsigned int const active_site,irange(0u,number_of_sites)) {
            unsigned int middle_signal = spec.allocateSignal();
            BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
                unsigned int const
                    left_signal = site_number == 0u ? spec.getStartSignal() : middle_signal,
                    right_signal = site_number == (number_of_sites-1) ? spec.getEndSignal() : middle_signal,
                    matrix_id = site_number == active_site ? spec.getZMatrixId() : spec.getIMatrixId();
                spec.connect(site_number,left_signal,right_signal,matrix_id);
            }
        }
        BOOST_FOREACH(unsigned int const active_site,irange(0u,number_of_sites-1)) {
            unsigned int middle_signal = spec.allocateSignal();
            BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
                unsigned int const
                    left_signal = site_number == 0u ? spec.getStartSignal() : middle_signal,
                    right_signal = site_number == (number_of_sites-1) ? spec.getEndSignal() : middle_signal,
                    matrix_id = (site_number == active_site) || (site_number == active_site+1) ? spec.getXMatrixId() : spec.getIMatrixId();
                spec.connect(site_number,left_signal,right_signal,matrix_id);
            }
        }
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            ASSERT_EQ(spec.getConnections()[site_number].size(),2*number_of_sites-1);
        }
        Operator correct_operator = constructTransverseIsingModelOperator(number_of_sites,1);
        checkOperatorsEquivalent(spec.compile(),correct_operator,random);
        spec.optimize();
        checkOperatorsEquivalent(spec.compile(),correct_operator,random);
        ASSERT_EQ(spec.getConnections()[0u].size(),3u);
        if(number_of_sites > 2) {
            BOOST_FOREACH(unsigned int const site_number,irange(1u,number_of_sites-1)) {
                ASSERT_EQ(spec.getConnections()[site_number].size(),5u);
            }
        }
        ASSERT_EQ(spec.getConnections()[number_of_sites-1].size(),3u);
    }
}
//@-others
}
//@-others

}
//@+node:gcross.20110828143807.2598: *3* StateSpecification
TEST_SUITE(StateSpecification) {

//@+others
//@+node:gcross.20110828143807.2610: *4* compile
TEST_SUITE(compile) {
//@+others
//@+node:gcross.20110828143807.2611: *5* 1 site
TEST_SUITE(_1_site) {
//@+others
//@+node:gcross.20110828143807.2614: *6* non-trivial link
TEST_CASE(nontrivial_link) {
    RNG random;

    REPEAT(100) {
        unsigned int dimension = random;
        VectorConstPtr const vector = random.randomVector(dimension);
        StateSpecification spec;
        spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),spec.lookupIdOf(vector));
        State state = spec.compile();
        EXPECT_EQ_VAL(state.numberOfSites(),1u);
        StateSiteAny const& state_site = *state.begin();
        ASSERT_EQ_VAL(state_site.size(),vector->size());
        ASSERT_EQ_VAL(state_site.physicalDimension(),vector->size());
        ASSERT_EQ_VAL(state_site.leftDimension(),1u);
        ASSERT_EQ_VAL(state_site.rightDimension(),1u);
        complex<double> ratio = state_site[0]/(*vector)[0];
        BOOST_FOREACH(unsigned int index, irange<size_t>(1u,vector->size())) {
            ASSERT_NEAR_ABS(state_site[index]/(*vector)[index],ratio,1e-14);
        }
    }
}
//@+node:gcross.20110828143807.2615: *6* trivial link
TEST_CASE(trivial_link) {
    StateSpecification spec;
    spec.connect(0,spec.getStartSignal(),spec.getEndSignal(),spec.lookupIdOfObservation(0,1));
    State state = spec.compile();
    EXPECT_EQ_VAL(state.numberOfSites(),1u);
    StateSiteAny const& state_site = *state.begin();
    EXPECT_EQ_VAL(state_site.size(),1u);
    EXPECT_EQ_VAL(state_site.physicalDimension(),1u);
    EXPECT_EQ_VAL(state_site.leftDimension(),1u);
    EXPECT_EQ_VAL(state_site.rightDimension(),1u);
    EXPECT_TRUE(boost::equal(state_site,list_of(c(1,0))));
}
//@-others
}
//@+node:gcross.20110828143807.2631: *5* W state
TEST_SUITE(W_state) {

//@+others
//@+node:gcross.20110828143807.2632: *6* two sites
TEST_CASE(two_sites) {
    StateSpecification spec;
    spec.connect(0,1,1,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));
    spec.connect(0,1,2,spec.lookupIdOf(vectorFromRange(list_of(0)(-1))));
    spec.connect(1,2,2,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));
    spec.connect(1,1,2,spec.lookupIdOf(vectorFromRange(list_of(0)(1))));
    State state = spec.compile();
    EXPECT_NEAR_ABS_VAL(computeStateOverlap(state,state),c(1,0),1e-13);
    Vector actual_state_vector = computeStateVector(state);
    ASSERT_EQ_VAL(actual_state_vector.size(),4u);
    complex<double> correct_state_vector[] = {0,1/sqrt(2),-1/sqrt(2),0};
    BOOST_FOREACH(unsigned int const index, irange(0u,4u)) {
        EXPECT_NEAR_ABS(actual_state_vector[index],correct_state_vector[index],1e-13);
    }
}
//@+node:gcross.20110828143807.2633: *6* three sites
TEST_CASE(three_sites) {
    StateSpecification spec;
    spec.connect(0,1,1,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));
    spec.connect(0,1,2,spec.lookupIdOf(vectorFromRange(list_of(0)(-1))));

    spec.connect(1,1,1,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));
    spec.connect(1,1,2,spec.lookupIdOf(vectorFromRange(list_of(0)(1))));
    spec.connect(1,2,2,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));

    spec.connect(2,2,2,spec.lookupIdOf(vectorFromRange(list_of(1)(0))));
    spec.connect(2,1,2,spec.lookupIdOf(vectorFromRange(list_of(0)(2))));

    State state = spec.compile();
    EXPECT_NEAR_ABS_VAL(computeStateOverlap(state,state),c(1,0),1e-13);

    Vector actual_state_vector = computeStateVector(state);
    ASSERT_EQ_VAL(actual_state_vector.size(),8u);

    complex<double> correct_state_vector[] =
        {0  // 000
        ,2/sqrt(6)  // 001
        ,1/sqrt(6)  // 010
        ,0  // 011
        ,-1/sqrt(6) // 100
        ,0  // 101
        ,0  // 110
        ,0  // 111
        }
    ;

    BOOST_FOREACH(unsigned int const index, irange(0u,8u)) {
        EXPECT_NEAR_ABS(actual_state_vector[index],correct_state_vector[index],1e-13);
    }
}
//@-others

}
//@-others
}
//@-others

}
//@+node:gcross.20110814140556.2422: *3* SignalTable
TEST_SUITE(SignalTable) {

//@+others
//@+node:gcross.20110814140556.2423: *4* getStartSignal
TEST_CASE(getStartSignal) {
    EXPECT_EQ_VAL(SignalTable().getStartSignal(),1u);
}
//@+node:gcross.20110814140556.2425: *4* getEndSignal
TEST_CASE(getEndSignal) {
    EXPECT_EQ_VAL(SignalTable().getEndSignal(),2u);
}
//@+node:gcross.20110814140556.2426: *4* allocateSignal
TEST_CASE(allocateSignal) {
    SignalTable signal_table;
    EXPECT_EQ_VAL(signal_table.allocateSignal(),3u);
    EXPECT_EQ_VAL(signal_table.allocateSignal(),4u);
}
//@-others

}
//@-others

}
//@-others
//@-leo
