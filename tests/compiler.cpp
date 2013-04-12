#include "nutcracker/compiler.hpp"
#include "nutcracker/utilities.hpp"

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

class TestingOperatorSpecification : public OperatorSpecification {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(TestingOperatorSpecification)
public:
    TestingOperatorSpecification() {}
    TestingOperatorSpecification(BOOST_RV_REF(OperatorSpecification) other) : OperatorSpecification(other) {}
    vector<SiteConnections> const& getConnections() { return connections; }
};
TEST_SUITE(Compiler) {

TEST_SUITE(DataTable_specializations) {

TEST_SUITE(MatrixTable) {

TEST_SUITE(initial_matrices) {

TEST_CASE(Null) {
    EXPECT_EQ_VAL(MatrixTable().lookupIdOf(make_shared<Matrix>(2,2,c(0,0))),0);
}
TEST_SUITE(I) {
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(I),matrix_table.getIMatrixId());
}
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getIMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),I->data()))
}
}
TEST_SUITE(X) {
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(X),matrix_table.getXMatrixId());
}
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getXMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),X->data()))
}
}
TEST_SUITE(Y) {
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(Y),matrix_table.getYMatrixId());
}
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getYMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),Y->data()))
}
}
TEST_SUITE(Z) {
TEST_CASE(correct_add) {
    MatrixTable matrix_table;
    EXPECT_EQ_VAL(matrix_table.lookupIdOf(Z),matrix_table.getZMatrixId());
}
TEST_CASE(correct_lookup) {
    MatrixTable matrix_table;
    Matrix const& matrix = *matrix_table.get(matrix_table.getZMatrixId());

    EXPECT_EQ_VAL(matrix.size1(),2)
    EXPECT_TRUE(boost::equal(matrix.data(),Z->data()))
}
}

}
TEST_SUITE(lookupIdOf) {

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

}
TEST_SUITE(lookupIdOfIdentityWithDimension) {

TEST_CASE(returns_I_for_dimension_2) {
    MatrixTable matrix_table;
    EXPECT_EQ(matrix_table.lookupIdOfIdentityWithDimension(2),matrix_table.getIMatrixId())
}

}

}
TEST_SUITE(VectorTable) {

TEST_SUITE(lookupIdOf) {

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

}
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
TEST_CASE(null) {
    EXPECT_EQ_VAL(VectorTable().lookupIdOfRange(vector<complex<double> >()),0);
}

}

}
TEST_SUITE(OperatorBuilder) {

TEST_SUITE(addProductTerm) {
TEST_CASE(increasing_diagonals) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            vector<MatrixConstPtr> components;
            REPEAT(site_number) { components.push_back(Pauli::I); }
            components.emplace_back(diagonalMatrix(list_of(0u)(site_number)));
            REPEAT(number_of_sites-site_number-1) { components.push_back(Pauli::I); }
            builder.addProductTerm(components);
        }
        Operator op = builder.compile();
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            StateBuilder builder(number_of_sites,PhysicalDimension(2u));
            vector<VectorConstPtr> components;
            REPEAT(site_number) { components.push_back(Qubit::Up); }
            components.push_back(Qubit::Down);
            REPEAT(number_of_sites-site_number-1) { components.push_back(Qubit::Up); }
            builder.addProductTerm(components);
            State state = builder.compile();
            ASSERT_NEAR_ABS(computeExpectationValue(state,op),c(site_number,0),1e-12);
        }
    }
}
TEST_CASE(magnetic_field) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            vector<MatrixConstPtr> components;
            REPEAT(site_number) { components.push_back(Pauli::I); }
            components.push_back(Pauli::Z);
            REPEAT(number_of_sites-site_number-1) { components.push_back(Pauli::I); }
            builder.addProductTerm(components);
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
}
TEST_SUITE(generateSpecification) {

TEST_CASE(single_connections) {
    OperatorBuilder builder(1,PhysicalDimension(2));
    builder.connect(0u,0u,0u,Pauli::I);
    builder.connect(0u,0u,1u,Pauli::X);
    builder.connect(0u,0u,2u,Pauli::Y);
    builder.connect(0u,0u,3u,Pauli::Z);
    TestingOperatorSpecification spec(builder.generateSpecification(false));
    SiteConnections correct_connections;
    correct_connections[make_pair(0u,0u)] = spec.getIMatrixId();
    correct_connections[make_pair(0u,1u)] = spec.getXMatrixId();
    correct_connections[make_pair(0u,2u)] = spec.getYMatrixId();
    correct_connections[make_pair(0u,3u)] = spec.getZMatrixId();
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),4u);
    EXPECT_TRUE(spec.getConnections()[0] == correct_connections);
}
TEST_CASE(multiconnections) {
    OperatorBuilder builder(1,PhysicalDimension(2));
    builder.connect(0u,0u,0u,Pauli::I);
    builder.connect(0u,0u,0u,Pauli::X);
    builder.connect(0u,0u,0u,Pauli::Y);
    builder.connect(0u,0u,0u,Pauli::Z);
    TestingOperatorSpecification spec(builder.generateSpecification(false));
    SiteConnections correct_connections;
    MatrixConstPtr matrix = squareMatrix(list_of(c(2,0))(c(1,-1))(c(1,1))(c(0,0)));
    correct_connections[make_pair(0u,0u)] = spec.lookupIdOf(matrix);
    EXPECT_EQ_VAL(spec.getConnections()[0].size(),1u);
    EXPECT_TRUE(spec.getConnections()[0] == correct_connections);
}
TEST_CASE(standard_loops) {
    OperatorBuilder builder(5,PhysicalDimension(2));
    TestingOperatorSpecification spec(builder.generateSpecification());
    SiteConnections correct_connections;
    correct_connections[make_pair(spec.getStartSignal(),spec.getStartSignal())] = spec.getIMatrixId();
    correct_connections[make_pair(spec.getEndSignal(),spec.getEndSignal())] = spec.getIMatrixId();
    BOOST_FOREACH(SiteConnections const& connections, spec.getConnections()) {
        ASSERT_TRUE(connections == correct_connections);
    }
}

}

}
TEST_SUITE(Operator_terms) {

TEST_CASE(GlobalExternalField) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(1u,5u)) {
        GlobalExternalField field_1(Pauli::Z), field_2 = field_1 * 0.5;
        checkOperatorsEqual(
            OperatorBuilder(number_of_sites,PhysicalDimension(2u))
                .addTerm(field_1)
                .compile()
            ,
            constructExternalFieldOperator(number_of_sites,Pauli::Z)
        );
        checkOperatorsEqual(
            OperatorBuilder(number_of_sites,PhysicalDimension(2u))
                .addTerm(field_2)
                .compile()
            ,
            constructExternalFieldOperator(number_of_sites,0.5*Pauli::Z)
        );
    }
}
TEST_CASE(GlobalNeighborCouplingField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        Operator op =
            OperatorBuilder(number_of_sites,PhysicalDimension(2u))
                .addTerm(GlobalExternalField(Pauli::Z))
                .addTerm(GlobalNeighborCouplingField(Pauli::X,Pauli::X)*0.5)
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
TEST_CASE(LocalExternalField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(1u,5u)) {
        complex<double> coefficient = random.randomComplexDouble();
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
            builder += LocalExternalField(site_number,Pauli::Z) * coefficient;
        }
        checkOperatorsEqual(
            builder.compile(),
            constructExternalFieldOperator(number_of_sites,Pauli::Z * coefficient)
        );
    }
}
TEST_CASE(LocalNeighborCouplingField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2u));
        BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
            builder += LocalExternalField(site_number,Pauli::Z);
            if(site_number+1 < number_of_sites) {
                builder += LocalNeighborCouplingField(site_number,Pauli::X,Pauli::X)*0.5;
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
TEST_CASE(TransverseIsingField) {
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        Operator op =
            OperatorBuilder(number_of_sites,PhysicalDimension(2u))
                .addTerm(TransverseIsingField(Pauli::Z,0.5*Pauli::X,Pauli::X))
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

}
TEST_SUITE(OperatorSpecification) {

TEST_SUITE(compile) {
TEST_SUITE(_1_site) {
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
}
TEST_SUITE(_2_sites) {
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
}
}
TEST_SUITE(eliminateDeadLeftSignals) {
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
}
TEST_SUITE(eliminateDeadRightSignals) {
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
}
TEST_SUITE(eliminateNullMatrices) {
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
}
TEST_SUITE(mergeLeftSignals) {
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
}
TEST_SUITE(mergeRightSignals) {
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
}
TEST_SUITE(optimize) {
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
}

}
TEST_SUITE(StateSpecification) {

TEST_SUITE(compile) {
TEST_SUITE(_1_site) {
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
}
TEST_SUITE(W_state) {

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

}
}

}
TEST_SUITE(SignalTable) {

TEST_CASE(getStartSignal) {
    EXPECT_EQ_VAL(SignalTable().getStartSignal(),1u);
}
TEST_CASE(getEndSignal) {
    EXPECT_EQ_VAL(SignalTable().getEndSignal(),2u);
}
TEST_CASE(allocateSignal) {
    SignalTable signal_table;
    EXPECT_EQ_VAL(signal_table.allocateSignal(),3u);
    EXPECT_EQ_VAL(signal_table.allocateSignal(),4u);
}

}

}
