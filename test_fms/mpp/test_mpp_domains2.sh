#!/bin/sh

# This is part of the GFDL FMS package. This is a shell script to
# execute tests in the test_fms/mpp directory.

# Ed Hartnett 11/29/19

# Set common test settings.
. ../test_common.sh

if [ "x$(uname -s)" = "xDarwin" ]
then
    is_darwin='skip'
elif [ "x$TRAVIS" = "xtrue" ]
then
    is_travis='skip'
fi

#echo "1: Test update nest domain"
#sed "s/test_nest_domain = .false./test_nest_domain = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 2 skip

#echo "2:  Test Subset Update"
#sed "s/test_subset = .false./test_subset = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 2 skip

echo "3: Test Halosize Performance"
sed "s/test_halosize_performance = .false./test_halosize_performance = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails 
run_test test_mpp_domains 2 $is_darwin 

#echo "4: Test Edge Update"
#sed "s/test_edge_update = .false./test_edge_update = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 2 skip

#echo "5: Test Nonsym Edge"
#sed "s/test_nonsym_edge = .false./test_nonsym_edge = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 2 skip

echo "6: Test Performance"
sed "s/test_performance = .false./test_performance = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin or TRAVIS it will be skipped because it fails 
run_test test_mpp_domains 6 $is_darwin $is_travis

echo "7: Test Global Sum"
sed "s/test_global_sum = .false./test_global_sum = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails 
run_test test_mpp_domains 2 $is_dawin 

echo "8: Test Cubic Grid Redistribute"
sed "s/test_cubic_grid_redistribute = .false./test_cubic_grid_redistribute = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin or TRAVIS it will be skipped because it fails
run_test test_mpp_domains 6 $is_darwin $is_travis

echo "9: Test Boundary"
sed "s/test_boundary = .false./test_boundary = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin or TRAVIS it will be skipped because it fails
run_test test_mpp_domains 6 $is_darwin $is_travis

echo "10: Test Adjoint"
sed "s/test_adjoint = .false./test_adjoint = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails
run_test test_mpp_domains 2 $is_darwin

#echo "11: Test Unstruct"
#sed "s/test_unstruct = .false./test_unstruct = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 2 skip

echo "12: Test Group"
sed "s/test_group = .false./test_group = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails
run_test test_mpp_domains 2 $is_darwin

echo "13: Test Interface"
sed "s/test_interface = .false./test_interface = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails
run_test test_mpp_domains 2 $is_darwin

#echo "14: Test Check Parallel"
#echo "Does not work on Darwin or elsewhere"
#sed "s/check_parallel = .false./check_parallel = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
run_test test_mpp_domains 6 skip

echo "15: Test Get Nbr"
sed "s/test_get_nbr = .false./test_get_nbr = .true./" $top_srcdir/test_fms/mpp/input_base.nml > input.nml
#If the system is Darwin it will be skipped because it fails
run_test test_mpp_domains 2 $is_darwin
