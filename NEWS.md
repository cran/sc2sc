## Package changes from previous sc2sc version 0.0.1-16

This version corrects a bug introduced in the previous version of the package when adding the argument na.rm and more than a variable is transferred.

## Package changes from previous sc2sc version 0.0.1-14

This version corrects a bug in the sc2cp and cp2sc functions when transferring counts, due to a line of code misplaced. We wish to thank PhD Priscila Espinosa for warning us about this fact and suggesting extensions and possibilities for improving the package. In general, transfers of counts performed with the sc2cp and cp2sc functions, specially with sc2cp, obtained using version 0.0.1-14 are not reliable.

The functions include now an additional argument: `na.rm`, which controls how missing values should be handled.

## Package changes from previous sc2sc version 0.0.1-12

For transferring data from postal code areas to census sections, weights are now computed using population densities. See <doi:10.4995/CARMA2024.2024.17796> for details.

### Package changes from previous sc2sc version 0.0.1-9

This version solves a bug in the internal computation of weights to transfer data in expansion subareas of cities as well as another, likely more relevant, bug impacting on the transferring of counts. This bug was inadvertently introduced when modifying the code to accommodate the new, smaller objects containing the info to make the transfers. In general, transfers of counts performed using version 0.0.1-9 are not reliable.


### Package changes from previous sc2sc version 0.0.1-7

This new version extends to year 2023 the horizon for making transfers. 

The sizes of the objects which contain the info to make the transfers has been reduced, and the code has been redone to avoid losses in computation velocity.

It has been solved some inconsistencies detected in the correspondences between the census sections and the postal codes partitions.
