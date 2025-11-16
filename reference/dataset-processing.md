# Datasets processing

This manual page describes manipulation methods using
[list](https://rdrr.io/r/base/list.html) objects. In index or name `i`
can be specified to define the array (by name of index) on which to
operate.

The following functions are currently available:

- `keepDatasets(object, range)` keep datasets in object which are in
  range

- `addDatasets(object, dataset, name)` add the 'dataset' to the object
  (of type list)

- `Save(object, file)` stores the object to a .RData file

This function appends a dataset in the list with customization if
necessary

This function deletes the items not included in the range parameter

## Usage

``` r
addDatasets(object, dataset, name)

keepDatasets(object, range = seq(length(object)))
```

## Arguments

- object:

  An instance of type list. Must get TRUE to inherits(object, 'list')

- dataset:

  An instance of class `SummarizedExperiment`

- name:

  the name to associate to the dataset in the final object

- range:

  xxx

## Value

An processed object of the same class as `object`.

## Details

The object must be of type list. Thetwo functions are implemented here
for of the package which uses MagellanNTK
