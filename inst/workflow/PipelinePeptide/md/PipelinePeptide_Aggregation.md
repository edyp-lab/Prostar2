---
title: "Aggreagtion"
abstract: >
  This page describes the computations done with Aggregation.

output:
    BiocStyle::html_document:
        highlight: tango
        thme: sandstone
        toc: false
        number_sections: TRUE
        css: style.css
date: 30 mai 2024
link-citations: true
---


This page describes the computations done with Aggregation. 



### Methods
**_Please refer to the table below for warnings about the different aggregation methods available._**

#### Handling of shared peptides
Multiple methods are available to handle shared peptides : 
* No shared peptide
* Shared as specific
* Simple redistribution of shared peptides
* Iterative redistribution of shared peptides


#### Selecting peptides
* All
* TopN


#### Functions
5 functions are available :
* Sum
* Mean
* Median
* medianPolish
* robustSummary

<style>
.tabledescagg table, 
.tabledescagg th, 
.tabledescagg td {
  border:1px solid black;
 border-collapse: collapse;
}
.tabledescagg th, 
.tabledescagg td {
  padding: 10px;
}
.tabledescagg th {
  font-weight: bold;
}
</style>
<table class="tabledescagg">
  <tr>
    <th> </th>
    <th>No shared peptide</th>
    <th>Shared as specific</th>
    <th>Simple redistribution of shared peptides</th>
    <th>Iterative redistribution of shared peptides</th>
  </tr>
  <tr>
    <td><b>All peptides</b></td>
    <td>txt 11</td>
    <td>txt 21</td>
    <td>txt 31</td>
    <td>txt 41</td>
  </tr>
  <tr>
    <td><b>N most abundant</b></td>
    <td>txt 12</td>
    <td>txt 22</td>
    <td>txt 32</td>
    <td>txt 42</td>
  </tr>
</table>
