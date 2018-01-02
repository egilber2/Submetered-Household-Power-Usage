---
title: "Analysis of Submetered Household Electric Power Usage"
author: "EJG"
date: "January 2, 2018"
output: 
  html_document:
    keep_md: true
---






##Abstract

This project is analyzed from the perspective of a data scientist working for an "Internet of Things" analytics consulting firm whose client is a home developer.  The client would like to know if insights can be found in the sub-metered energy consumption data set that could be used as an incentive to potential home buyers that may be interested in "smart home" technology.  


##Methods
The 'household_power_consumption' data set is available at the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption).  It consists of time series data with 2,075,259 energy measurements and 9 variables.  The variable definitions are shown below.

<table class="table table-striped table-hover table-responsive table-bordered" style="font-size: 14px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Data Set Feature Definitions</caption>
 <thead><tr>
<th style="text-align:left;"> Feature </th>
   <th style="text-align:left;"> Definition </th>
   <th style="text-align:left;"> Sub-Meter-Coverage </th>
  </tr></thead>
<tbody>
<tr>
<td style="text-align:left;"> date </td>
   <td style="text-align:left;"> Date format dd/mm/yyyy </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> time </td>
   <td style="text-align:left;"> time format hh:mm:ss </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> global_active_power </td>
   <td style="text-align:left;"> household global minute-averaged active power (kilowatt) </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> global_reactive_power </td>
   <td style="text-align:left;"> household global minute-averaged reactive power (kilowatt) </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> voltage </td>
   <td style="text-align:left;"> minute-averaged voltage (volt) </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> global_intensity </td>
   <td style="text-align:left;"> household global minute-averaged current intensity (ampere) </td>
   <td style="text-align:left;">  </td>
  </tr>
<tr>
<td style="text-align:left;"> sub_metering_1 </td>
   <td style="text-align:left;"> energy sub-metering No. 1 (watt-hour of active energy) </td>
   <td style="text-align:left;"> kitchen (dishwasher, oven, microwave) </td>
  </tr>
<tr>
<td style="text-align:left;"> sub_metering_2 </td>
   <td style="text-align:left;"> energy sub-metering No. 2 (watt-hour of active energy) </td>
   <td style="text-align:left;"> laundry room (washing machine,  drier, light, refrigerator) </td>
  </tr>
<tr>
<td style="text-align:left;"> sub_metering_3 </td>
   <td style="text-align:left;"> energy sub-metering No. 3 (watt-hour of active energy) </td>
   <td style="text-align:left;"> electric water heater and air conditioner </td>
  </tr>
</tbody>
</table>

