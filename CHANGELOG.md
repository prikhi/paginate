# CHANGELOG

## v6.0.2

Similar to the changes in v6.0.1, temporarily preserve the existing
responseData when updating the requestData using the updateData function. The
previous version only affected the updatePerPage function.

## v6.0.1

Temporarily preserve the existing responseData when changing the items per page
instead of clearing it out before fetching the new items. This helps prevent
view changes outside the scope of the Pagination element, e.g. when rendering
of the entire page depends on the responseData.

## v6.0.0

Update the package for Elm v0.19.0


## v5.1.0

Add a getRemoteData function to pull the raw WebData out of a Paginated value.

## v5.0.0

Support storing of additional response data. This allows you to save additional
information returned by the fetch command that is not pagination specific.

A getResponseData function allows you to grab this data from a Paginated value.


## v4.0.0

Flip arguments of the updateData & updatePerPage functions. This improves
composibility when passing a Paginated through pipe operators.


## v3.0.0

Add ability to change items per page.


## v2.1.0

* Add helper functions for rendering pagers, a general version and one specific
  for Bootstrap 4's classes.
* Add isFirst & isLast querying functions.

## v2.0.0

Swap order of jumpTo arguments


## v1.0.1

Fix ability to publish package.

## v1.0.0

Initial release
