LSIF Indexer for Ada
--------------------


Command line switches
---------------------

The LSIF Indexer supports the following command line switches:

 * -P/--project  Path to project files
 * -X            Scenario variable


Project file attributes
-----------------------

The LSIF Indexer uses the following attributes specified in project files:

 1. LSIF.Workspace_Root  Root directory of the workspace

When processing project files, the LSIF Indexer sets the value of the GPR_TOOL
scenario variable to "lsif-ada" unless another value is specified in the
command line or environment variable.
