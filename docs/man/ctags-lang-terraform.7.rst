.. _ctags-lang-terraform(7):

==============================================================
ctags-lang-terraform
==============================================================

Random notes about tagging Terraform files with Universal Ctags

:Version: 6.1.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Terraform ...
|	**ctags** ... --language-force=Terraform ...
|	**ctags** ... --map-Terraform=+.tf ...
|
|	**ctags** ... --extras=+{reference} --languages=+TerraformVariables ...
|	**ctags** ... --extras=+{reference} --language-force=TerraformVariables ...
|	**ctags** ... --extras=+{reference} --map-Terraform=+.tfvars ...

DESCRIPTION
-----------
This man page gathers random notes about tagging Terraform files.

TIPS
-----------

Extracting variables assigned in Variable definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Specify ``--extras=+{reference}`` and ``--languages=+TerraformVariables``
to extract variables assigned in variables definitions (`*.tfvars`).
The TerraformVariables parser extracts variables in `*.tfvars` files
with ``variable`` kind with ``assigned`` role of ``Terraform`` language.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New kind ``local``

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`,
`Configuration Syntax <https://developer.hashicorp.com/terraform/language/syntax/configuration>`_ (https://developer.hashicorp.com/terraform/language/syntax/configuration),
`Variable Definitions (.tfvars) Files <https://developer.hashicorp.com/terraform/language/values/variables#variable-definitions-tfvars-files>`_ (https://developer.hashicorp.com/terraform/language/values/variables#variable-definitions-tfvars-files)
