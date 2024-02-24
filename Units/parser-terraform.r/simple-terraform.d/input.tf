provider "aws" {
  region = "us-east-1"
}

variable "events_bus_name" {
  type = string
  default = "hello-world"
}

# variable "dont_extract_me0" {
// variable "dont_extract_me1" {

resource "aws_cloudwatch_event_bus" "example_events_bus" {
  name = var.events_bus_name
}

module "database" {
  source = "../../modules/database"
}

data "aws_ssm_parameter" "example_database_password" {
  name = "example-database-password"
}

output "password" {
  value = data.aws_ssm_parameter.example_database_password.value
}

locals {
  this_is_a_local_variable_1 = "this is the value of local variable 1"
  this_is_a_local_variable_2 = var.events_bus_name == local.this_is_a_local_variable_1 ## var.events_bus_name == shouldn't be picked up as a local

  /* THE DEFINITIONS IN THIS BLOCK SHOULDN'T BE PICKED UP
variable "no_parse_super_aws_thing" {
  type = string
  default = "hello-world"
}
resource "aws_cloudwatch_event_bus" "no_parse_example_events_bus_1" {
  name = var.events_bus_name
}

module "no_parse_database_1" {
  source = "../../modules/database"
}

data "aws_ssm_parameter" "no_parse_example_database_password_1" {
  name = "example-database-password"
}

output "no_parse_password_1" {
  value = data.aws_ssm_parameter.example_database_password.value
}

locals {
  no_parse_this_is_a_local_variable_1 = "this is the value of local variable 1"

}
*/
}
