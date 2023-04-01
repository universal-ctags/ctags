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
