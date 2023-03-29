output "password" {
  value = data.aws_ssm_parameter.example_database_password.value
}
