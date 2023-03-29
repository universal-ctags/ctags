resource "aws_cloudwatch_event_bus" "example_events_bus" {
  name = var.events_bus_name
}
