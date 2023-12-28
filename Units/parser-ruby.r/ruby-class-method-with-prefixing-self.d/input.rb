# Derrived from gitlab/app/models/integration.rb
class Integration < ApplicationRecord
  UnknownType = Class.new(StandardError)
  self.inheritance_column = :type_new

  def self.field(name, storage: field_storage, **attrs)
    fields << ::Integrations::Field.new(name: name, integration_class: self, **attrs)

    case storage
    when :attribute
      # noop
    when :properties
      prop_accessor(name)
    when :data_fields
      data_field(name)
    else
      raise ArgumentError, "Unknown field storage: #{storage}"
    end

    boolean_accessor(name) if attrs[:type] == 'checkbox' && storage != :attribute
  end

  def UnknownType.f()
  end
end
