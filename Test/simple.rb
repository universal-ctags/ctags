#!/usr/bin/ruby

module ModuleExample
    class ClassExample
        def class_method
            puts "in class_method"
        end
        def ClassExample.singleton_class_method
            puts "in singleton_class_method"
        end
        def class_method_exclamation!
            puts "in class_method_exclamation!"
        end
        def class_method_question?
            puts "in class_method_question?"
        end
    end
    def module_method
        puts "in module_method"
    end
    def ModuleExample.singleton_module_method
        puts "in singleton_module_method"
    end
end

ModuleExample::ClassExample.singleton_class_method
