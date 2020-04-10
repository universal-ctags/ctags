class X
  @rr0 = 0
  @rr1 = 0
  @rr2 = 0
  @rr3 = 0
  @rr4 = 0
  @rr5 = 0
  @rr6 = 0
  @rr7 = 0
  @rr8 = 0
  @rr9 = 0
  @rr10 = 0
  @rr11 = 0                    
  attr_reader :rr0
  attr_reader(:rr1)
  attr_reader "rr2"
  attr_reader :rr3, :rr4
  attr_reader "rr5", "rr6"
  attr_reader :rr7, "rr8"
  attr_reader "rr9", :rr10
  attr_reader("rr11")

  @rr12 = 0
  @rr13 = 0
  @rr14 = 0
  @rr15 = 0
  @rr16 = 0
  @rr17 = 0
  @rr18 = 0
  @rr19 = 0
  @rr20 = 0
  @rr21 = 0
  @rr22 = 0
  @rr23 = 0
  attr_reader 'rr12'
  attr_reader 'rr13', 'rr14'
  attr_reader :rr15, 'rr16'
  attr_reader 'rr17', :rr18
  attr_reader('rr19')
  attr_reader 'rr20', "rr21"
  attr_reader "rr22", 'rr23'

  @ww0 = 0
  @ww1 = 0
  @ww2 = 0
  @ww3 = 0
  @ww4 = 0
  @ww5 = 0
  @ww6 = 0
  @ww7 = 0    
  @ww8 = 0
  @ww9 = 0
  @ww10 = 0
  @ww11 = 0      
  attr_writer :ww0
  attr_writer (:ww1)
  attr_writer "ww2"
  attr_writer :ww3, :ww4
  attr_writer "ww5", "ww6"
  attr_writer :ww7, "ww8"
  attr_writer "ww9", :ww10
  attr_writer ("ww11")
  @rw0 = 0
  @rw1 = 0
  @rw2 = 0
  @rw3 = 0
  @rw4 = 0
  @rw5 = 0
  @rw6 = 0
  @rw7 = 0    
  @rw8 = 0
  @rw9 = 0
  @rw10 = 0
  @rw11 = 0        
  attr_accessor :rw0
  attr_accessor (  :rw1)
  attr_accessor "rw2"
  attr_accessor :rw3, :rw4
  attr_accessor "rw5", "rw6"
  attr_accessor :rw7, "rw8"
  attr_accessor "rw9", :rw10
  attr_accessor (  "rw11")
end
