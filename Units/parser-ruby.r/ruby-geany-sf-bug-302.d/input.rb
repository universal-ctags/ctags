# https://sourceforge.net/p/geany/bugs/302/

class UtilitiesController
  def new
    File.open("somefile","r") do |infile|
      infile.readline
    end
    render :action => 'show'
  end

  def set_utilities_class
    @utilities_class = "current_menu"
  end
end
