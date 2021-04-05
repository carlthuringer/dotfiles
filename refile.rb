require 'date'
year = "2020"

org_files = Dir["#{year}-*-*.org"].sort

output = File.open("#{year}.org", "w")

org_files.each do |file|
  pp file
  File.open(file) do |f|
    date_str = f.path.match(/(\d+-\d+-\d+)/)[0]
    date = Date.parse(date_str)
    output << "* #{date.strftime("%A, %Y-%m-%d")}\n"
    output << ":PROPERTIES:\n:CREATED: #{date.strftime('%Y%m%d')}\n:END:\n"
    f.each do |line|
      case line
      when /^#/
        next
      when /^\*+/
        output << "*#{line}"
      else
        output << line
      end
    end
  end
end
  
output.close
  
# Let's read the org file, grab the date from the title line, then write a headline for that date.

# Then for all other lines, if there's a headline star, add one, otherwise print the line as-is.
