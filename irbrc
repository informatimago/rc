# -*- mode:ruby -*-

# require 'irb/ext/save-history'
# IRB.conf[:SAVE_HISTORY] = 100
# IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"


def ri(*args) 
  doc=IO.popen("ri "+args.join(" "),"w+");
  doc.each_line{|line| puts line;};
end


def netstat(*args)
  if args.empty? then
    puts `netstat -tnpl`;
  else
    puts system("netstat "+args.join(" "));
  end
end


# class Class
#   def _superclasses 
#     if superclass 
#       superclass._superclasses<<self
#     else
#       [self];
#     end
#   end
#   def superclasses ; _superclasses.reverse ; end
# end

    

# class Array;def unshiftArray(a);a.reverse.each{|e|self.unshift(e);};self;end;end
# $:.unshiftArray(Dir.glob("/usr/lib/ruby/gems/1.8/gems/*/{cli,lib}"));

# require 'ruby-debug'




# IRB.conf[:IRB_RC] = proc do |conf|
#   leader = " " * conf.irb_name.length
#   conf.prompt_i = "#{conf.irb_name} --> "
#   conf.prompt_s = leader + ' |-" '
#   conf.prompt_c = leader + ' +-> '
#   conf.return_format = leader + " ==> %s\n\n"
#   puts "Welcome!"
# end



####         ####
printf "Pascal, Welcome to the IRB!\n\n"
#### THE END ####
