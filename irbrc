# -*- mode:ruby;coding:utf-8 -*-

require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 200
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"


if ENV["INSIDE_EMACS"] then
   puts "Inside Emacs we are.  Simple prompt we need."

   IRB.conf[:USE_MULTILINE] = nil
   IRB.conf[:USE_SINGLELINE] = false
   IRB.conf[:PROMPT_MODE] = :INF_RUBY

   IRB.conf[:USE_READLINE] = false 
   IRB.conf[:USE_COLORIZE] = true
end


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


ENV['PAGER']         = 'cat'
ENV['VLM_COMPILER']  = '/usr/bin/clang'
ENV['VLM_CFLAGS']    = '-target aarch64-linux-android -mcpu=cortex-a53 -Wno-gnu-variable-sized-type-not-at-end -Wno-initializer-overrides -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wno-implicit-function-declaration -H'
ENV['VLM_COVFLAGS']  = ''
ENV['VLM_COVERAGE']  = '/usr/bin/gcov'
ENV['VLM_TOOLCHAIN'] = '/usr/bin/'


####         ####
printf "Pascal, Welcome to the IRB!\n\n"
#### THE END ####

Dir.chdir "/build/pbourguignon/clang/work/build.devel/utest-nkernel"
Dir.pwd
load '/home/pbourguignon/works/harman/Ceedling/bin/ceedling.irb'
ceedling 'gcov:all'
