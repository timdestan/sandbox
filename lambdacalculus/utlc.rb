# Interpreter for the untyped lambda calculus
#
# Rewrites an expression until a fixed point is reached.
# Provides a shell-like interface. Relatively minimal.
# Type "exit" to exit.
#
# Author: Tim Destan

require_relative 'logging'
require_relative 'expression'
require_relative 'parser'

# Contains the interpreter for the untyped Lambda Calculus.
# Rewrite logic is contained in expression.rb

if __FILE__ == $0
  # main
  loop do
    print ">> "
    line = STDIN.readline.chomp
    break if line == "exit"
    begin
      expr = Parser.new().parse(line)
      while expr.can_rewrite?
        old, expr = expr, expr.rewrite()
        puts "#{old}  ==>  #{expr}\n"
      end
      puts "Done evaluating #{expr}\n"
    rescue RewriteError => e
      puts "Evaluation error triggered: #{e.stacktrace}"
    rescue ParseError => e
      puts "Parse error triggered: #{e.stacktrace}"
    rescue LexError => e
      puts "Lexing error triggered: #{e.stacktrace}"
    end
  end
  puts "bye."
end