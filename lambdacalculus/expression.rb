# Expression Hierarchy for lambda calculus (untyped)
# 
# Expression (abstract)
# |_ IdentExp (a single identifier)
# |_ LambdaExp (a lambda expression with one parameter)
# |_ ApplicationExp (an application of the form (E1 E2))
# 
# Author: Tim Destan

require_relative('logging')

class RewriteError < Exception
  def initialize(*args)
    super(*args)
  end
end

# Abstract base class for the expressions that
# make up the parse tree for our lambda calculus.
class Expression
  def can_rewrite?
    false
  end

  def rewrite
    warn("Rewrite called on non-rewritable expression.")
    return self
  end
end

class IdentExp < Expression
  attr_accessor :identifier
  def initialize(x)
    @identifier = x
  end

  # Returns self unmodified unless we match the x,
  # in which case we return the argument value.
  def replace_all(x, arg_val)
    log(" #{self} -> Replace all (#{x}) -> (#{arg_val})")
    expr = (@identifier == x) ? arg_val : self
    log(" #{self} -> Returning #{expr}")
    return expr
  end

  # The variables of a single identifier is just itself.
  def vars
    return [@identifier]
  end

  def to_s
    @identifier
  end
end

class LambdaExp < Expression
  attr_accessor :identifier, :body
  def initialize(x,e)
    @identifier = x
    @body = e
  end
  
  # Can't rewrite a lambda expression alone so see
  # if we can rewrite the body.
  def can_rewrite?
    @body.can_rewrite?
  end

  def rewrite()
    LambdaExp.new(@identifier, @body.rewrite())
  end

  # Replaces all instances of this lambda expression's
  # formal parameter with the provided argument.
  def call(arg_val)
    @body.replace_all(@identifier, arg_val)
  end

  def vars
    @body.vars() - [@identifier]
  end

  # Don't believe this is quite working yet.
  def replace_all(x,arg_val)
    log(" #{self} -> Replace all (#{x}) -> (#{arg_val})")
    # If the x is the same as our identifier, then any
    # x's in our body are scoped to refer to our identifier,
    # not whichever one caused this replacement. We return
    # ourself unmodified.
    expr = self
    unless x == @identifier
      expr = LambdaExp.new(@identifier, @body.replace_all(x, arg_val))
    end
    log(" #{self} -> Returning #{expr}")
    return expr
  end

  # def replace_all(x, arg_val)
  #   myself = self
  #   # Check if we need to alpha-rename.
  #   my_vars = vars() + [@identifier]
  #   if my_vars.include?(x)
  #     log("Need to alpha-rename #{self} to rewrite #{x}...")
  #     Alphabet = Array("a".."z")
  #     candidate_index
  #     while my_vars.include? Alphabet[candidate_index]
  #       candidate_index += 1
  #       raise RewriteException.new("Exhausted the alphabet") if candidate_index >= Alphabet.length
  #     end
  #     myself = LambdaExp.new(
  #       (@identifier == x ? @identifier : Alphabet[candidate_index]),
  #       @body.replace_all(x, Alphabet[candidate_index]))
  #   end
  #   LambdaExp.new(myself.identifier, myself.body.replace_all(x,arg_val))
  # end

  def to_s
    "\\#{@identifier}.#{@body}"
  end
end

class ApplicationExp < Expression
  attr_accessor :e1, :e2
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def can_i_rewrite?
    @e1.is_a? LambdaExp
  end

  def can_rewrite?
    can_i_rewrite? or @e1.can_rewrite? or @e2.can_rewrite?
  end

  def vars
    [@e1.vars() + @e2.vars()].uniq
  end

  def rewrite()
    case
    when can_i_rewrite?
      @e1.call(@e2)
    when @e1.can_rewrite?
      ApplicationExp.new(@e1.rewrite(), @e2)
    when @e2.can_rewrite?
      ApplicationExp.new(@e1, @e2.rewrite())
    else
      warn("Rewrite called on non-rewritable expression.")
    end
  end

  def replace_all(x, arg_val)
    log(" #{self} -> Replace all (#{x}) -> (#{arg_val})")
    expr = ApplicationExp.new(
      @e1.replace_all(x, arg_val),
      @e2.replace_all(x, arg_val))
    log(" #{self} -> Returning #{expr}")
    return expr
  end

  def to_s
    "(#{@e1}) #{@e2}"
  end
end