# Tokenizer for the untyped lambda calculus
# Transforms a string of text into a stream of tokens.
# Author : Tim Destan

class Token
  def initialize(name)
    @name = name
  end
  def to_s
    @name
  end
end

# Token for an identifier.
class IdentToken < Token
  attr_accessor :id
  def initialize(the_id)
    super(the_id)
    @id = the_id
  end
end

# Constant Tokens
class Tokens
  LAMBDA = Token.new("\\")
  DOT = Token.new(".")
  LPAREN = Token.new("(") 
  RPAREN = Token.new(")")
end

class LexError < Exception
  def initialize(*args)
    super(*args)
  end
end

class Tokenizer
  WHITESPACE = [' ', '\t', '\r', '\n']
  ALPHA_LOWER = ('a'..'z')
  ALPHA_UPPER = ('A' .. 'Z')

  # Single instance
  class << self
    private :new
  end

  def self.tokenize(str)
    raise LexError.new("Can't lex empty string.") if str.nil? or str.empty?
    tokens = []
    str.split(//).each do |chr|
      case chr
      when '.'
        tokens << Tokens::DOT
      when '\\'
        tokens << Tokens::LAMBDA
      when *WHITESPACE
        next
      when *ALPHA_LOWER
        tokens << IdentToken.new(chr)
      when *ALPHA_UPPER
        tokens << IdentToken.new(chr)
      when '('
        tokens << Tokens::LPAREN
      when ')'
        tokens << Tokens::RPAREN
      else
        raise LexError.new("Unexpected character #{chr}")
      end
    end
    return tokens
  end
end
