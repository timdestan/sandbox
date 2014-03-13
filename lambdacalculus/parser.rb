# Parser for the untyped lambda calculus
# Transforms a stream of tokens into an Expression.
# Author : Tim Destan

require_relative 'lexer'
require_relative 'expression'

# E -> F E | F
# F -> x | \x.E | ( E )

class ParseError < Exception
  def initialize(*args)
    super(*args)
  end
end

# Create a clean parser for each parse (i.e. don't reuse)
class Parser

  def initialize()
    @idx = 0
    @tokens = nil
  end

  def check_not_off_edge()
    raise ParseError.new("Incomplete expression.") if @idx >= @tokens.length
  end

  def current_token()
    check_not_off_edge()
    return @tokens[@idx]
  end

  def next_token()
    @idx += 1
  end

  def expect(token)
    curr = current_token()
    raise ParseError.new("Expected #{token} but found #{curr}") unless curr == token
    next_token()
  end

  def parse_ident()
    curr = current_token()
    if curr.is_a? IdentToken
      next_token()
      return IdentExp.new(curr.id)
    else
      raise ParseError.new("Expected identifier but found #{curr}")
    end
  end

  def done?
    @idx == @tokens.length
  end

  def left_associate(expressions)
    if expressions.length > 1
      *most, last = expressions
      return ApplicationExp.new(left_associate(most), last)
    else
      return expressions[0]
    end
  end

  def next_starts_expression?()
    return false if done?
    curr = current_token()
    return (curr.is_a? IdentToken or
            curr == Tokens::LPAREN or
            curr == Tokens::LAMBDA)
  end

  def parse_expression()
    expressions = []
    expressions << parse_f()
    while next_starts_expression?
      expressions << parse_f()
    end
    return left_associate(expressions)
  end

  def parse_f()
    curr = current_token()
    return parse_ident() if curr.is_a? IdentToken
    case @tokens[@idx]
    when Tokens::LPAREN
      next_token()
      expr = parse_expression()
      expect(Tokens::RPAREN)
      return expr
    when Tokens::LAMBDA
      next_token()
      ident = parse_ident()
      expect(Tokens::DOT)
      expr = parse_expression()
      return LambdaExp.new(ident.identifier,expr)
    else
      raise ParseError.new("Unexpected #{curr}")
    end
  end

  def confirm_done()
    raise ParseError.new("Did not completely consume input string") unless done?
  end

  def parse_tokens(tokens)
    @idx = 0
    @tokens = tokens
    expr = parse_expression()
    confirm_done()
    return expr
  end

  def parse(str)
    raise ParseError.new("Cannot parse an empty string.") if str.nil? or str.empty?
    tokens = Tokenizer.tokenize(str)
    return parse_tokens(tokens)
  end
end