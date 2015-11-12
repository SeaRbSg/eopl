# An envirnoment is a list of bindings (hashes)
# e.g. [{x: 1, y:2}, {y:3}]
class Env
  attr_reader :env
  def initialize env
    @env = env
  end

  def has_binding? var
    env.any? { |h| h.has_key? var }
  end

  def apply var
    env.reverse.each do |h|
      val = h[var]
      unless val.nil?
        return val
      end
    end
    nil
  end

  def extend vars, vals
    env << Hash[vars.zip vals]
  end
end

class LambdaCalculusExp
  def lambda_exp?
    self.class.name == "LambdaExp"
  end

  def var_exp?
    self.class.name == "VarExp"
  end

  def app_exp?
    self.class.name == "AppExp"
  end
end

class LambdaExp < LambdaCalculusExp
  attr_reader :arg, :body
  def initialize arg, body
    @arg = arg
    @body = body
  end

  def == other
    self.arg == other.arg && self.body == other.body
  end
end

class VarExp < LambdaCalculusExp
  attr_reader :value
  def initialize value
    @value = value
  end

  def == other
    self.value == other.value
  end
end

class AppExp < LambdaCalculusExp
  attr_reader :operator, :operand
  def initialize operator, operand
    @operator = operator
    @operand = operand
  end

  def == other
    self.operator == other.operator && self.operand == other.operand
  end
end

def occurs_free? var, exp
  if exp.var_exp?
    var == exp.value
  elsif exp.lambda_exp?
    !(var == exp.arg) && occurs_free?(var, exp.body)
  elsif exp.app_exp?
    occurs_free?(var, exp.operator) || occurs_free?(var, exp.operand)
  end
end

# Ex:
# "(lambda (x) x)" => ["(", "lambda", "(", "x", ")", "x", ")"]
def string_to_tokens s
   s.gsub(/[()]/, ' \0 ').split(" ")
end 

def tokens_to_tree tokens
  token = tokens.shift
  if token == "("
    l = []
    until tokens[0] == ")"
      l << tokens_to_tree(tokens)
    end
    tokens.shift
    l
  elsif token == ")"
    raise
  else
    token.to_sym
  end
end

def parse_scheme_expression datum
  if datum.is_a? Symbol
    VarExp.new datum
  elsif datum.is_a? Array
    if datum[0] == :lambda # Lambda-exp -- [:lambda, [arg], subexpr]
      arg = datum[1][0]
      body = datum[2]
      LambdaExp.new arg, parse_scheme_expression(body)
    else # App-exp -- [operator, operand]
      AppExp.new(
        parse_scheme_expression(datum[0]),
        parse_scheme_expression(datum[1])
      )
    end
  else
    raise "Invalid syntax for lambda calculus"
  end
end
