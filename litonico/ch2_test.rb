require 'minitest/autorun'
require './ch2.rb'

class TestEnv < Minitest::Test
  def setup
    @env = Env.new [{x: 1, y:2}, {y:3}]
  end

  def test_has_binding
    assert @env.has_binding?(:x)
    refute @env.has_binding?(:z)
  end

  def test_applying_env
    assert_equal 1, @env.apply(:x)
    assert_equal 3, @env.apply(:y)
  end

  def test_extending_env
    @env.extend [:z, :w], [1, 5]
    assert @env.has_binding? :z
    assert @env.has_binding? :w
    assert_equal @env.apply(:z), 1
    assert_equal @env.apply(:w), 5
  end
end

class TestLambda < Minitest::Test
  def test_is_var_exp
    lexp = LambdaExp.new(:y, AppExp.new(VarExp.new(:y), VarExp.new(:x)))
    assert lexp.lambda_exp?
    refute lexp.var_exp?
    refute lexp.app_exp?
    assert VarExp.new(:a).var_exp?
  end

  def test_occurs_free
    # \y.xy
    free_x = LambdaExp.new(:y, AppExp.new(VarExp.new(:x), VarExp.new(:y)))
    # \x.yx
    free_y = LambdaExp.new(:x, AppExp.new(VarExp.new(:y), VarExp.new(:x)))
    assert occurs_free?(:x, free_x)
    refute occurs_free?(:y, free_x)
    assert occurs_free?(:y, free_y)
  end
end

class TestParsing < Minitest::Test
  def test_tokens_to_tree
    tokens = string_to_tokens("()")
    assert_equal [], tokens_to_tree(tokens)
    tokens = string_to_tokens("(lambda)")
    assert_equal [:lambda], tokens_to_tree(tokens) 
    tokens = string_to_tokens("(lambda (x))")
    assert_equal [:lambda, [:x]], tokens_to_tree(tokens) 
    tokens = string_to_tokens("(lambda (x) (x y))")
    assert_equal [:lambda, [:x], [:x, :y]], tokens_to_tree(tokens) 
  end

  def test_parse_expr
    tree = tokens_to_tree(string_to_tokens("(lambda (x) x)")) 
    simple_exp = parse_scheme_expression tree
    assert_equal simple_exp, LambdaExp.new(:x, VarExp.new(:x))
  end
end
