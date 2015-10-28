require 'minitest/autorun'
require 'citrus'

module DiffOne
  ZERO = '(diff (one) (one))'
  NEGONE = "(diff #{ZERO} (one))"
  POSTWO = "(diff (one) #{NEGONE})"

  refine String do
    def to_i
      Gramm.parse(self).val
    end

    def zero?
      self.to_i == 0
    end

    def successor
      Gramm.parse(self).add
    end

    def predecessor
      Gramm.parse(self).sub
    end

    def + other
      "(diff #{self} #{Gramm.parse(other).neg})"
    end
  end

  def self.zero
    ZERO
  end

Citrus.eval <<END
grammar Gramm
  rule diff-tree
    (uno | difo)
  end

  rule uno
    '(one)'
    { def val; 1; end
      def add; DiffOne::POSTWO ; end
      def sub; DiffOne::ZERO   ; end
      def neg; DiffOne::NEGONE ; end }
  end

  rule difo
    ('(diff' s op1:diff-tree s op2:diff-tree s ')')
    { def lhs; capture(:op1); end
      def rhs; capture(:op2); end
      def val; lhs.val - rhs.val; end

      def tot a, b
        "(diff " + a + " " + b + ")"
      end

      def add
        rhs == "(one)" ? lhs.to_s : tot(tot(lhs, rhs), DiffOne::NEGONE)
      end

      def sub
        rhs == DiffOne::NEGONE ? lhs.to_s : tot(tot(lhs, rhs), "(one)")
      end

      def neg
        tot(rhs, lhs)
      end
      }
  end

  rule s
    [ \t]*
  end
end
END
end

class DiffOneTest < Minitest::Test
  using DiffOne

  def setup
    @zero = DiffOne.zero
  end

  def test_simplest_one
    assert_equal 1, '(one)'.to_i
  end

  def test_cero
    assert_equal 0, '(diff (one) (one))'.to_i
    assert_equal 0, DiffOne.zero.to_i
  end

  def test_complex_one
    assert_equal 1, '(diff (one) (diff (one) (one)))'.to_i
    assert_equal 2, '(diff (diff (one) (diff (one) (one))) (diff (diff (one) (one)) (diff (one) (diff (one) (one)))))'.to_i
  end

  def test_successors
    assert_equal 1, @zero.successor.to_i
    assert_equal 4, @zero.successor.successor.successor.successor.to_i
  end

  def test_predecessors
    assert_equal -1, @zero.predecessor.to_i
    assert_equal -4, @zero.predecessor.predecessor.predecessor.predecessor.to_i
  end

  def test_is_zero?
    assert @zero.zero?
    assert @zero.successor.predecessor.zero?
  end

  def test_add_trees
    assert_equal  5, ('(diff (one) (diff (one) (one)))' +
                      @zero.successor.successor.successor.successor).to_i
    assert_equal -2, (@zero.predecessor.predecessor.predecessor.predecessor +
                     '(diff (diff (one) (diff (one) (one))) (diff (diff (one) (one)) (diff (one) (diff (one) (one)))))').to_i
  end
end

