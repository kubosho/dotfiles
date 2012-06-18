require 'matrix'

class String
  def to_sexp
    array = self.chomp.split(/([()])| /).delete_if{|c| c.size == 0}
    raise "The expression doesn't close." if array[-1] != ')'
    array.parse_sexp
  end

  def to_s_sexp
    self
  end
end

class Array
  def to_s_sexp
    str=""
    str += '('
    self.each do |s|
      str += ( s.class == Array ? s.to_s_sexp : s.to_s )
      str += ' '
    end
    str += ')'
    str
  end

  def parse_sexp
    q = []
    q << '['
    self.each do |l|
      case l
      when '('
        q << '['
      when ')'
        q << '],'
      else
        q << '"' + l + '",'
      end
    end
    q << ']'
    return eval( q.join )
  end
end

class Vector
  def []=(i,x)
    @elements[i]=x
  end
end

class Color
  attr_accessor :vector

  def initialize(r, g, b)
    @vector = Vector[r, g, b]
  end

  def +(rhs)
    color = clone
    color.vector += rhs.vector
    color.limit
  end

  def *(rhs)
    color = clone
    color.vector *= rhs
    color.limit
  end

  def limit
    color = clone
    color.vector[0] = 255 if (color.vector[0] > 255)
    color.vector[1] = 255 if (color.vector[1] > 255)
    color.vector[2] = 255 if (color.vector[2] > 255)
    color
  end

  def to_html
    "##{format("%02x", @vector[0])}#{format("%02x", @vector[1])}#{format("%02x", @vector[2])}"
  end
end

class EmacsHistoryData
  attr_accessor :use_count, :ranking

  def initialize
    @use_count = 0
    @ranking = 0
  end
end

class EmacsHistory
  def initialize(file_name)
    @hash = Hash.new
    parse(file_name)
    setup_ranking
  end

  def parse(file_name)
    open(file_name).each do |line|
      next if (line =~ /^;/)

      line.chomp.to_sexp[0].each do |name|
        s_exp = name.to_s_sexp
        @hash[s_exp] = EmacsHistoryData.new if !@hash.key? s_exp
        @hash[s_exp].use_count += 1
      end
    end
  end

  def setup_ranking
    @ranking = @hash.to_a.sort {|a, b| (b[1].use_count <=> a[1].use_count).nonzero? or a[0] <=> b[0] }

    current = 1
    prev = @ranking[0]

    @ranking.each do |a|
      current += 1 if (a[1].use_count != prev[1].use_count)
      a[1].ranking = current
      prev = a
    end

    @ranking_total = current
  end

  def count(name)
    @hash.fetch(name) {|| 0 }
  end

  def ranking
    @ranking
  end

  def color(s_exp)
    color1 = Color.new(255, 0, 0)
    color2 = Color.new(255, 255, 0)
    color3 = Color.new(0, 0, 255)

    return color3.to_html if (!@hash.key?(s_exp))

    data = @hash[s_exp]
    
    index = data.ranking - 1
    half = @ranking_total / 2

    if (index < half)
      rate = index.to_f / half
      (color1 * (1.0 - rate) + color2 * (rate)).to_html
    else
      rate = (index.to_f - half) / half
      (color2 * (1.0 - rate) + color3 * (rate)).to_html
    end
  end
end
