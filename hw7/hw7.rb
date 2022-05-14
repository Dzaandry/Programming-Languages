# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  def eval_prog env
    self
  end
  def preprocess_prog 
    self
  end
  def shift(dx, dy)
    Point.new(x + dx, y + dy)
  end
  def intersect other
    other.intersectPoint self
  end
  def intersectPoint p 
    if real_close_point(x, y, p.x, p.y)
      p
    else NoPoints.new()
    end
  end
  def intersectLine line
    if real_close(y, line.m * x + line.b)
      self
    else
      NoPoints.new()
    end
  end
    def intersectVerticalLine vline
      if real_close(x, vline.x)
        self
      else
        NoPoints.new()
      end
    end
    def intersectWithSegmentAsLineResult seg
      if inbetween(x, seg.x1, seg.x2) && inbetween(y, seg.y1, seg.y2)
        self
      else
        NoPoints.new()
      end 
    end

    private
    def inbetween(v, end1, end2)
      epsilon = GeometryExpression::Epsilon
      (end1 - epsilon <= v && v <= end2 + epsilon) or
        (end2 - epsilon <= v && v <= end2 + epsilon)
    end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end
  def eval_prog env
    self
  end
  def preprocess_prog 
    self
  end
  def shift(dx, dy)
    Line.new(m, b + dy - m * dx)
  end
  def intersect other
    other.intersectLine self
  end
  def intersectPoint p 
    p.intersectLine self
  end
  def intersectLine line
    if real_close(m, line.m)
      if real_close(b, line.b)
        self
      else
        NoPoints.new()
      end
    else
      xcoord = (line.b - b) / (m - line.m)
      ycoord = m*xcoord + b
      Point.new(xcoord, ycoord)
    end
  end
    def intersectVerticalLine vline
      Point.new(vline.x, m*vline.x + b)
    end
    def intersectWithSegmentAsLineResult seg
      seg
    end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def eval_prog env
    self
  end
  def preprocess_prog 
    self
  end
  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end
  def intersect other
    other.intersectVerticalLine self
  end
  def intersectPoint p 
    p.intersectVerticalLine self
  end
  def intersectLine line
    line.intersectVerticalLine self
  end
    def intersectVerticalLine vline
      if real_close(x, vline.x)
        vline
      else
        NoPoints.new()
      end
    end
    def intersectWithSegmentAsLineResult seg
      seg
    end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end
  def eval_prog env
    self
  end
  def preprocess_prog 
    if real_close(x1, x2) && real_close(y1, y2)
      Point.new(x1, y1)
    elsif (real_close(x1, x2) && (y2 < y1)) || (x2 < x1)
      LineSegment.new(x2, y2, x1, y1)
    else
      self
    end
  end
  def shift(dx, dy)
    LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  end
  def intersect other
    other.intersectLineSegment self
  end
  def intersectPoint p 
    p.intersectLineSegment self
  end
  def intersectLine line
    line.intersectLineSegment self
  end
    def intersectVerticalLine vline
      vline.intersectLineSegment self
    end
    def intersectWithSegmentAsLineResult seg
      seg1 = [x1, y1, x2, y2]
      seg2 = [seg.x1, seg.y1, seg.x2, seg.y2]
      if real_close(x1, x2)   # the segments are on a vertical line
        aXstart, aYstart, aXend, aYend, 
        bXstart, bYstart, bXend, bYend = y1 < seg.y1 ? (seg1 + seg2) : (seg2 + seg1) 
        if real_close(aYend, bYstart)
          Point.new(aXend, aYend)
        elsif aYend < bYstart
          NoPoints.new()
        elsif aYend > bYend
          LineSegment.new(bXstart, bYstart, bXend, bYend)
        else
          LineSegment.new(bXstart, bYstart, aXend, aYend)
        end
      else
        aXstart, aYstart, aXend, aYend, 
        bXstart, bYstart, bXend, bYend = x1 < seg.x1 ? seg1 + seg2 : seg2 + seg1
        if real_close(aXend, bXstart)
          Point.new(aXend, aYend)
        elsif aXend < bXstart
          NoPoints.new()
        elsif aXend > bXend
          LineSegment.new(bXstart, bYstart, bXend, bYend)
        else 
          LineSegment.new(bXstart, bYstart, aXend, aYend)
        end
      end
    end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  def eval_prog env
    @e2.eval_prog ([[@s, @e1.eval_prog(env)]] + env)
  end
  def preprocess_prog
    Let.new(@s, (@e1.preprocess_prog), (@e2.preprocess_prog))
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end
  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end


#Constants for testing
ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

#Point Tests
a = Point.new(THREE,FIVE)
if not (a.x == THREE and a.y == FIVE)
	puts "Point is not initialized properly"
else
  puts "Point is initialized well enough"
end
if not (a.eval_prog([]) == a)
	puts "Point eval_prog should return self"
else
  puts "eval_prog correct?"
end
if not (a.preprocess_prog == a)
	puts "Point preprocess_prog should return self"
else
  puts "preprocess_prog correct?"
end
a1 = a.shift(THREE,FIVE)
if not (a1.x == SIX and a1.y == TEN)
	puts "Point shift not working properly"
else 
  puts "shift good"
end
puts a.x
a2 = a.intersect(Point.new(THREE,FIVE))
if not (a2.x == THREE and a2.y == FIVE)
	puts "Point intersect not working properly"
else
  puts "point intersect good"
end 
a3 = a.intersect(Point.new(FOUR,FIVE))
if not (a3.is_a? NoPoints)
	puts "Point intersect not working properly"
else
  puts "point intersect ok"
end

b = Line.new(THREE,FIVE)
if not (b.m == THREE and b.b == FIVE)
	puts "Line not initialized properly"
end
if not (b.eval_prog([]) == b)
	puts "Line eval_prog should return self"
end
if not (b.preprocess_prog == b)
	puts "Line preprocess_prog should return self"
end

b1 = b.shift(THREE,FIVE) 
if not (b1.m == THREE and b1.b == ONE)
	puts "Line shift not working properly"
end

b2 = b.intersect(Line.new(THREE,FIVE))
if not (((b2.is_a? Line)) and b2.m == THREE and b2.b == FIVE)
	puts "Line intersect not working properly"
end
b3 = b.intersect(Line.new(THREE,FOUR))
if not ((b3.is_a? NoPoints))
	puts "Line intersect not working properly"
end

#VerticalLine Tests
c = VerticalLine.new(THREE)
if not (c.x == THREE)
	puts "VerticalLine not initialized properly"
end

if not (c.eval_prog([]) == c)
	puts "VerticalLine eval_prog should return self"
end
if not (c.preprocess_prog == c)
	puts "VerticalLine preprocess_prog should return self"
end
c1 = c.shift(THREE,FIVE)
if not (c1.x == SIX)
	puts "VerticalLine shift not working properly"
end
c2 = c.intersect(VerticalLine.new(THREE))
if not ((c2.is_a? VerticalLine) and c2.x == THREE )
	puts "VerticalLine intersect not working properly"
end
c3 = c.intersect(VerticalLine.new(FOUR))
if not ((c3.is_a? NoPoints))
	puts "VerticalLine intersect not working properly"
end

#LineSegment Tests
d = LineSegment.new(ONE,TWO,-THREE,-FOUR)
if not (d.eval_prog([]) == d)
	puts "LineSegement eval_prog should return self"
end
d1 = LineSegment.new(ONE,TWO,ONE,TWO)
d2 = d1.preprocess_prog
if not ((d2.is_a? Point)and d2.x == ONE and d2.y == TWO) 
	puts "LineSegment preprocess_prog should convert to a Point"
	puts "if ends of segment are real_close"
end

d = d.preprocess_prog
if not (d.x1 == -THREE and d.y1 == -FOUR and d.x2 == ONE and d.y2 == TWO)
	puts "LineSegment preprocess_prog should make x1 and y1"
	puts "on the left of x2 and y2"
end

d3 = d.shift(THREE,FIVE)
if not (d3.x1 == ZERO and d3.y1 == ONE and d3.x2 == FOUR and d3.y2 == SEVEN)
	puts "LineSegment shift not working properly"
end

d4 = d.intersect(LineSegment.new(-THREE,-FOUR,ONE,TWO))
if not (((d4.is_a? LineSegment)) and d4.x1 == -THREE and d4.y1 == -FOUR and d4.x2 == ONE and d4.y2 == TWO)	
	puts "LineSegment intersect not working properly"
end
d5 = d.intersect(LineSegment.new(TWO,THREE,FOUR,FIVE))
if not ((d5.is_a? NoPoints))
	puts "LineSegment intersect not working properly"
end

#Intersect Tests
i = Intersect.new(LineSegment.new(-ONE,-TWO,THREE,FOUR), LineSegment.new(THREE,FOUR,-ONE,-TWO))
i1 = i.preprocess_prog.eval_prog([])
if not (i1.x1 == -ONE and i1.y1 == -TWO and i1.x2 == THREE and i1.y2 == FOUR)
	puts "Intersect eval_prog should return the intersect between e1 and e2"
end

#Var Tests
v = Var.new("a")
v1 = v.eval_prog([["a", Point.new(THREE,FIVE)]])
if not ((v1.is_a? Point) and v1.x == THREE and v1.y == FIVE)
	puts "Var eval_prog is not working properly"
end 
if not (v.preprocess_prog == v)
	puts "Var preprocess_prog should return self"
end

#Let Tests
l = Let.new("a", LineSegment.new(-ONE,-TWO,THREE,FOUR),
             Intersect.new(Var.new("a"),LineSegment.new(THREE,FOUR,-ONE,-TWO)))
l1 = l.preprocess_prog.eval_prog([])
if not (l1.x1 == -ONE and l1.y1 == -TWO and l1.x2 == THREE and l1.y2 == FOUR)
	puts "Let eval_prog should evaluate e2 after adding [s, e1] to the environment"
end

#Let Variable Shadowing Test
l2 = Let.new("a", LineSegment.new(-ONE, -TWO, THREE, FOUR),
              Let.new("b", LineSegment.new(THREE,FOUR,-ONE,-TWO), Intersect.new(Var.new("a"),Var.new("b"))))
l2 = l2.preprocess_prog.eval_prog([["a",Point.new(0,0)]])
if not (l2.x1 == -ONE and l2.y1 == -TWO and l2.x2 == THREE and l2.y2 == FOUR)
	puts "Let eval_prog should evaluate e2 after adding [s, e1] to the environment"
end


#Shift Tests
s = Shift.new(THREE,FIVE,LineSegment.new(-ONE,-TWO,THREE,FOUR))
s1 = s.preprocess_prog.eval_prog([])
if not (s1.x1 == TWO and s1.y1 == THREE and s1.x2 == SIX and s1.y2 == 9)
	puts "Shift should shift e by dx and dy"
end




